import type { IVectorStore } from "@/src/infra/interfaces/vector.repository";
import type { QuestionAgentStrategy } from "./strategy";
import type { RetryPolicy } from "@langchain/langgraph";
import { ERROR_TYPE } from "@/src/config";
import type { IGenericAgentUsecase } from "../interfaces/GenericAgent.usecase";
import type { QuestionAgentDTO } from "../models/llmAgentResponse.dto";
import type { MultiAgentDTO } from "../types";
import { MULTI_AGENT_SOURCE_VARS } from "../types/source";
import { MULTI_AGENT_STEPS } from "../types/steps";

interface QuestionAgentResponse {
    output: QuestionAgentDTO;
    prompt: string;
}

export class QuestionAgentUsecase implements IGenericAgentUsecase {
    constructor(
        private readonly vectorStore: IVectorStore,
        private readonly questionAgentStrategy: QuestionAgentStrategy
    ) {}

    private async execute(input: string): Promise<QuestionAgentResponse> {
        const similarData = await this.vectorStore.similaritySearch(input, 5);
        const context = similarData.map(data => data.pageContent).join('\n');
        const prompt = await this.questionAgentStrategy.formatQuestionPrompt(input, context);
        const rawResponse = await this.questionAgentStrategy.sendToModel(prompt);
        const output = await this.questionAgentStrategy.parseOutput(rawResponse);
        
        return {
            output,
            prompt: prompt.promptMessages.map(m => m.toString()).join("\n"),
        };
    }

    async callNode(state: MultiAgentDTO): Promise<MultiAgentDTO> {
        try {
            const { output, prompt } = await this.execute(state.input);

            return {
                ...state,
                input: prompt,
                llMOutput: output,
                searchedSources: [MULTI_AGENT_SOURCE_VARS.DOCUMENT]
            };
        } catch (error: any) {
            return {
                ...state,
                error: error.message,
            };
        }
    }

    public errorPolicy: RetryPolicy = {
        maxAttempts: 3,
        retryOn: (error: Error) => {
            if (error.cause == ERROR_TYPE.PARSER)
                return true;
            return false;
        }
    }

    async route(state: MultiAgentDTO): Promise<string> {
        if (state.error) {
            return MULTI_AGENT_STEPS.ERROR;
        }

        if (state.llMOutput.missing.length > 0) {
            return MULTI_AGENT_STEPS.SEARCH;
        }

        return MULTI_AGENT_STEPS.EXECUTE;
    }
}