import type { IVectorStore } from "@/src/infra/interfaces/vector.repository";
import type { LlmAgentResponseDTO } from "../models/llmAgentResponse.dto";
import type { QuestionAgentService } from "./service";
import type { RetryPolicy } from "@langchain/langgraph";
import { ERROR_TYPE } from "@/src/config";
import type { IGenericAgentUsecase } from "../interfaces/GenericAgent.usecase";
import { MULTI_AGENT_SOURCE_VARS, MULTI_AGENT_STEPS, type MultiAgentDTO } from "../strategy";

interface QuestionAgentResponse {
    output: LlmAgentResponseDTO;
    prompt: string;
}

export class QuestionAgentUsecase implements IGenericAgentUsecase {
    constructor(
        private readonly vectorStore: IVectorStore,
        private readonly questionAgentService: QuestionAgentService
    ) {}

    private async execute(input: string): Promise<QuestionAgentResponse> {
        const similarData = await this.vectorStore.similaritySearch(input, 5);
        const context = similarData.map(data => data.pageContent).join('\n');
        const prompt = await this.questionAgentService.formatQuestionPrompt(input, context);
        const rawResponse = await this.questionAgentService.sendToModel(prompt);
        const output = await this.questionAgentService.parseOutput(rawResponse);
        
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