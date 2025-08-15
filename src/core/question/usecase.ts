import type { IVectorStore } from "@/src/infra/interfaces/vector.repository";
import type { QuestionAgentStrategy } from "./strategy";
import type { RetryPolicy } from "@langchain/langgraph";
import { ERROR_TYPE } from "@/src/config";
import type { IGenericAgentUsecase } from "../interfaces/GenericAgent.usecase";
import type { QuestionAgentDTO } from "./questionAgent.dto";
import { MULTI_AGENT_SOURCE_VARS } from "../types/source";
import { MULTI_AGENT_STEPS } from "../types/steps";
import type { MultiAgentDTO } from "../types/dto";
import { logger } from "@/src/tools/logger";

export class QuestionAgentUsecase implements IGenericAgentUsecase {
    public readonly boundCallNode;
    public readonly boundRoute;
    public readonly boundErrorPolicy;

    constructor(
        private readonly vectorStore: IVectorStore,
        private readonly questionAgentStrategy: QuestionAgentStrategy
    ) {
        this.boundCallNode = this.callNode.bind(this);
        this.boundRoute = this.route.bind(this);
        this.boundErrorPolicy = this.errorPolicy;
    }

    private async execute(input: string): Promise<QuestionAgentDTO> {
        const similarData = await this.vectorStore.similaritySearch(input, 5);
        const context = similarData.map(data => data.pageContent).join('\n');
        const prompt = await this.questionAgentStrategy.formatQuestionPrompt(input, context);
        const rawResponse = await this.questionAgentStrategy.sendToModel(prompt);
        const output = await this.questionAgentStrategy.parseOutput(rawResponse);

        return output;
    }

    async callNode(state: MultiAgentDTO): Promise<MultiAgentDTO> {
        try {
            const output = await this.execute(state.input);
            logger.thinking(output.content);
            return {
                ...state,
                llMOutput: output,
                searchedSources: [MULTI_AGENT_SOURCE_VARS.DOCUMENT]
            };
        } catch (error: any) {
            throw Error(error.message || "An error occurred while processing the question.");
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
            logger.thinking(state.error);
            return MULTI_AGENT_STEPS.ERROR;
        }

        if (state.llMOutput.missing.length > 0) {
            logger.thinking("Faltando informação, buscando...");
            return MULTI_AGENT_STEPS.SEARCH;
        }

        logger.thinking("Pergunta respondida, executando...");
        return MULTI_AGENT_STEPS.EXECUTE;
    }
}