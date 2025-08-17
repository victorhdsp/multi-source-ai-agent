import type { IVectorStore } from "@/src/infra/interfaces/vector.repository";
import type { QuestionAgentStrategy } from "@/src/domain/question/strategy";
import type { RetryPolicy } from "@langchain/langgraph";
import { ERROR_MESSAGE, ERROR_TYPE } from "@/src/config";
import type { QuestionAgentDTO } from "@/src/domain/question/models";
import { MULTI_AGENT_SOURCE_VARS } from "@/src/domain/core/types/source";
import { MULTI_AGENT_STEPS } from "@/src/domain/core/types/steps";
import type { MultiAgentDTO } from "@/src/domain/core/types/dto";
import { logger } from "@/src/tools/logger";
import { secureExec, secureExecAsync } from "@/src/utils/secureExec";

export class QuestionAgentUsecase {
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
        const context = await secureExecAsync(async () => {
            const similarData = await this.vectorStore.similaritySearch(input, 3);
            return similarData.map(data => data.pageContent).join('\n');
        }, ERROR_MESSAGE.FAIL_IN_RAG);

        const prompt = await secureExecAsync(() => (
            this.questionAgentStrategy.formatQuestionPrompt(input, context)
        ), ERROR_MESSAGE.FAIL_TO_CREATE_PROMPT);
        
        const rawResponse = await secureExecAsync(() => (
            this.questionAgentStrategy.sendToModel(prompt)
        ), ERROR_MESSAGE.NO_OUTPUT);

        return await secureExecAsync(() => (
            this.questionAgentStrategy.parseOutput(rawResponse)
        ), ERROR_MESSAGE.FAIL_TO_PARSE);
    }

    async callNode(state: MultiAgentDTO): Promise<MultiAgentDTO> {
        try {
            const output = await this.execute(state.input);
            const newState: MultiAgentDTO = {
                ...state,
                llMOutput: output,
                searchedSources: [MULTI_AGENT_SOURCE_VARS.DOCUMENT]
            }

            logger.thinking(output.content);
            logger.state(newState);

            return newState;
        } catch (err) {
            const error = err as Error;
            throw new Error(`Erro ao chamar o nó <QuestionAgent> {${error.message}}`);
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