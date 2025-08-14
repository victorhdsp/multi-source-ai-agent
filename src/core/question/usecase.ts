import type { IVectorStore } from "@/src/infra/interfaces/vector.repository";
import type { LlmAgentResponseDTO } from "../models/llmAgentResponse.dto";
import type { QuestionAgentService } from "./agent.service";
import type { SearchAgentStateDTO } from "../models/searchAgentRequest.dto";
import type { RetryPolicy } from "@langchain/langgraph";
import { ERROR_TYPE } from "@/src/config";
import type { IGenericAgentUsecase } from "../interfaces/GenericAgent.usecase";

export class QuestionAgentUsecase implements IGenericAgentUsecase {
    constructor(
        private readonly vectorStore: IVectorStore,
        private readonly questionAgentService: QuestionAgentService
    ) {}

    private async execute(input: string): Promise<LlmAgentResponseDTO> {
        const similarData = await this.vectorStore.similaritySearch(input, 5);
        const context = similarData.map(data => data.pageContent).join('\n');
        return this.questionAgentService.execute(input, context);
    }

    async callNode(state: SearchAgentStateDTO): Promise<SearchAgentStateDTO> {
        const response = await this.execute(state.input);
        
        return {
            ...state,
            output: response
        };
    }

    public errorPolicy: RetryPolicy = {
        maxAttempts: 3,
        retryOn: (error: Error) => {
            if (error.cause == ERROR_TYPE.PARSER)
                return true;
            return false;
        }
    }
}