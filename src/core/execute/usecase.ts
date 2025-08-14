import type { RetryPolicy } from "@langchain/langgraph";
import type { IGenericAgentUsecase } from "../interfaces/GenericAgent.usecase";
import type { SearchAgentStateDTO } from "../models/searchAgentRequest.dto";

export class ExecuteAgentUsecase implements IGenericAgentUsecase {
    callNode(state: SearchAgentStateDTO): Promise<SearchAgentStateDTO> {
        return Promise.resolve(state);
    }

    errorPolicy: RetryPolicy = {};
}