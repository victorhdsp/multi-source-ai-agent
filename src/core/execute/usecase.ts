import type { RetryPolicy } from "@langchain/langgraph";
import type { IGenericAgentUsecase } from "../interfaces/GenericAgent.usecase";
import type { SearchAgentStateDTO } from "../search/strategy/strategy";
import type { NameNodes } from "../usecase";

export class ExecuteAgentUsecase implements IGenericAgentUsecase {
    async callNode(state: SearchAgentStateDTO): Promise<SearchAgentStateDTO> {
        return ;
    }

    errorPolicy: RetryPolicy = {};
}