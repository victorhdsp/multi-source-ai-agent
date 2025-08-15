import type { RetryPolicy } from "@langchain/langgraph";
import type { SearchAgentStateDTO } from "../search/selfAskWithSearch/types";
import type { NameNodes } from "../usecase";

export interface IGenericAgentUsecase {
    callNode(state: SearchAgentStateDTO): Promise<SearchAgentStateDTO>;
    errorPolicy: RetryPolicy;
    route?(state: SearchAgentStateDTO): Promise<NameNodes>;
}

export interface IGenericRouteUsecase {
    callNode(state: SearchAgentStateDTO): Promise<SearchAgentStateDTO>;
    errorPolicy: RetryPolicy;
}