import type { RetryPolicy } from "@langchain/langgraph";
import type { SearchAgentStateDTO } from "../models/searchAgentRequest.dto";

export interface IGenericAgentUsecase {
    callNode(state: SearchAgentStateDTO): Promise<SearchAgentStateDTO>;
    errorPolicy: RetryPolicy;
}