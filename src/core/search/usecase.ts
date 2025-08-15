import { END, START, StateGraph, type RetryPolicy } from "@langchain/langgraph";
import type { IGenericAgentUsecase } from "../interfaces/GenericAgent.usecase";
import type { SearchAgentTools } from "./tools/tools";
import { MULTI_AGENT_STEPS } from "../types/steps";
import type { SelfAskWithSearchStrategy } from "./selfAskWithSearch/strategy";
import type { MultiAgentDTO } from "../types/dto";
import { searchAgentState, type SearchAgentState } from "./selfAskWithSearch/types";
import { SEARCH_AGENT_STEPS } from "./selfAskWithSearch/types/steps";
import type { SearchAgentDTO } from "./selfAskWithSearch/types/dto";

export class SearchAgentUsecase implements IGenericAgentUsecase{
    constructor(
        private readonly strategyService: SelfAskWithSearchStrategy,
        private readonly toolService: SearchAgentTools,
    ){}

    async callNode(state: MultiAgentDTO): Promise<MultiAgentDTO> {
        const workflow = new StateGraph<SearchAgentState>(searchAgentState)
        .addNode("agentNode", this.strategyService.callNode)
        .addNode("decisionNode", s => s)
        .addNode("getPageNode", this.toolService.getPage)
        .addNode("getMusicDbNode", this.toolService.getMusicDB);
        
        workflow.addEdge(START, "agentNode");
        workflow.addEdge("agentNode", "decisionNode");
        workflow.addConditionalEdges("decisionNode", this.strategyService.route as any, {
            [SEARCH_AGENT_STEPS.STOP]: END,
            [SEARCH_AGENT_STEPS.ANALYZE]: "agentNode",
            [SEARCH_AGENT_STEPS.GET_PAGE]: "getPageNode",
            [SEARCH_AGENT_STEPS.GET_MUSIC_DB]: "getMusicDbNode",
        });
        
        const agent = workflow.compile();
        
        const initialState: SearchAgentDTO = {
            userInput: state.input,
            llMOutput: {
                ...state.llMOutput,
                step: "ANALYZE"
            },
            searchedSources: state.searchedSources,
            history: [],
            permissions: new Set(),
            numberOfSteps: 0,
        }
        
        const result = await agent.invoke({ ...initialState });
        
        return {
            input: result.userInput,
            llMOutput: {
                content: result.llMOutput.content,
                missing: result.llMOutput.missing,
                type: state.llMOutput.type,
            },
            searchedSources: result.searchedSources,
            error: result.error,
        }
    }

    errorPolicy: RetryPolicy = {}

    async route(state: MultiAgentDTO): Promise<string> {
        if (state.error)
            return MULTI_AGENT_STEPS.ERROR;

        if (state.llMOutput.missing.length > 0)
            return MULTI_AGENT_STEPS.SEARCH;

        return MULTI_AGENT_STEPS.EXECUTE;
    }
}