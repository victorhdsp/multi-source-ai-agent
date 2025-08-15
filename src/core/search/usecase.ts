import { Command, END, MemorySaver, START, StateGraph, type RetryPolicy, interrupt } from '@langchain/langgraph';
import type { IGenericAgentUsecase } from "../interfaces/GenericAgent.usecase";
import type { SearchAgentTools } from "./tools/tools";
import { MULTI_AGENT_STEPS } from "../types/steps";
import type { SelfAskWithSearchStrategy } from "./selfAskWithSearch/strategy";
import type { MultiAgentDTO } from "../types/dto";
import { searchAgentState, type SearchAgentState } from "./selfAskWithSearch/types";
import { SEARCH_AGENT_STEPS } from "./selfAskWithSearch/types/steps";
import type { SearchAgentDTO } from "./selfAskWithSearch/types/dto";
import { logger } from "@/src/tools/logger";
import { HUMAN_REQUEST } from '@/src/config';
import { rlPrompt, waitForUserInput } from '@/src/tools/readline';

export class SearchAgentUsecase implements IGenericAgentUsecase{
    public readonly boundCallNode;
    public readonly boundRoute;
    public readonly boundErrorPolicy;

    constructor(
        private readonly strategyService: SelfAskWithSearchStrategy,
        private readonly toolService: SearchAgentTools,
    ){
        this.boundCallNode = this.callNode.bind(this);
        this.boundRoute = this.route.bind(this);
        this.boundErrorPolicy = this.errorPolicy;
    }

    async callNode(state: MultiAgentDTO): Promise<MultiAgentDTO> {
        const workflow = new StateGraph<SearchAgentState>(searchAgentState)
        .addNode("agentNode", this.strategyService.boundCallNode)
        .addNode("decisionNode", s => s)
        .addNode("getPageNode", this.toolService.boundGetPage)
        .addNode("getMusicDbNode", this.toolService.boundFindDBMusic)

        workflow.addEdge(START, "agentNode");
        workflow.addEdge("agentNode", "decisionNode");
        workflow.addConditionalEdges("decisionNode", this.strategyService.boundRoute as any, {
            [SEARCH_AGENT_STEPS.STOP]: END,
            [SEARCH_AGENT_STEPS.ANALYZE]: "agentNode",
            [SEARCH_AGENT_STEPS.GET_PAGE]: "getPageNode",
            [SEARCH_AGENT_STEPS.GET_MUSIC_DB]: "getMusicDbNode",
            [SEARCH_AGENT_STEPS.WHATNOT]: END,
        });
        workflow.addEdge("getPageNode", "decisionNode");
        workflow.addEdge("getMusicDbNode", "decisionNode");
        
        const checkpointer = new MemorySaver();
        const agent = workflow.compile({ checkpointer });
        
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
        
        const configurable = { thread_id: "search" };
        const result = await agent.invoke({
            ...initialState,
        }, { configurable });
        
        const interrupted = (result as any).__interrupt__;
        if (interrupted) {
            if (interrupted[interrupted.length - 1].value.type === HUMAN_REQUEST.PERMISSION) {
                const answer = await waitForUserInput("Deseja continuar? (y/N)");
                await agent.invoke(new Command({ resume: answer }), { configurable });
                rlPrompt();
            }
        }
        
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