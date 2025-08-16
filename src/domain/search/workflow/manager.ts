import { Command, END, MemorySaver, START, StateGraph, interrupt, type PregelOptions } from '@langchain/langgraph';
import { searchAgentState, type SearchAgentState } from "../selfAskWithSearch/types";
import { SEARCH_AGENT_STEPS } from "../selfAskWithSearch/types/steps";
import type { SelfAskWithSearchStrategy } from "../selfAskWithSearch/strategy";
import type { SearchAgentTools } from "../tools/tools";
import type { SearchAgentDTO } from "../selfAskWithSearch/types/dto";
import { INTERRUPT_TYPES, type InterruptDTO } from '../../core/types/human';
import { rlPrompt, waitForUserInput } from '@/src/tools/readline';
import { logger } from '@/src/tools/logger';

export class SearchAgentWorkflowManager {
    public readonly agent;
    private configurable = { thread_id: "search" };

    constructor(
        private readonly strategyService: SelfAskWithSearchStrategy,
        private readonly toolService: SearchAgentTools,
    ) {
        this.agent = this.instance();
    }

    private instance() {
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
        return workflow.compile({ checkpointer });
    }

    private async interruptWorkflow(interrupted: { value: InterruptDTO}[]) {
        if (interrupted && interrupted.length > 0) {
            const last = interrupted[interrupted.length - 1];
            
            if (last && last.value.type === INTERRUPT_TYPES.PERMISSION) {
                const answer = await waitForUserInput(last.value.message);
                await this.agent.invoke(
                    new Command({ resume: answer }), { configurable: this.configurable });
                rlPrompt();
            }
        }
    }

    public async runWorkflow(initialState: SearchAgentDTO): Promise<SearchAgentDTO> {
        const streams = await this.agent.stream(initialState, { configurable: this.configurable });

        for await (const stream of streams) {
            await this.interruptWorkflow((stream as any).__interrupt__);
            logger.state(stream);
        }
        
        const streamState = await this.agent.getState({ configurable: this.configurable });
        return streamState.values as SearchAgentDTO;
    }
}