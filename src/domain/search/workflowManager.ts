import { Command, END, MemorySaver, START, StateGraph, interrupt, type PregelOptions } from '@langchain/langgraph';
import { searchAgentState, type SearchAgentState } from "./selfAskWithSearch/types";
import { SEARCH_AGENT_STEPS } from "./selfAskWithSearch/types/steps";
import type { SelfAskWithSearchStrategy } from "./selfAskWithSearch/strategy";
import type { SearchAgentDTO } from "./selfAskWithSearch/types/dto";
import { INTERRUPT_TYPES, type InterruptDTO } from '../core/interference/type';
import { rlPrompt, waitForUserInput } from '@/src/tools/readline';
import { logger } from '@/src/tools/logger';
import type { ToolBoxService } from './tool/service';

export class SearchAgentWorkflowManager {
    public readonly agent;
    private configurable = { thread_id: "search" };

    constructor(
        private readonly strategyService: SelfAskWithSearchStrategy,
        private readonly toolBox: ToolBoxService,
    ) {
        this.agent = this.instance();
    }

    private instance() {
        const workflow = new StateGraph<SearchAgentState>(searchAgentState)
        .addNode("agentNode", this.strategyService.boundCallNode)
        .addNode("decisionNode", s => s)
        .addNode("useCurlNode", state => this.toolBox.useTool(state as SearchAgentDTO, "useCurl"))
        .addNode("useSQLiteNode", state => this.toolBox.useTool(state as SearchAgentDTO, "useSQLite"))
        .addNode("useQuestionNode", state => this.toolBox.useTool(state as SearchAgentDTO, "useQuestion"));

        workflow.addEdge(START, "agentNode");
        workflow.addEdge("agentNode", "decisionNode");
        workflow.addConditionalEdges("decisionNode", this.strategyService.boundRoute as any, {
            [SEARCH_AGENT_STEPS.STOP]: END,
            [SEARCH_AGENT_STEPS.ANALYZE]: "agentNode",
            [SEARCH_AGENT_STEPS.USE_CURL]: "useCurlNode",
            [SEARCH_AGENT_STEPS.USE_SQL]: "useSQLiteNode",
            [SEARCH_AGENT_STEPS.USE_QUESTION]: "useQuestionNode",
            [SEARCH_AGENT_STEPS.WHATNOT]: END,
        });
        workflow.addEdge("useCurlNode", "decisionNode");
        workflow.addEdge("useSQLiteNode", "decisionNode");
        workflow.addEdge("useQuestionNode", "decisionNode");
        
        const checkpointer = new MemorySaver();
        return workflow.compile({ checkpointer });
    }

    private async interruptWorkflow(interrupted: { value: InterruptDTO}[]) {
        if (interrupted && interrupted.length > 0) {
            const last = interrupted[interrupted.length - 1];
            
            if (last && last.value.type) {
                const answer = await waitForUserInput(last.value.message);
                const newStream = await this.agent.stream(
                    new Command({ resume: answer }), { configurable: this.configurable }
                );
                rlPrompt();
                
                for await (const stream of newStream) {
                    await this.interruptWorkflow((stream as any).__interrupt__ || []);
                }
            }
        }
    }

    public async runWorkflow(initialState: SearchAgentDTO): Promise<SearchAgentDTO> {
        const streams = await this.agent.stream(initialState, { configurable: this.configurable });

        for await (const stream of streams) {
            await this.interruptWorkflow((stream as any).__interrupt__);
        }
        
        const streamState = await this.agent.getState({ configurable: this.configurable });
        return streamState.values as SearchAgentDTO;
    }
}