import { Command, END, MemorySaver, START, StateGraph, type RetryPolicy } from '@langchain/langgraph';
import type { SearchAgentTools } from "@/src/domain/search/tools/tools";
import { MULTI_AGENT_STEPS } from "@/src/domain/core/types/steps";
import type { SelfAskWithSearchStrategy } from "@/src/domain/search/selfAskWithSearch/strategy";
import type { MultiAgentDTO } from "@/src/domain/core/types/dto";
import { searchAgentState, type SearchAgentState } from "@/src/domain/search/selfAskWithSearch/types";
import { SEARCH_AGENT_STEPS } from "@/src/domain/search/selfAskWithSearch/types/steps";
import type { SearchAgentDTO } from "@/src/domain/search/selfAskWithSearch/types/dto";
import { rlPrompt, waitForUserInput } from '@/src/tools/readline';
import { INTERRUPT_TYPES } from '../core/types/human';
import type { SearchAgentWorkflowManager } from './workflow/manager';

export class SearchAgentUsecase {
    public readonly boundCallNode;
    public readonly boundRoute;
    public readonly boundErrorPolicy;

    constructor(
        private readonly strategyService: SelfAskWithSearchStrategy,
        private readonly toolService: SearchAgentTools,
        private readonly manager: SearchAgentWorkflowManager,
    ){
        this.boundCallNode = this.callNode.bind(this);
        this.boundRoute = this.route.bind(this);
        this.boundErrorPolicy = this.errorPolicy;
    }

    async callNode(state: MultiAgentDTO): Promise<MultiAgentDTO> { 
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

        const result = await this.manager.runWorkflow(initialState);

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