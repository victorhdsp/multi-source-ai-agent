import type { RetryPolicy } from '@langchain/langgraph';
import { MULTI_AGENT_STEPS } from "@/src/domain/core/types/steps";
import type { MultiAgentDTO } from "@/src/domain/core/types/dto";
import type { SearchAgentDTO } from "@/src/domain/search/selfAskWithSearch/types/dto";
import type { SearchAgentWorkflowManager } from './workflow/manager';

export class SearchAgentUsecase {
    public readonly boundCallNode;
    public readonly boundRoute;
    public readonly boundErrorPolicy;

    constructor(
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