import { START, END, StateGraph } from "@langchain/langgraph";
import type { QuestionAgentUsecase } from "./question/usecase";
import type { SearchAgentUsecase } from "./search/usecase";
import type { ExecuteAgentUsecase } from "./execute/usecase";
import type { ResearchInterferenceUsecase } from "./interference/research.usecase";
import { multiAgentState } from "./types/state";
import { MULTI_AGENT_STEPS } from "./types/steps";
import type { CheckResultInterferenceUsecase } from "./interference/checkResult.usecase";

type MultiAgentState = typeof multiAgentState;

export class MultiAgentUseCase {
    constructor(
        private readonly questionUsecase: QuestionAgentUsecase,
        private readonly searchUsecase: SearchAgentUsecase,
        private readonly executeUsecase: ExecuteAgentUsecase,
        private readonly researchUsecase: ResearchInterferenceUsecase,
        private readonly checkResultUsecase: CheckResultInterferenceUsecase
    ) {}

    private executeGraph (prompt: string) {
        const workflow = new StateGraph<MultiAgentState>(multiAgentState)
            .addNode("questionNode", this.questionUsecase.boundCallNode, {
                retryPolicy: this.questionUsecase.boundErrorPolicy,
            })
            .addNode("searchNode", this.searchUsecase.boundCallNode, {
                retryPolicy: this.searchUsecase.boundErrorPolicy
            })
            // .addNode("researchNode", this.researchUsecase.callNode, {
            //     retryPolicy: this.researchUsecase.errorPolicy
            // })
            // .addNode("checkResultNode", this.checkResultUsecase.callNode, {
            //     retryPolicy: this.checkResultUsecase.errorPolicy
            // })
            // .addNode("executeNode", this.executeUsecase.callNode, {
            //     retryPolicy: this.executeUsecase.errorPolicy
            // })
            // .addNode("errorNode", async (state) => {
            //     return state;
            // });

        workflow.addEdge(START, "questionNode");
        workflow.addEdge("questionNode", "searchNode");

        // workflow.addConditionalEdges("questionNode", this.questionUsecase.route as any, {
        //     [MULTI_AGENT_STEPS.ERROR]: "errorNode",
        //     [MULTI_AGENT_STEPS.SEARCH]: "researchNode",
        //     [MULTI_AGENT_STEPS.EXECUTE]: "checkResultNode"
        // });

        // workflow.addConditionalEdges("searchNode", this.searchUsecase.route as any, {
        //     [MULTI_AGENT_STEPS.ERROR]: "errorNode",
        //     [MULTI_AGENT_STEPS.SEARCH]: "researchNode",
        //     [MULTI_AGENT_STEPS.EXECUTE]: "checkResultNode"
        // });
        
        // workflow.addEdge("researchNode", "errorNode");
        // workflow.addEdge("checkResultNode", "errorNode");

        // DEFAULT
        // workflow.addEdge("executeNode", END);
        // workflow.addEdge("errorNode", END);

        return workflow;
    }

    async execute(prompt: string): Promise<string> {
        const workflow = this.executeGraph(prompt);
        const agent = workflow.compile();
        const result = await agent.invoke({ 
            input: prompt
         });

        if (result.llMOutput) {
            return result.llMOutput.content;
        }

        return "No output from the agent.";
    }
}