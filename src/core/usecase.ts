import { START, END, StateGraph } from "@langchain/langgraph";
import type { QuestionAgentUsecase } from "./question/usecase";
import { searchAgentState, type SearchAgentState } from "./search/strategy/strategy";
import type { SearchAgentUsecase } from "./search/usecase";
import type { ExecuteAgentUsecase } from "./execute/usecase";
import type { ResearchInterferenceUsecase } from "./interference/research.usecase";
import type { CheckResultUsecase } from "./interference/checkResult.usecase";
import { MULTI_AGENT_STEPS, multiAgentState } from "./strategy";

type MultiAgentState = typeof multiAgentState;

export class MultiAgentUseCase {
    constructor(
        private readonly questionUsecase: QuestionAgentUsecase,
        private readonly searchUsecase: SearchAgentUsecase,
        private readonly executeUsecase: ExecuteAgentUsecase,
        private readonly researchUsecase: ResearchInterferenceUsecase,
        private readonly checkResultUsecase: CheckResultUsecase
    ) {}

    private executeGraph (prompt: string) {
        const graph = new StateGraph<MultiAgentState>(multiAgentState)
            .addNode("questionNode", this.questionUsecase.callNode, {
                retryPolicy: this.questionUsecase.errorPolicy,
            })
            .addNode("searchNode", this.searchUsecase.callNode, {
                retryPolicy: this.searchUsecase.errorPolicy
            })
            .addNode("researchNode", this.researchUsecase.callNode, {
                retryPolicy: this.researchUsecase.errorPolicy
            })
            .addNode("checkResultNode", this.checkResultUsecase.callNode, {
                retryPolicy: this.checkResultUsecase.errorPolicy
            })
            .addNode("executeNode", this.executeUsecase.callNode, {
                retryPolicy: this.executeUsecase.errorPolicy
            })
            .addNode("errorNode", async (state) => {
                return state;
            });

        graph.addEdge(START, "questionNode");

        graph.addConditionalEdges("questionNode", this.questionUsecase.route as any, {
            [MULTI_AGENT_STEPS.ERROR]: "errorNode",
            [MULTI_AGENT_STEPS.SEARCH]: "researchNode",
            [MULTI_AGENT_STEPS.EXECUTE]: "checkResultNode"
        });

        graph.addConditionalEdges("searchNode", this.searchUsecase.route as any, {
            [MULTI_AGENT_STEPS.ERROR]: "errorNode",
            [MULTI_AGENT_STEPS.SEARCH]: "researchNode",
            [MULTI_AGENT_STEPS.EXECUTE]: "checkResultNode"
        });
        
        graph.addEdge("researchNode", "errorNode");
        graph.addEdge("checkResultNode", "errorNode");

        // DEFAULT
        graph.addEdge("executeNode", END);
        graph.addEdge("errorNode", END);

        return graph;
    }

    async execute(prompt: string): Promise<string> {
        const graph = this.executeGraph(prompt);
        return "executando";
    }
}