import { StateGraph } from "@langchain/langgraph";
import type { QuestionAgentUsecase } from "./question/usecase";
import { searchAgentState } from "./models/searchAgentRequest.dto";
import type z from "zod";
import type { SearchAgentUsecase } from "./search/usecase";
import type { ExecuteAgentUsecase } from "./execute/usecase";

type SearchAgentState = z.infer<typeof searchAgentState>;

export class CallToMultiAgentUseCase {
    constructor(
        private readonly questionUsecase: QuestionAgentUsecase,
        private readonly searchUsecase: SearchAgentUsecase,
        private readonly executeUsecase: ExecuteAgentUsecase
    ) {}

    private executeGraph (prompt: string): StateGraph<SearchAgentState> {
        const graph = new StateGraph(searchAgentState);

        graph.addNode("questionNode", this.questionUsecase.callNode, {
            retryPolicy: this.questionUsecase.errorPolicy
        });
        graph.addNode("searchNode", this.searchUsecase.callNode, {
            retryPolicy: this.searchUsecase.errorPolicy
        });
        graph.addNode("executeNode", this.executeUsecase.callNode, {
            retryPolicy: this.executeUsecase.errorPolicy
        });

        return graph;
    }

    async execute(prompt: string): Promise<string> {
        const graph = this.executeGraph(prompt);
        return "executando";
    }
}