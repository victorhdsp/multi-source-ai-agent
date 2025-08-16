import { START, END, StateGraph } from "@langchain/langgraph";
import type { QuestionAgentUsecase } from "@/src/domain/question/usecase";
import type { SearchAgentUsecase } from "@/src/domain/search/usecase";
import { multiAgentState } from "./types/state";
import { MULTI_AGENT_STEPS } from "./types/steps";
import { logger } from "../../tools/logger";

type MultiAgentState = typeof multiAgentState;

export class MultiAgentUseCase {
    constructor(
        private readonly questionUsecase: QuestionAgentUsecase,
        private readonly searchUsecase: SearchAgentUsecase,
    ) {}

    private executeGraph (prompt: string) {
        logger.talk("Pensando...")
        const workflow = new StateGraph<MultiAgentState>(multiAgentState)
            .addNode("questionNode", this.questionUsecase.boundCallNode, {
                retryPolicy: this.questionUsecase.boundErrorPolicy,
            })
            .addNode("searchNode", this.searchUsecase.boundCallNode, {
                retryPolicy: this.searchUsecase.boundErrorPolicy
            })

        workflow.addEdge(START, "questionNode");

        workflow.addConditionalEdges("questionNode", this.questionUsecase.route as any, {
            [MULTI_AGENT_STEPS.ERROR]: END, //"errorNode",
            [MULTI_AGENT_STEPS.SEARCH]: "searchNode", //"researchNode",
            [MULTI_AGENT_STEPS.EXECUTE]: END, //"checkResultNode"
        });
        
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