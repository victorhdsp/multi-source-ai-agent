import { Command, END, interrupt, type RetryPolicy } from "@langchain/langgraph";
import type { IGenericRouteUsecase } from "../interfaces/GenericAgent.usecase";
import { ERROR_MESSAGE, HUMAN_REQUEST, HUMAN_RESPONSE } from "@/src/config";
import type { MultiAgentDTO } from "../types/dto";

export class CheckResultInterferenceUsecase implements IGenericRouteUsecase {
    private talkToHuman(prompt: string): boolean {
        const rawResponse = interrupt({
            type: HUMAN_REQUEST.PERMISSION,
            question: prompt
        });
        
        const response = rawResponse.toLowerCase().trim();

        if (HUMAN_RESPONSE.TRUE.has(response))
            return true;
        if (HUMAN_RESPONSE.FALSE.has(response))
            return false;
        
        return this.talkToHuman(ERROR_MESSAGE.WRONG_INPUT(["s", "n"]));
    }

    async callNode(state: MultiAgentDTO): Promise<MultiAgentDTO> {
        if (state.llMOutput.type === "QUERY")
            return new Command({ goto: END }) as any;

        const prompt = (
            "Montei um plano de ação para concluir a tarefa que foi pedida. \n" +
            "``` Plano de ação\n" +
            state.llMOutput.content + "\n" +
            "```" +
            "Você aprova a execução desse plano? (y/n)"
        )

        const permission = this.talkToHuman(prompt);

        if (permission) {
            return new Command({ goto: "executeNode" }) as any;
        }

        return { ...state, error: ERROR_MESSAGE.NO_PERMISSION_TO_EXECUTE };
    }

    errorPolicy: RetryPolicy = {};
}