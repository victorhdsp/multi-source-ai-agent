import { Command, interrupt, type RetryPolicy } from "@langchain/langgraph";
import type { IGenericRouteUsecase } from "../interfaces/GenericAgent.usecase";
import { ERROR_MESSAGE, HUMAN_REQUEST, HUMAN_RESPONSE } from "@/src/config";
import type { MultiAgentDTO } from "../types";

export class ResearchInterferenceUsecase implements IGenericRouteUsecase {
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
        const prompt = (
            "Você ainda não tem informações suficientes para continuar. \n" +
            "Esta faltando essas informações: \n" +
            `${state.llMOutput.missing.join(", ")}\n` +
            "Posso voltar para a pesquisa? (y/n)"
        )

        const isPermitted = this.talkToHuman(prompt);

        if (isPermitted) {
            return new Command({ goto: "searchNode" }) as any;
        }

        return { ...state, error: ERROR_MESSAGE.NO_PERMISSION_TO_RESEARCH } ;
    }

    errorPolicy: RetryPolicy = {};
}