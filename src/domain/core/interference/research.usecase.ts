import { Command, type RetryPolicy } from "@langchain/langgraph";
import { ERROR_MESSAGE } from "@/src/config";
import type { MultiAgentDTO } from "@/src/domain/core/types/dto";
import { persistentTalk } from "./helper/persistentTalk";
import { HUMAN_RESPONSE, INTERRUPT_TYPES, type InterruptType } from "../types/human";

export class ResearchInterferenceUsecase {
    async callNode(state: MultiAgentDTO): Promise<MultiAgentDTO> {
        const prompt = (
            "Você ainda não tem informações suficientes para continuar. \n" +
            "Esta faltando essas informações: \n" +
            `${state.llMOutput.missing.join(", ")}\n` +
            "Posso voltar para a pesquisa? (y/n)"
        )

        const permission = await persistentTalk(
            prompt,
            INTERRUPT_TYPES.PERMISSION as InterruptType,
            [HUMAN_RESPONSE.TRUE, HUMAN_RESPONSE.FALSE]
        );

        if (HUMAN_RESPONSE.TRUE.has(permission)) {
            return new Command({ goto: "searchNode" }) as any;
        }

        return { ...state, error: ERROR_MESSAGE.NO_PERMISSION_TO_RESEARCH } ;
    }

    errorPolicy: RetryPolicy = {};
}