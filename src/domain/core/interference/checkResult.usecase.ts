
import { Command, END, type RetryPolicy } from "@langchain/langgraph";
import { ERROR_MESSAGE} from "@/src/config";
import type { MultiAgentDTO } from "@/src/domain/core/types/dto";
import { HUMAN_RESPONSE, INTERRUPT_TYPES, type InterruptType } from "../types/human";
import { persistentTalk } from "./helper/persistentTalk";

export class CheckResultInterferenceUsecase {
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

        const permission = persistentTalk(
            prompt,
            INTERRUPT_TYPES.PERMISSION as InterruptType,
            [HUMAN_RESPONSE.TRUE, HUMAN_RESPONSE.FALSE]
        );

        if (HUMAN_RESPONSE.TRUE.has(permission)) {
            return new Command({ goto: "executeNode" }) as any;
        }

        return { ...state, error: ERROR_MESSAGE.NO_PERMISSION_TO_EXECUTE };
    }

    errorPolicy: RetryPolicy = {};
}