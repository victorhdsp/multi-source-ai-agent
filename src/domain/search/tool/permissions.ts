import { ERROR_MESSAGE } from "@/src/config";
import { persistentTalk } from "../../core/interference/helper/persistentTalk";
import { HUMAN_RESPONSE, INTERRUPT_TYPES } from "../../core/types/human";
import type { SearchAgentDTO } from "../selfAskWithSearch/types/dto";

export class PermissionManager {
    async requestInternetPermission(state: SearchAgentDTO): Promise<boolean> {
        if (!state.permissions.has("INTERNET")) {
            const prompt = (
                "Para conseguir continuar preciso fazer a busca em uma pagina da internet: \n" +
                `${state.llMOutput.content}\n` +
                "Tenho permissão para fazer isso? (y/n)"
            )

            const permission = await persistentTalk(
                prompt,
                INTERRUPT_TYPES.PERMISSION,
                [HUMAN_RESPONSE.TRUE, HUMAN_RESPONSE.FALSE]
            );
            if (!HUMAN_RESPONSE.TRUE.has(permission)) {
                return false
            }
        }

        return true
    }
}