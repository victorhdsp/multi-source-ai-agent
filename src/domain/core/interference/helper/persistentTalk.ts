import { interrupt } from "@langchain/langgraph";
import type { InterruptDTO, InterruptType } from "../../types/human";
import { ERROR_MESSAGE } from "@/src/config";

export async function persistentTalk(
    prompt: string,
    type: InterruptType,
    validResponses: Set<string>[]
): Promise<string> {
    let message = prompt;
    const rawResponse = interrupt({
        type: type,
        message: message,
    } as InterruptDTO);

    const wrongMessage = ERROR_MESSAGE.WRONG_INPUT(
        Array.from(validResponses).map((response) => {
            const value = response.values().next().value as string;
            return value;
        })
    )

    while (true) {
        const response = rawResponse.toLowerCase().trim();

        if (validResponses.some((set) => set.has(response))) {
            return response;
        }

        message = wrongMessage;
        await new Promise((resolve) => setTimeout(resolve, 100));
    }
}