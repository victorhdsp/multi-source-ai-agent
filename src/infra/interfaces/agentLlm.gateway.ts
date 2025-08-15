import type { CallbackManagerForLLMRun } from "@langchain/core/callbacks/manager";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
import type { BaseMessage } from "@langchain/core/messages";
import type { ChatResult } from "@langchain/core/outputs";

export interface IAgentLLMService extends BaseChatModel {
    _llmType(): string;
    _generate(
        messages: BaseMessage[],
        options: BaseChatModel["ParsedCallOptions"],
        runManager?: CallbackManagerForLLMRun,
        tryCount?: number
    ): Promise<ChatResult>;
}
