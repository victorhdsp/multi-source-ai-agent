import type { CallbackManagerForLLMRun } from "@langchain/core/callbacks/manager";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
import type { BaseMessage } from "@langchain/core/messages";
import type { ChatResult } from "@langchain/core/outputs";
import { FakeListChatModel } from "langchain/embeddings/fake";

export class mockAgentLLMService extends BaseChatModel {
    private primaryLLM: BaseChatModel;

    constructor(
        readonly response: string
    ) {
        const baseParams = {}
        super(baseParams);

        this.primaryLLM = new FakeListChatModel({
            responses: [response],
        });
    }

    _llmType() {
        return "agent-llm-service";
    }

    async _generate(
        messages: BaseMessage[],
        options: this["ParsedCallOptions"],
        runManager?: CallbackManagerForLLMRun,
        tryCount: number = 0
    ): Promise<ChatResult> {
        return await this.primaryLLM._generate(messages, options, runManager);
    }
}