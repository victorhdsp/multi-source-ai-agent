import { ERROR_TYPE } from "@/src/config";
import { logger } from "@/src/tools/logger";
import type { CallbackManagerForLLMRun } from "@langchain/core/callbacks/manager";
import { BaseChatModel } from "@langchain/core/language_models/chat_models";
import type { BaseMessage } from "@langchain/core/messages";
import type { ChatResult } from "@langchain/core/outputs";
import { ChatGoogleGenerativeAI } from "@langchain/google-genai";
import dotenv from 'dotenv';

dotenv.config();

export class AgentLLMService extends BaseChatModel {
    private primaryLLM: BaseChatModel;
    private secondaryLLM: BaseChatModel;

    constructor() {
        const baseParams = {}
        super(baseParams);

        this.primaryLLM = new ChatGoogleGenerativeAI({
            model: process.env.PRIMARY_MODEL || "gemini-2.0-flash-lite",
            apiKey: process.env.GOOGLE_API_KEY,
            temperature: 0
        });

        this.secondaryLLM = new ChatGoogleGenerativeAI({
            model: process.env.SECONDARY_MODEL || "gemini-2.5-flash",
            apiKey: process.env.GOOGLE_API_KEY,
            temperature: 0
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
        try {
            if (tryCount > 0) {
                return await this.secondaryLLM._generate(messages, options, runManager);
            }
            return await this.primaryLLM._generate(messages, options, runManager);

        } catch (err) {
            if (tryCount < 2) {
                logger.warn(`Tentativa ${tryCount + 1} falhou. Tentando novamente...`);
                return this._generate(messages, options, runManager, tryCount + 1);
            }
            const error = err as Error;
            throw new Error(`Erro ao gerar resposta: ${error.message}`, { cause: ERROR_TYPE.LLM });
        }
    }
}