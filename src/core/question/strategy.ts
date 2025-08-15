
import { ChatPromptTemplate } from "@langchain/core/prompts";
import { toJsonSchema } from "@langchain/core/utils/json_schema";
import type { IAgentLLMService } from "@/src/infra/interfaces/agentLlm.gateway";
import { QuestionAgent, type QuestionAgentDTO } from "../models/llmAgentResponse.dto";
import { ERROR_MESSAGE } from "@/src/config";

export const agentQuestionPrompt = [
    { role: "system", content: "Você é um assistente em tarefas de resposta a perguntas. Use os seguintes trechos de contexto recuperados para responder à pergunta. Se você não souber a resposta, apenas diga que não sabe. Use no máximo três frases e mantenha a resposta concisa." },
    { role: "user", content: "Pergunta: {question}" },
    { role: "user", content: "Contexto: {context}" },
    { role: "user", content: "{format_instructions}" }
]

export class QuestionAgentStrategy {
    constructor(
        private readonly model: IAgentLLMService,
    ) {}

    async formatQuestionPrompt(question: string, content: string): Promise<ChatPromptTemplate> {
        const format = (toJsonSchema(QuestionAgent) as any).properties;
        
        const prompt = await ChatPromptTemplate.fromMessages(agentQuestionPrompt)
            .partial({
                format_instructions: `${JSON.stringify(format)}`,
                question,
                context: content
            });

        return prompt;
    }

    async sendToModel(prompt: ChatPromptTemplate): Promise<string> {
        const chain = prompt.pipe(this.model);
        const chainResult = await chain.invoke({});
        return chainResult.text;
    }

    async parseOutput(output: string): Promise<QuestionAgentDTO> {
        const rawContent = output.replace("```json", "").replace("```", "");
        
        try {
            const rawParsedOutput = JSON.parse(rawContent);
            const parser = QuestionAgent.parse(rawParsedOutput) as QuestionAgentDTO;
            return parser;
        } catch (error: any) {
            throw new Error(ERROR_MESSAGE.FAIL_TO_PARSE);
        }
    }
}