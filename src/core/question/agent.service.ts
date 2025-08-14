import type { LlmAgentResponseDTO } from "../models/llmAgentResponse.dto";
import { ChatPromptTemplate } from "@langchain/core/prompts";
import { LlmAgentResponseParser } from "../utils/outputParser/LlmAgentResponse.parser";
import { AgentLLMService } from "@/src/infra/interfaces/agentLlm.service";

export const agentQuestionPrompt = [
    { role: "system", content: "Você é um assistente em tarefas de resposta a perguntas. Use os seguintes trechos de contexto recuperados para responder à pergunta. Se você não souber a resposta, apenas diga que não sabe. Use no máximo três frases e mantenha a resposta concisa." },
    { role: "user", content: "Pergunta: {question}" },
    { role: "user", content: "Contexto: {context}" },
    { role: "user", content: "{format_instructions}" }
]

export class QuestionAgentService {
    constructor(
        private readonly model: AgentLLMService,
        private readonly parser: LlmAgentResponseParser
    ) {}

    async execute(question: string, content: string): Promise<LlmAgentResponseDTO> {
        const prompt = await ChatPromptTemplate.fromMessages(agentQuestionPrompt)
            .partial({ format_instructions: this.parser.getFormatInstructions() });

        const chain = prompt.pipe(this.model);
        const chainResult = await chain.invoke({ question, context: content });

        const result = await this.parser.parse(chainResult.text);
        return result;
    }
}