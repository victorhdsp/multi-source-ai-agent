import { LlmAgentResponse, type LlmAgentResponseDTO } from "../models/llmAgentResponse.dto";
import { ChatPromptTemplate } from "@langchain/core/prompts";
import { AgentLLMService } from "@/src/infra/interfaces/agentLlm.service";
import { toJsonSchema } from "@langchain/core/utils/json_schema";

export const agentQuestionPrompt = [
    { role: "system", content: "Você é um assistente em tarefas de resposta a perguntas. Use os seguintes trechos de contexto recuperados para responder à pergunta. Se você não souber a resposta, apenas diga que não sabe. Use no máximo três frases e mantenha a resposta concisa." },
    { role: "user", content: "Pergunta: {question}" },
    { role: "user", content: "Contexto: {context}" },
    { role: "user", content: "{format_instructions}" }
]

export class QuestionAgentService {
    constructor(
        private readonly model: AgentLLMService,
    ) {}

    async formatQuestionPrompt(question: string, content: string): Promise<ChatPromptTemplate> {
        const format = (toJsonSchema(LlmAgentResponse) as any).properties;
        
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

    async parseOutput(output: string): Promise<LlmAgentResponseDTO> {
        const rawContent = output.replace("```json", "").replace("```", "");
        const rawParsedOutput = JSON.parse(rawContent);
        return LlmAgentResponse.parse(rawParsedOutput) as LlmAgentResponseDTO;
    }
}