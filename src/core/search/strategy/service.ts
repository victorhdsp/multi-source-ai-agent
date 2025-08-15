import { ChatPromptTemplate } from "@langchain/core/prompts";
import { llmSearchResponse, SEARCH_AGENT_STEPS, searchAgentState, type LLMSearchResponseDTO, type SearchAgentDTO } from "./strategy";
import { TypeResponseStrategy } from "../../types";
import { toJsonSchema } from "@langchain/core/utils/json_schema";
import type { IAgentLLMService } from "@/src/infra/interfaces/agentLlm.gateway";

export class SelfAskWithSearchService {
    constructor(
        private readonly model: IAgentLLMService,
    ) {}

    private agentSearchPrompt = [
        { role: "system", content: "Você é um agente especialista na decomposição de problemas."},
        { role: "system", content: "Seu objetivo é {objective}."},
        { role: "system", content: "Você tem acesso a ferramentas que podem ajudar na sua tarefa."},
        { role: "system", content: "Você pode fazer perguntas para essas ferramentas para ter mais informações, se fizer sentido (não adianta buscar sobre finanças em um banco de dados de cachorros), no formato: {tool.action}({tool.format})."},
        { role: "user", content: "Com base no objetivo, preciso responder o seguinte problema: {problem}."},
        { role: "user", content: "Temos essa lista de informações que estão faltando: {missing}, você tem permissão para adicionar novos items, bem como remover conforme necessário para resolver o problema."},
        { role: "user", content: "Você deve modificar o content" },
        { role: "user", content: "Você deve incrementar {output} e seu trabalho termina quando tiver informações o suficiente para resolver o problema."},
        { role: "user", content: "Você deve responder com o seguinte formato: {format_instructions}"},
    ]

    private async formatAgentPrompt(state: SearchAgentDTO): Promise<ChatPromptTemplate> {
        const format = (toJsonSchema(llmSearchResponse) as any).properties;
        
        const prompt = ChatPromptTemplate.fromMessages(this.agentSearchPrompt)
            .partial({
                objective: TypeResponseStrategy[state.llMOutput.type],
                problem: state.userInput,
                missing: state.llMOutput.missing.join(", "),
                output: state.history.join(", "),
                format_instructions: `${JSON.stringify(format)}`
            });

        return prompt;
    }

    async agent(state: SearchAgentDTO): Promise<SearchAgentDTO> {
        const prompt = await this.formatAgentPrompt(state);
        const chain = prompt.pipe(this.model);
        const chainResult = await chain.invoke({});
        const rawContent = chainResult.text.replace("```json", "").replace("```", "");
        const rawParsedOutput = JSON.parse(rawContent);
        const output = llmSearchResponse.parse(rawParsedOutput);

        return {
            ...state,
            llMOutput: output as LLMSearchResponseDTO,
            numberOfSteps: state.numberOfSteps + 1
        };
    }

    async decide(state: SearchAgentDTO): Promise<string> {
        if (state.error || state.numberOfSteps > 5) {
            return SEARCH_AGENT_STEPS.STOP;
        }
        
        if (state.llMOutput.missing.length === 0) {
            return SEARCH_AGENT_STEPS.STOP;
        }

        return state.llMOutput.step;
    }
}
