import { ChatPromptTemplate } from "@langchain/core/prompts";
import { toJsonSchema } from "@langchain/core/utils/json_schema";
import type { IAgentLLMService } from "@/src/infra/interfaces/agentLlm.gateway";
import { AnswerTypeStrategy } from "../../types/answerType";
import { SEARCH_AGENT_STEPS } from "./types/steps";
import type { SearchAgentDTO, SelfAskDTO } from "./types/dto";
import { selfAskState } from "./types";
import { ERROR_MESSAGE } from "@/src/config";
import type { resolveToolType } from "../tools/type";

export class SelfAskWithSearchStrategy {
    constructor(
        private readonly model: IAgentLLMService,
        readonly searchAgentTools: resolveToolType[] = []
    ) {
        if (this.model.bindTools) {
            this.model.bindTools(searchAgentTools);
        } else {
            console.error("Este modelo não suporta vinculação de ferramentas");
        }
    }

    private agentSearchPrompt = [
        { role: "system", content: "Você é um agente especialista na decomposição de problemas."},
        { role: "system", content: "Seu objetivo é {objective}."},
        { role: "user", content: "Com base no objetivo, preciso responder o seguinte problema: {problem}."},
        { role: "user", content: "Temos essa lista de informações que estão faltando: {missing}, você tem permissão para adicionar novos items, bem como remover conforme necessário para resolver o problema."},
        { role: "user", content: "Você deve modificar o content" },
        { role: "user", content: "Você deve incrementar {output} e seu trabalho termina quando tiver informações o suficiente para resolver o problema."},
        { role: "user", content: "Você deve responder com o seguinte formato: {format_instructions}"},
    ]

    private async formatAgentPrompt(state: SearchAgentDTO): Promise<ChatPromptTemplate> {
        const format = (toJsonSchema(selfAskState) as any).properties;

        const prompt = ChatPromptTemplate.fromMessages(this.agentSearchPrompt)
            .partial({
                objective: AnswerTypeStrategy[state.llMOutput.type],
                problem: state.userInput,
                missing: state.llMOutput.missing.join(", "),
                output: state.history.join(", "),
                format_instructions: `${JSON.stringify(format)}`
            });

        return prompt;
    }

    async callNode(state: SearchAgentDTO): Promise<SearchAgentDTO> {
        const prompt = await this.formatAgentPrompt(state);
        const chain = prompt.pipe(this.model)
        const chainResult = await chain.invoke({});
        const rawContent = chainResult.text.replace("```json", "").replace("```", "");
        try {
            const rawParsedOutput = JSON.parse(rawContent);
            const output = selfAskState.parse(rawParsedOutput);

            return {
                ...state,
                llMOutput: output as SelfAskDTO,
                numberOfSteps: state.numberOfSteps + 1
            };
        } catch (error) {
            throw new Error(ERROR_MESSAGE.FAIL_TO_PARSE)
        }

    }

    async route(state: SearchAgentDTO): Promise<string> {
        if (state.error || state.numberOfSteps > 5) {
            return SEARCH_AGENT_STEPS.STOP;
        }
        
        if (state.llMOutput.missing.length === 0) {
            return SEARCH_AGENT_STEPS.STOP;
        }

        return state.llMOutput.step;
    }
}
