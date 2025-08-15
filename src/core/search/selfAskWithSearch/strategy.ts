import { ChatPromptTemplate, type BaseMessagePromptTemplateLike } from "@langchain/core/prompts";
import { toJsonSchema } from "@langchain/core/utils/json_schema";
import type { IAgentLLMService } from "@/src/infra/interfaces/agentLlm.gateway";
import { AnswerTypeStrategy } from "../../types/answerType";
import { SEARCH_AGENT_STEPS } from "./types/steps";
import type { SearchAgentDTO, SelfAskDTO } from "./types/dto";
import { selfAskState } from "./types";
import { ERROR_MESSAGE } from "@/src/config";
import type { resolveToolType } from "../tools/type";
import { logger } from "@/src/tools/logger";

export class SelfAskWithSearchStrategy {
    public readonly boundCallNode;
    public readonly boundRoute;
    private bindedTools: boolean = false;

    constructor(
        private readonly model: IAgentLLMService,
        readonly searchAgentTools: resolveToolType[] = []
    ) {
        if (this.model.bindTools) {
            this.model.bindTools(searchAgentTools);
            this.bindedTools = true;
        } else {
            logger.error("Este modelo não suporta vinculação de ferramentas");
        }
        this.boundCallNode = this.callNode.bind(this);
        this.boundRoute = this.route.bind(this);
    }

    private agentSearchPrompt: BaseMessagePromptTemplateLike[] = [
        { role: "system", content: "Você faz parte de um time de agentes que tem como objetivo resolver o problema enviado pelo cliente, antes do problema chegar em você ele já passou pelo 'assistente' que fez uma análise inicial, classificou o tipo do problema como {problem_type} e começou a decompor o problema em partes menores."},
        { role: "user", content: "Você precisa dar uma solução se conseguir para o problema do usuário seguindo o formato da classificação seguindo o seu objetivo: {objective}" },
        { role: "user", content: "Caso você não tenha informação o suficiente para solucionar o problema no seu histórico de informações, você pode usar alguma das ferramentas disponíveis para incremetar o histórico de informações e tentar novamente." },
        { role: "user", content: "Caso olhando para as ferramentas disponíveis você ache que não vai conseguir obter a resposta, então retorne um erro, mas jamais responda sem ter uma informação precisa disponível." },
        { role: "user", content: "O histórico atual de informações é: {history}" },
        { role: "user", content: "Você vai receber uma lista de informações faltantes que devem ser importantes para resolver o problema, você pode adicionar novas informações que você julgar necessário, você tambem pode remover, porém apenas se no histórico tiver a informação que responda essa pergutna." },
        { role: "user", content: "Esse é um schema feito no Zod que representa o formato da resposta, ela sempre deve seguir esse schema: {format_instructions}, dentro do schema tem `type` que não deve ser modficado, `step` é usado para que o cliente tenha noção do passo atual, porém eles tem valores específicos que são: {steps}, caso você consiga responder a pergunta utilize o step de WHATNOT e caso esteja resolvido mesmo que ainda tenha perguntas use o step de STOP." },
        { role: "user", content: "O problema do usuário é: {problem}" },
        { role: "user", content: "As informações faltantes são: {missing}" },
    ]

    private useTools() {
        if (this.bindedTools) {
            return [];
        }

        const tools = this.searchAgentTools.map(tool => {
            const schema = (toJsonSchema(tool.schema) as any).properties;
            return `Nome da ferramenta: ${tool.name}, Descrição da ferramenta e como usar: ${tool.description}, Schema do input esperado pela ferramenta: ${schema}`;
        }).join("\n");

        const prompt: BaseMessagePromptTemplateLike[] = [
            { role: "user", content: "Você tem as seguintes ferramentas disponíveis para utilizar: " },
            { role: "user", content: tools },
            { role: "user", content: "Para utilizar essas ferramentas, seguindo o schema, você deve marcar o step correspondente e o content deve ser uma string contendo o schema de parâmetros." }
        ];
        return prompt;
    }

    private async formatAgentPrompt(state: SearchAgentDTO): Promise<ChatPromptTemplate> {
        const format = (toJsonSchema(selfAskState) as any).properties;

        const problemType = Object.entries(AnswerTypeStrategy).map(([key, value]) => {
            return `"${key}" - "${value}"`;
        }).join(", ");

        const history = state.history.join("\n");

        const steps = Object.keys(SEARCH_AGENT_STEPS).join(", ");

        const prompt = ChatPromptTemplate.fromMessages([
            ...this.agentSearchPrompt,
            ...this.useTools()
        ])
            .partial({
                problem_type: problemType,
                objective: AnswerTypeStrategy[state.llMOutput.type],
                history: history,
                format_instructions: `${JSON.stringify(format)}`,
                steps: steps,
                problem: state.userInput,
                missing: state.llMOutput.missing.join(", "),
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
            rawParsedOutput.type = state.llMOutput.type;
            const output = selfAskState.parse(rawParsedOutput);
            
            logger.thinking(output.content);

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
        if (state.error) {
            logger.error(state.error);
            return SEARCH_AGENT_STEPS.STOP;
        }

        if (state.numberOfSteps > 5) {
            logger.warn(`Limite de ${state.numberOfSteps} tentativas atingido`);
            return SEARCH_AGENT_STEPS.STOP;
        }

        if (state.llMOutput.missing.length === 0) {
            logger.thinking("Já sei a resposta!");
            return SEARCH_AGENT_STEPS.STOP;
        }

        if (state.llMOutput.step === SEARCH_AGENT_STEPS.WHATNOT) {
            logger.thinking("Não consegui responder a pergunta.");
            return SEARCH_AGENT_STEPS.WHATNOT;
        }
        
        logger.thinking(`Vou tentar ${state.llMOutput.step} para obter mais informações.`);
        return state.llMOutput.step;
    }
}
