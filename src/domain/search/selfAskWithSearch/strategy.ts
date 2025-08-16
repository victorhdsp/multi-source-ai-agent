import { ChatPromptTemplate, type BaseMessagePromptTemplateLike } from "@langchain/core/prompts";
import { toJsonSchema } from "@langchain/core/utils/json_schema";
import type { IAgentLLMService } from "@/src/infra/interfaces/agentLlm.gateway";
import { AnswerTypeStrategy } from "@/src/domain/core/types/answerType";
import { SEARCH_AGENT_STEPS } from "@/src/domain/search/selfAskWithSearch/types/steps";
import type { SearchAgentDTO, SelfAskDTO } from "@/src/domain/search/selfAskWithSearch/types/dto";
import { selfAskState } from "@/src/domain/search/selfAskWithSearch/types";
import { ERROR_MESSAGE } from "@/src/config";
import type { resolveToolType } from "@/src/domain/search/tools/type";
import { logger } from "@/src/tools/logger";
import { agentSearchPrompt } from "./prompt";

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
            ...agentSearchPrompt,
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
