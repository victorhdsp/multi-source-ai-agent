import type { IAgentLLMService } from "@/src/infra/interfaces/agentLlm.gateway";
import { SEARCH_AGENT_STEPS } from "@/src/domain/search/selfAskWithSearch/types/steps";
import type { SearchAgentDTO, SelfAskDTO } from "@/src/domain/search/selfAskWithSearch/types/dto";
import { selfAskState } from "@/src/domain/search/selfAskWithSearch/types";
import { ERROR_MESSAGE } from "@/src/config";
import type { resolveToolType } from "@/src/domain/search/tools/type";
import { logger } from "@/src/tools/logger";
import { formatAgentPrompt, useTools } from "./prompt";

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
            logger.error(ERROR_MESSAGE.NOT_SUPPORT_BIND_TOOLS);
        }
        this.boundCallNode = this.callNode.bind(this);
        this.boundRoute = this.route.bind(this);
    }

    async callNode(state: SearchAgentDTO): Promise<SearchAgentDTO> {
        const tools = useTools(this.bindedTools, this.searchAgentTools);
        const prompt = await formatAgentPrompt(state, tools);
        const chain = prompt.pipe(this.model)
        const chainResult = await chain.invoke({});
        const rawContent = chainResult.text.replace("```json", "").replace("```", "");

        try {
            const rawParsedOutput = JSON.parse(rawContent);
            rawParsedOutput.type = state.llMOutput.type;
            const output = selfAskState.parse(rawParsedOutput);
            
            const newState: SearchAgentDTO = {
                ...state,
                llMOutput: output as SelfAskDTO,
                numberOfSteps: state.numberOfSteps + 1
            };

            logger.thinking(output.content);
            logger.state(newState);

            return newState;
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
