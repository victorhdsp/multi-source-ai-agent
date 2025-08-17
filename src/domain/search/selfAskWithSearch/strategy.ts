import type { IAgentLLMService } from "@/src/infra/interfaces/agentLlm.gateway";
import { SEARCH_AGENT_STEPS } from "@/src/domain/search/selfAskWithSearch/types/steps";
import type { SearchAgentDTO, SelfAskDTO } from "@/src/domain/search/selfAskWithSearch/types/dto";
import { selfAskState } from "@/src/domain/search/selfAskWithSearch/types";
import { ERROR_MESSAGE } from "@/src/config";
import { logger } from "@/src/tools/logger";
import { formatAgentPrompt, useTools } from "./prompt";
import type { ToolBoxService } from "../tool/service";
import type { BaseMessagePromptTemplateLike } from "@langchain/core/prompts";
import { safeJsonParse } from "@/src/utils/safeParser";
import { secureExec } from "@/src/utils/secureExec";

export class SelfAskWithSearchStrategy {
    public readonly boundCallNode;
    public readonly boundRoute;
    private tools: BaseMessagePromptTemplateLike[] = [];

    constructor(
        private readonly model: IAgentLLMService,
        private readonly toolBox: ToolBoxService
    ) {
        this.boundCallNode = this.callNode.bind(this);
        this.boundRoute = this.route.bind(this);
    }

    async init() {
        const tools = await this.toolBox.getAllTools();

        if (this.model.bindTools) {
            this.model.bindTools(tools);
        } else {
            logger.warn("[SelfAskWithSearch] (init):", ERROR_MESSAGE.NOT_SUPPORT_BIND_TOOLS);
            this.tools = useTools(tools);
        }
    }

    async callNode(state: SearchAgentDTO): Promise<SearchAgentDTO> {
        try {
            const prompt = await formatAgentPrompt(state, this.tools);
            const chain = prompt.pipe(this.model)
            const chainResult = await chain.invoke({});

            const output = secureExec(() => {
                const rawParsedOutput = safeJsonParse<SelfAskDTO>(chainResult.text);
                rawParsedOutput.type = state.llMOutput.type;
                return selfAskState.parse(rawParsedOutput);
            }, ERROR_MESSAGE.FAIL_TO_PARSE);

            const newState: SearchAgentDTO = {
                ...state,
                llMOutput: output as SelfAskDTO,
                numberOfSteps: state.numberOfSteps + 1
            };

            if (newState.llMOutput.step === SEARCH_AGENT_STEPS.ANALYZE) {
                newState.llMOutput.step = SEARCH_AGENT_STEPS.STOP;
            }

            logger.thinking(output.content);
            logger.state(newState);

            return newState;
        } catch (err) {
            const error = err as Error;
            throw new Error(`Erro ao chamar o nó <SelfAskWithSearch> {${error.message}}`);
        }
    }

    async route(state: SearchAgentDTO): Promise<string> {
        if (state.error) {
            logger.error("[SelfAskWithSearch] (Route):", state.error);
            logger.errorState(state.error, "[SelfAskWithSearch] - Route");
            return SEARCH_AGENT_STEPS.STOP;
        }

        if (state.numberOfSteps > 5) {
            logger.warn(`Limite de ${state.numberOfSteps} tentativas atingido`);
            return SEARCH_AGENT_STEPS.STOP;
        }

        if (state.llMOutput.step === SEARCH_AGENT_STEPS.STOP) {
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
