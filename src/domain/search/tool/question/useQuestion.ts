import { DynamicStructuredTool, tool } from '@langchain/core/tools';
import { logger } from '@/src/tools/logger';
import type { IDocumentTool, ITool } from '../type';
import type { SearchAgentDTO } from '../../selfAskWithSearch/types/dto';
import type { InterruptDTO } from '@/src/domain/core/interference/type';
import { useQuestionConsume, type UseQuestionConsume } from './type';
import { SEARCH_AGENT_STEPS } from '../../selfAskWithSearch/types/steps';
import { interrupt } from '@langchain/langgraph';
import { safeJsonParse } from '@/src/utils/safeParser';

export class UseQuestionTool implements ITool<UseQuestionConsume, string> {
    constructor() {
        this.getDoc = this.getDoc.bind(this);
        this.getTool = this.getTool.bind(this);
        this.execute = this.execute.bind(this);
        this.traitResult = this.traitResult.bind(this);
        this.useNode = this.useNode.bind(this);
    }

    async getDoc(): Promise<IDocumentTool> {
        return {
            name: SEARCH_AGENT_STEPS.USE_QUESTION,
            description: (
                "Ferramenta utilizada para fazer perguntas ao usuário e obter respostas que ajudem a resolver o problema.\n" +
                "Regras:\n" +
                "   - Você deve fazer perguntas claras e objetivas para obter respostas precisas.\n" +
                "   - Leve em consideração que é a conversa com um humano, evite grandes blocos de texto sem quebras de linha.\n" +
                "   - Utilize a ferramenta de forma estratégica para obter informações relevantes.\n"
            ),
            schema: useQuestionConsume
        };
    }

    async getTool(): Promise<DynamicStructuredTool> {
        const document = await this.getDoc();

        return tool(
            async (params) => {
                return await this.execute(params);
            },
            document
        );
    }

    async execute(params: UseQuestionConsume): Promise<string> {
        return "Uma resposta qualquer";
    }

    async traitResult(answer: string): Promise<string> {
        return "Um tratamento qualquer"
    }

    private async requestAnswer(state: SearchAgentDTO): Promise<string> {
        const rawStateContent = safeJsonParse<UseQuestionConsume>(state.llMOutput.content)
        const { message } = useQuestionConsume.parse(rawStateContent);

        const answer = interrupt({
            type: "QUESTION",
            message: message,
        } as InterruptDTO);

        return answer;
    }

    async useNode(state: SearchAgentDTO): Promise<SearchAgentDTO> {
        const answer = await this.requestAnswer(state);
        
        try {
            const newState: SearchAgentDTO = {
                ...state,
                history: [...state.history, answer],
                llMOutput: { ...state.llMOutput, step: "ANALYZE" },
            };
            
            logger.state(newState);
            return newState;
        } catch (err) {
            const error = err as Error;
            logger.error("[useQuestion] (useNode):", error.message);
            return { ...state, error: error.message };
        }
    }
}