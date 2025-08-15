import { interrupt } from "@langchain/langgraph";
import type { SearchAgentDTO } from "../strategy/strategy";
import { findDBMusicTool } from "./getMusicDB.tool";
import { getPageTool } from "./getPage.tool";
import { ERROR_MESSAGE, HUMAN_REQUEST, HUMAN_RESPONSE } from "@/src/config";

export const searchAgentTools = [getPageTool, findDBMusicTool];

export class SearchAgentTools {
    private talkToHuman(prompt: string): boolean {
        const rawResponse = interrupt({
            type: HUMAN_REQUEST.PERMISSION,
            question: prompt
        });
        
        const response = rawResponse.toLowerCase().trim();

        if (HUMAN_RESPONSE.TRUE.has(response))
            return true;
        if (HUMAN_RESPONSE.FALSE.has(response))
            return false;
        
        return this.talkToHuman(ERROR_MESSAGE.WRONG_INPUT(["s", "n"]));
    }

    async getPage (state: SearchAgentDTO): Promise<SearchAgentDTO> {
        if (!state.permissions.has("INTERNET")) {
            const prompt = (
                "Para conseguir continuar preciso fazer a busca em uma pagina da internet: \n" +
                `${state.llMOutput.content}\n` +
                "Tenho permissão para fazer isso? (y/n)"
            )

            const isPermitted = this.talkToHuman(prompt);

            if (!isPermitted) {
                return {
                    ...state,
                    error: ERROR_MESSAGE.NO_PERMISSION_TO_WEB_SEARCH,
                };
            }
            state.permissions.add("INTERNET");
        }

        const url = getPageTool.schema.parse(state.llMOutput.content).url;
        const response = await getPageTool.invoke({ url: url });

        return {
            ...state,
            history: [...state.history, response],
            searchedSources: [ ...state.searchedSources, url ],
        };
    }

    async getMusicDB (state: SearchAgentDTO): Promise<SearchAgentDTO> {
        return findDBMusicTool;
    }
}