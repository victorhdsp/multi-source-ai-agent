
import { interrupt } from "@langchain/langgraph";
import { ERROR_MESSAGE, HUMAN_REQUEST, HUMAN_RESPONSE } from "@/src/config";
import { findDBMusicTool } from "./musicDB/getMusicDB.tool";
import type { SearchAgentDTO } from "../selfAskWithSearch/types/dto";
import type { ITool, resolveToolType } from "./type";
import type { DynamicStructuredTool, DynamicTool } from "@langchain/core/tools";
import { docPageTool } from "./getPage/doc";

export class SearchAgentTools {
    searchAgentTools: resolveToolType[] = [];
    getPageTool: resolveToolType;
    findDBMusicTool: resolveToolType;

    constructor(
        getPageTool: ITool,
        findDBMusicTool: DynamicStructuredTool | DynamicTool
    ){
        this.getPageTool = getPageTool.invoke();
        this.findDBMusicTool = findDBMusicTool;

        this.searchAgentTools.push(this.getPageTool);
        this.searchAgentTools.push(this.findDBMusicTool);
    }

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

        const url = docPageTool.schema.parse(state.llMOutput.content).url;
        const response = await this.getPageTool.invoke({ url: url });

        return {
            ...state,
            history: [...state.history, response],
            searchedSources: [ ...state.searchedSources, url ],
        };
    }

    async getMusicDB (state: SearchAgentDTO): Promise<SearchAgentDTO> {
        const rawStateContent = JSON.parse(state.llMOutput.content);
        const { table, columns, filters } = findDBMusicTool.schema.parse(rawStateContent);

        const response = await findDBMusicTool.invoke({ table, columns, filters });
        const resultContent = response.join("\n");

        return {
            ...state,
            history: [...state.history, resultContent],
            searchedSources: [ ...state.searchedSources, `music_db:${table}` ],
        }
    }
}
