
import { interrupt } from "@langchain/langgraph";
import { ERROR_MESSAGE, HUMAN_REQUEST, HUMAN_RESPONSE } from "@/src/config";
import type { SearchAgentDTO } from "../selfAskWithSearch/types/dto";
import type { resolveToolType } from "./type";
import { docPageTool } from "./getPage/doc";
import { docFindDBMusicTool } from "./musicDB/doc";
import type { GetPageTool } from "./getPage/tool";
import type { FindMusicDBTool } from "./musicDB/tool";
import { logger } from "@/src/tools/logger";

export class SearchAgentTools {
    searchAgentTools: resolveToolType[] = [];

    protected getPageTool: resolveToolType;
    protected findDBMusicTool: resolveToolType;

    public readonly boundGetPage;
    public readonly boundFindDBMusic;

    constructor(
        getPageTool: GetPageTool,
        findDBMusicTool: FindMusicDBTool
    ){
        this.getPageTool = getPageTool.current;
        this.findDBMusicTool = findDBMusicTool.current;

        this.searchAgentTools.push(this.getPageTool);
        this.searchAgentTools.push(this.findDBMusicTool);

        this.boundGetPage = this.getPage.bind(this);
        this.boundFindDBMusic = this.getMusicDB.bind(this);
    }

    private talkToHuman(prompt: string): boolean {
        let question = prompt;
        const rawResponse = interrupt({
            type: HUMAN_REQUEST.PERMISSION,
            question: question
        });

        while(true) {
            const response = rawResponse.toLowerCase().trim();

            if (HUMAN_RESPONSE.TRUE.has(response))
                return true;
            if (HUMAN_RESPONSE.FALSE.has(response))
                return false;
            question = ERROR_MESSAGE.WRONG_INPUT(["s", "n"]);
        }
    }

    async getPage (state: SearchAgentDTO): Promise<SearchAgentDTO> {
        if (!state.permissions.has("INTERNET")) {
            const prompt = (
                "Para conseguir continuar preciso fazer a busca em uma pagina da internet: \n" +
                `${state.llMOutput.content}\n` +
                "Tenho permissão para fazer isso? (y/n)"
            )

            const isPermitted = true//this.talkToHuman(prompt);

            if (!isPermitted) {
                return {
                    ...state,
                    error: ERROR_MESSAGE.NO_PERMISSION_TO_WEB_SEARCH,
                };
            }
            state.permissions.add("INTERNET");
        }
        
        let newItemToHistory = "";
        const rawStateContent = JSON.parse(state.llMOutput.content)
        const params = docPageTool.schema.parse(rawStateContent);

        try {
            newItemToHistory = await this.getPageTool.invoke({ url: params.url });
        } catch (error) {
            logger.error("Error occurred while fetching page:", error);
            newItemToHistory = `Não consegui acessar a página ${params.url}`;
        }

        return {
            ...state,
            history: [...state.history, newItemToHistory],
            searchedSources: [ ...state.searchedSources, params.url ],
            llMOutput: { ...state.llMOutput, step: "ANALYZE" },
        };
    }

    async getMusicDB (state: SearchAgentDTO): Promise<SearchAgentDTO> {
        const rawStateContent = JSON.parse(state.llMOutput.content);
        const { table, columns, filters } = docFindDBMusicTool.schema.parse(rawStateContent);

        const response = await this.findDBMusicTool.invoke({ table, columns, filters });
        const resultContent = response.join("\n");

        return {
            ...state,
            history: [...state.history, resultContent],
            searchedSources: [ ...state.searchedSources, `music_db:${table}` ],
            llMOutput: { ...state.llMOutput, step: "ANALYZE" },
        }
    }
}
