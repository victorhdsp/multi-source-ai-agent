import { ERROR_MESSAGE } from "@/src/config";
import type { SearchAgentDTO } from "@/src/domain/search/selfAskWithSearch/types/dto";
import type { resolveToolType } from "@/src/domain/search/tools/type";
import { docPageTool } from "@/src/domain/search/tools/getPage/doc";
import { docFindDBMusicTool } from "@/src/domain/search/tools/musicDB/doc";
import type { GetPageTool } from "@/src/domain/search/tools/getPage/tool";
import type { FindMusicDBTool } from "@/src/domain/search/tools/musicDB/tool";
import { logger } from "@/src/tools/logger";
import { persistentTalk } from "../../core/interference/helper/persistentTalk";
import { HUMAN_RESPONSE, INTERRUPT_TYPES, type InterruptType } from "../../core/types/human";

export class SearchAgentTools {
    searchAgentTools: resolveToolType[] = [];

    public readonly boundGetPage;
    public readonly boundFindDBMusic;

    constructor(
        private getPageTool: GetPageTool,
        private findDBMusicTool: FindMusicDBTool
    ){
        this.searchAgentTools.push(this.getPageTool.current);
        this.searchAgentTools.push(this.findDBMusicTool.current);

        this.boundGetPage = this.getPage.bind(this);
        this.boundFindDBMusic = this.getMusicDB.bind(this);
    }

    async getPage (state: SearchAgentDTO): Promise<SearchAgentDTO> {
        if (!state.permissions.has("INTERNET")) {
            const prompt = (
                "Para conseguir continuar preciso fazer a busca em uma pagina da internet: \n" +
                `${state.llMOutput.content}\n` +
                "Tenho permissão para fazer isso? (y/n)"
            )

            const permission = await persistentTalk(
                prompt,
                INTERRUPT_TYPES.PERMISSION as InterruptType,
                [HUMAN_RESPONSE.TRUE, HUMAN_RESPONSE.FALSE]
            );
            if (!HUMAN_RESPONSE.TRUE.has(permission)) {
                return {
                    ...state,
                    error: ERROR_MESSAGE.NO_PERMISSION_TO_WEB_SEARCH,
                };
            }
            state.permissions.add("INTERNET");
        }
        
        const userInput = state.userInput
        const rawStateContent = JSON.parse(state.llMOutput.content)
        const params = docPageTool.schema.parse(rawStateContent);
        const rawResult = await this.getPageTool.current.invoke({ url: params.url });
        const result = await this.getPageTool.traitResult(params.url, rawResult, userInput);
        
        const newState: SearchAgentDTO = {
            ...state,
            history: [...state.history, result],
            searchedSources: [ ...state.searchedSources, params.url ],
            llMOutput: { ...state.llMOutput, step: "ANALYZE" },
        };

        logger.thinking(`Página acessada: ${params.url}`);
        logger.state(newState);

        return newState;
    }

    async getMusicDB (state: SearchAgentDTO): Promise<SearchAgentDTO> {
        const rawStateContent = JSON.parse(state.llMOutput.content);
        const { table, columns, filters } = docFindDBMusicTool.schema.parse(rawStateContent);

        const userInput = state.userInput;
        const rawResult = await this.findDBMusicTool.current.invoke({ table, columns, filters });
        const name = `${table} - ${filters || "none"} - ${columns?.join(", ") || "all"}`;
        const result = await this.findDBMusicTool.traitResult(name, rawResult, userInput);

        const newState: SearchAgentDTO = {
            ...state,
            history: [...state.history, result],
            searchedSources: [ ...state.searchedSources, `music_db:${table}` ],
            llMOutput: { ...state.llMOutput, step: "ANALYZE" },
        };
        logger.thinking(`Buscando na tabela: ${table}.`);
        logger.state(newState);

        return newState;
    }
}
