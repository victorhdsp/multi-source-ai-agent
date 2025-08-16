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
import * as cheerio from "cheerio";

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
        
        let newItemToHistory = "";
        const rawStateContent = JSON.parse(state.llMOutput.content)
        const params = docPageTool.schema.parse(rawStateContent);

        try {
            logger.thinking(`Buscando página: ${params.url}`);
            const bruteHTML = await this.getPageTool.invoke({ url: params.url });
            const $ = cheerio.load(bruteHTML);
            // ADD SIMILAR TO docPageTool -- Embedding
            newItemToHistory = $("body").text().replace(/\s+/g, ' ').trim()
        } catch (error) {
            logger.error("Error occurred while fetching page:", error);
            newItemToHistory = `Não consegui acessar a página ${params.url}`;
        }

        logger.thinking(`Página acessada: ${params.url}`);


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

        logger.thinking(`Buscando na tabela: ${table}.`);

        return {
            ...state,
            history: [...state.history, resultContent],
            searchedSources: [ ...state.searchedSources, `music_db:${table}` ],
            llMOutput: { ...state.llMOutput, step: "ANALYZE" },
        }
    }
}
