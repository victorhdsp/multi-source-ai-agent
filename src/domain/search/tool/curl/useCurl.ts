import type z from 'zod';
import { DynamicStructuredTool, tool } from '@langchain/core/tools';
import { useCurlConsume, type UseCurlConsume } from './type';
import { logger } from '@/src/tools/logger';
import { ERROR_MESSAGE } from '@/src/config';
import type { GetPageService } from './getPageService';
import * as cheerio from 'cheerio';
import type { EmbeddingService } from '@/src/infra/gateway/embedding.service';
import type { IDocumentTool, ITool } from '../type';
import type { SearchAgentDTO } from '../../selfAskWithSearch/types/dto';
import { persistentTalk } from '@/src/domain/core/interference/helper/persistentTalk';
import { HUMAN_RESPONSE, INTERRUPT_TYPES } from '@/src/domain/core/interference/type';
import { SEARCH_AGENT_STEPS } from '../../selfAskWithSearch/types/steps';
import { safeJsonParse } from '@/src/utils/safeParser';
import chalk from "chalk";

interface UseCurlTraitment {
    url: string;
    rawContent: string;
    userInput: string;
}

export class UseCurlTool implements ITool<UseCurlConsume, UseCurlTraitment> {
    constructor(
        private readonly service: GetPageService,
        private readonly embedding: EmbeddingService,
    ) {}

    async getDoc(): Promise<IDocumentTool> {
        return {
            name: SEARCH_AGENT_STEPS.USE_CURL,
            description: "A ferramenta para fazer requisições HTTP utilizando curl",
            schema: useCurlConsume
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

    private async requestInternetPermission(state: SearchAgentDTO): Promise<boolean> {
        const { url } = safeJsonParse<UseCurlConsume>(state.llMOutput.content);

        if (!state.permissions.has("INTERNET")) {
            const prompt = (
                "Para conseguir continuar preciso fazer a busca em uma pagina da internet: \n" +
                chalk.italic.underline(`${url}\n`) +
                "Tenho permissão para fazer isso? (y/n)"
            )

            const permission = await persistentTalk(
                prompt,
                INTERRUPT_TYPES.PERMISSION,
                [HUMAN_RESPONSE.TRUE, HUMAN_RESPONSE.FALSE]
            );
            if (!HUMAN_RESPONSE.TRUE.has(permission)) {
                return false
            }
        }

        return true
    }

    async execute(params: UseCurlConsume): Promise<string> {
        try {
            logger.thinking(`Buscando página: ${params.url}`);
            return await this.service.getPage(params);
    
        } catch (err) {
            const error = err as Error; 
            logger.error("[useCurl] (execute):", error.message);
            logger.errorState(error.message, "[useCurl] - Execute");
            return ERROR_MESSAGE.NO_ACCESS_TO_PAGE(params.url);
        }
    }

    async traitResult(params: UseCurlTraitment): Promise<string> {
        const { url, rawContent, userInput } = params;
        logger.thinking(`Processando conteúdo da página: ${url}`);
        try {
            const $ = cheerio.load(rawContent);
            $("script, style, nav, footer, header, aside").remove();
            const rawHTML = $("main, article, p, h1, h2, h3").text();
            const body = rawHTML.replace(/\s+/g, " ").trim();
            const content = await this.embedding.getSimilar(url, body, userInput);

            return content.join("\n\n");
    
        } catch (err) {
            const error = err as Error;
            logger.error("[useCurl] (traitResult):", error.message);
            logger.errorState(error.message, "[useCurl] - TraitResult");
            return ERROR_MESSAGE.NO_ACCESS_TO_PAGE(url);
        }
    }

    async useNode(state: SearchAgentDTO): Promise<SearchAgentDTO> {
        const hasPermission = await this.requestInternetPermission(state);
        
        try {
            if (!hasPermission) throw new Error(ERROR_MESSAGE.NO_PERMISSION_TO_WEB_SEARCH);
            state.permissions.add("INTERNET");

            const userInput = state.userInput
            const rawStateContent = safeJsonParse<UseCurlConsume>(state.llMOutput.content)
            const { url } = useCurlConsume.parse(rawStateContent);

            const tool = await this.getTool();
            const rawContent = await tool.invoke({ url });
            const result = await this.traitResult({ url, rawContent, userInput });

            const newState: SearchAgentDTO = {
                ...state,
                history: [...state.history, result],
                searchedSources: [...state.searchedSources, url],
                llMOutput: { ...state.llMOutput, step: "ANALYZE" },
            };

            logger.thinking(`Página acessada: ${url}`);
            logger.state(newState);

            return newState;
        } catch (err) {
            const error = err as Error;
            logger.error("[useCurl] (useNode):", error.message);
            logger.errorState(error.message, "[useCurl] - UseNode");
            return { ...state, error: error.message };
        }
    }
}