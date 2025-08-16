import { tool } from "@langchain/core/tools";
import type { DocumentTool } from "@/src/domain/search/tools/type";
import type { GetPageConsume, IGetService } from "@/src/domain/search/tools/getPage/type";
import { ERROR_MESSAGE } from "@/src/config";
import { logger } from "@/src/tools/logger";
import * as cheerio from "cheerio";
import type { EmbeddingService } from "@/src/infra/gateway/embedding.service";

export class GetPageTool {
  public current;

  constructor(
    private readonly document: DocumentTool,
    private readonly service: IGetService,
    private readonly embedding: EmbeddingService
  ) { 

    this.current = tool(
      async (params: GetPageConsume) => {
        try {
          return await this.execute(params);
        } catch (err) {
          const error = err as Error;
          throw new Error(error.message || "Failed to get page content");
        }
      },
      this.document
    );
  }

  async execute(params: GetPageConsume): Promise<string> {
    try {
      logger.thinking(`Buscando página: ${params.url}`);
      return await this.service.getPage(params);

    } catch (err) {
      const error = err as Error; 
      logger.error(error.message);
      return ERROR_MESSAGE.NO_ACCESS_TO_PAGE(params.url);
    }
  }

  async traitResult(url: string, rawContent: string, userInput: string): Promise<string> {
    try {
      logger.thinking(`Processando conteúdo da página: ${url}`);
      const $ = cheerio.load(rawContent);
      $("script, style, nav, footer, header, aside").remove();
      const rawHTML = $("main, article, p, h1, h2, h3").text();
      const body = rawHTML.replace(/\s+/g, " ").trim();
      const content = await this.embedding.getSimilar(
        url,
        body,
        userInput
      )
      return content.join("\n\n");

    } catch (err) {
      const error = err as Error;
      logger.error(error.message);
      return ERROR_MESSAGE.NO_ACCESS_TO_PAGE(url);
    }
  } 
}