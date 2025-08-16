import { tool } from "@langchain/core/tools";
import type { DocumentTool, ITool } from "@/src/domain/search/tools/type";
import type { DatabaseConsume, IFindDatabaseService } from "@/src/domain/search/tools/musicDB/types";
import type { EmbeddingService } from "@/src/infra/gateway/embedding.service";
import { logger } from "@/src/tools/logger";
import { ERROR_MESSAGE } from "@/src/config";

export class FindMusicDBTool {
  public readonly current;
  
  constructor(
    private readonly document: DocumentTool,
    private readonly service: IFindDatabaseService,
    private readonly embedding: EmbeddingService
  ) {
    this.current = tool(
      async (params) => {
        return await this.execute(params);
      },
      this.document
    );
  }

  async execute(params: DatabaseConsume): Promise<string> {
    logger.thinking(`Consultando banco de dados: ${params.table}`);
    try {
      const rawResponse = await this.service.selectFromDatabase(params);

      const rows = rawResponse.map((item) => {
        const entries = Object.entries(item)
          .map(([key, value]) => `"${key}": "${value}"`).join(", ");
        return `  - ${entries}`;
      });

      const rawConsult = (
        `Consulta ao banco de dados: ${"music.db"}\n` +
        `Tabelas: ${params.table}\n` +
        `Filtros: ${params.filters}\n` +
        `Colunas: ${params.columns?.join(", ") || "Todas"}\n` +
        `Resultado:\n` +
        rows.join("\n") 
      )

      return rawConsult;

    } catch (error) {
      logger.error(`Erro ao consultar banco de dados: ${error}`);
      return ERROR_MESSAGE.NO_ACCESS_TO_DB(params.table, "music.db");
    }
  }
  
  async traitResult(name: string, rawContent: string, userInput: string): Promise<string> {
    const result = await this.embedding.getSimilar(
      name,
      rawContent,
      userInput
    )
  
    return result.join("\n\n");;
  }
}