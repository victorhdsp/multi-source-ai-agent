import { tool } from "@langchain/core/tools";
import type { DocumentTool } from "../type";
import type { DatabaseConsume, ISelectDatabaseService } from "./types";
import type { GenericTool } from "../genericTool";

//const db = new Database(`${SQL_DATABASE_PATH}/music.db`);

export class GetMusicDBTool implements GenericTool {
  constructor(
    private readonly document: DocumentTool,
    private readonly service: ISelectDatabaseService
  ) { }

  async execute(params: DatabaseConsume): Promise<string> {
    const rawResponse = await this.service.selectFromDatabase(params);

    const rows = rawResponse.map((item) => {
      const entries = Object.entries(item)
        .map(([key, value]) => `"${key}": "${value}"`).join(", ");
      return `  - ${entries}`;
    });

    const response = (
      `Consulta ao banco de dados: ${"music.db"}\n` +
      `Tabelas: ${params.table}\n` +
      `Filtros: ${params.filters}\n` +
      `Colunas: ${params.columns?.join(", ") || "Todas"}\n` +
      `Resultado:\n` +
      rows.join("\n") 
    )
    return response;
  }

  invoke() {
    return tool(
      async (params: DatabaseConsume) => {
        try {
          return this.execute(params);
        } catch (err) {
          const error = err as Error;
          throw new Error(error.message || "Failed to get music database content");
        }
      },
      this.document
    );
  }
}