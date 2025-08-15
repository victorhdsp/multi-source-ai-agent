import { tool } from "@langchain/core/tools";
import type { DocumentTool, ITool } from "../type";
import type { DatabaseConsume, IFindDatabaseService } from "./types";

export class FindMusicDBTool {
  public readonly current;
  
  constructor(
    private readonly document: DocumentTool,
    private readonly service: IFindDatabaseService
  ) {
    this.current = tool(
      async (params: DatabaseConsume) => {
        return await this.execute(params);
      },
      this.document
    );
  }

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
}