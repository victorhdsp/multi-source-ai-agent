import { SQL_DATABASE_PATH } from "@/src/config";
import { tool } from "@langchain/core/tools";
import z from "zod";
import Database from "better-sqlite3";
import fs from "fs";
import { databaseMetadataSchema } from '../../models/databaseMetadata';

const db = new Database(`${SQL_DATABASE_PATH}/music.db`);
const rawMetadata = fs.readFileSync(`${SQL_DATABASE_PATH}/music_metadata.json`, {encoding: "utf-8"});
const metadata = databaseMetadataSchema.parse(JSON.parse(rawMetadata));

let database_map = ""
for (const [tableName, tableInfo] of Object.entries(metadata.tables)) {
  database_map += `- ${tableName} -> ${tableInfo.table_description}\n`;
  for (const [columnName, columnInfo] of Object.entries(tableInfo.columns)) {
    database_map += `  - ${columnName} -> ${columnInfo.description} (ex: ${columnInfo.sample_values.slice(0, 3).join(", ")})\n`;
  }
}

export const docFindDBMusicTool = {
    name: "query_database",
    description: 
`Consulta dados no banco de dados SQLite.  
Banco de dados: ${metadata.database}
Tabelas:
${database_map}
Regras:
- Somente SELECT
- Sempre filtrar com WHERE para evitar trazer tudo
`,
    schema: z.object({
      table: z.string().describe("Nome da tabela para buscar"),
      columns: z.array(z.string()).optional().describe("Colunas a serem retornadas"),
      filters: z.string().optional().describe("Condições WHERE para filtrar resultados"),
    }),
  }

export const queryDatabaseTool = tool(
  async ({ table, filters, columns }) => {
    const cols = columns?.length ? columns.join(", ") : "*";
    let query = `SELECT ${cols} FROM ${table}`;
    if (filters) {
      query += ` WHERE ${filters}`;
    }
    return db.prepare(query).all();
  },
  docFindDBMusicTool
);