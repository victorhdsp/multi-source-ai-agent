import fs from 'fs';
import { SQL_DATABASE_PATH } from "@/src/config";
import { databaseMetadataSchema } from "@/src/core/search/tools/musicDB/databaseMetadata";
import { databaseConsumeSchema } from "./types";
import { SEARCH_AGENT_STEPS } from '../../selfAskWithSearch/types/steps';

const rawMetadata = fs.readFileSync(`${SQL_DATABASE_PATH}/music_metadata.json`, { encoding: "utf-8" });
const metadata = databaseMetadataSchema.parse(JSON.parse(rawMetadata));

let database_map = ""
for (const [tableName, tableInfo] of Object.entries(metadata.tables)) {
  database_map += `- ${tableName} -> ${tableInfo.table_description}\n`;
  for (const [columnName, columnInfo] of Object.entries(tableInfo.columns)) {
    database_map += `  - ${columnName} -> ${columnInfo.description} (ex: ${columnInfo.sample_values.slice(0, 3).join(", ")})\n`;
  }
}


export const docFindDBMusicTool = {
  name: SEARCH_AGENT_STEPS.GET_MUSIC_DB,
  description:
    `Consulta dados no banco de dados SQLite,
Banco de dados: ${metadata.database}
Tabelas:
${database_map}
Regras:
- Somente SELECT
- Sempre filtrar com WHERE para evitar trazer tudo
`,
  schema: databaseConsumeSchema
}