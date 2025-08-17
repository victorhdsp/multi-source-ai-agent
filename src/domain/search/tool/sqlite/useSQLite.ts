import fs from "fs";
import path from "path";
import type { EmbeddingService } from "@/src/infra/gateway/embedding.service";
import type { IDocumentTool, ITool } from "../type";
import { useSQLiteConsume, type UseSQLiteConsume } from "./type";
import { tool, type DynamicStructuredTool } from "@langchain/core/tools";
import { logger } from "@/src/tools/logger";
import type { FindDBService } from "./findDBService";
import { ERROR_MESSAGE, SQL_METADATA_PATH } from "@/src/config";
import { SEARCH_AGENT_STEPS } from "../../selfAskWithSearch/types/steps";
import { databaseMetadataSchema } from "@/src/domain/core/types/databaseMetadata";
import type { SearchAgentDTO } from "../../selfAskWithSearch/types/dto";
import { safeJsonParse } from "@/src/utils/safeParser";

interface UseSqliteTraitment {
    path: string;
    rawContent: string;
    userInput: string;
}

export class UseSQLiteTool implements ITool<UseSQLiteConsume, UseSqliteTraitment> {
    constructor(
        private readonly service: FindDBService,
        private readonly embedding: EmbeddingService
    ) { }

    private getSpecificDBDescription(pathURL: string): string {
        const rawMetadata = fs.readFileSync(pathURL, { encoding: "utf-8" });
        const metadata = databaseMetadataSchema.parse(safeJsonParse(rawMetadata));

        let description = "Nome do banco de dados: " + metadata.database + "\n";
        description += `Path: ${path.join(SQL_METADATA_PATH, metadata.database)}\n`;
        for (const [tableName, tableInfo] of Object.entries(metadata.tables)) {
            description += `- ${tableName} -> ${tableInfo.table_description}\n`;
            for (const [columnName, columnInfo] of Object.entries(tableInfo.columns)) {
                description += `  - ${columnName} -> ${columnInfo.description} (ex: ${columnInfo.sample_values.slice(0, 3).join(", ")})\n`;
            }
        }

        return description;
    }

    async getDoc(): Promise<IDocumentTool> {
        const dbs = fs.readdirSync(SQL_METADATA_PATH);

        const databaseMap = dbs.map(db => {
            const fullPath = path.join(SQL_METADATA_PATH, db);
            return this.getSpecificDBDescription(fullPath);
        }).join("\n\n");

        return {
            name: SEARCH_AGENT_STEPS.USE_SQL,
            description: (
                "Consulta dados no banco de dados SQLite" +
                `Você tem acesso ao seguinte banco de dados:\n` +
                `(${dbs.map(db => db.replace("_metadata.json", ".db")).join(", ")})\n` +
                "Descrição dos bancos de dados:\n" +
                databaseMap + "\n\n" +
                "Regras:" +
                "- Você precisa escolher o banco de dados, enviar o `path`, a `table`, os `filters` e as `columns` que precisa para receber os dados." +
                "- Sempre que possível, utilize filtros para limitar a quantidade de dados retornados." +
                "- Se não tiver certeza sobre o nome do banco de dados, tabela ou colunas, consulte a descrição do banco de dados." +
                "- Sempre retorne os dados no formato especificado."
            ),
            schema: useSQLiteConsume
        }
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

    async execute(params: UseSQLiteConsume): Promise<string> {
        logger.thinking(`Consultando banco de dados:\n\t${params.path}`);

        try {
            const rawResponse = await this.service.selectFromDatabase(params);

            const rows = rawResponse.map((item) => {
                const entries = Object.entries(item)
                    .map(([key, value]) => `"${key}": "${value}"`).join(", ");
                return `  - ${entries}`;
            });

            const rawConsult = (
                `Consulta ao banco de dados: \n\t${params.path}\n` +
                `Tabelas: ${params.table}\n` +
                `Filtros: ${params.filters}\n` +
                `Colunas: ${params.columns?.join(", ") || "Todas"}\n` +
                `Resultado:\n` +
                rows.join("\n")
            )

            return rawConsult;

        } catch (error) {
            logger.error(`[useSQLite] Erro ao consultar banco de dados: ${error}`);
            return ERROR_MESSAGE.NO_ACCESS_TO_DB(params.table, params.path);
        }
    }

    async traitResult(params: UseSqliteTraitment): Promise<string> {
        const { path, rawContent, userInput } = params;
        const result = await this.embedding.getSimilar(
            path,
            rawContent,
            userInput
        )

        return result.join("\n\n");
    }

    async useNode(state: SearchAgentDTO): Promise<SearchAgentDTO> {
        try {
            const rawStateContent = safeJsonParse<UseSQLiteConsume>(state.llMOutput.content);
            const { path, table, columns, filters } = useSQLiteConsume.parse(rawStateContent);

            const userInput = state.userInput;
            const tool = await this.getTool();
            const rawContent = await tool.invoke({ path, table, columns, filters });
            const result = await this.traitResult({ path, rawContent, userInput });

            const newState: SearchAgentDTO = {
                ...state,
                history: [...state.history, result],
                searchedSources: [...state.searchedSources, path],
                llMOutput: { ...state.llMOutput, step: "ANALYZE" },
            };

            logger.thinking(`Buscando na tabela: ${table}.`);
            logger.state(newState);

            return newState;
        } catch (err) {
            const error = err as Error;
            logger.error("[useSQLite] (useNode):", error.message);
            return { ...state, error: error.message };
        }
    }
}