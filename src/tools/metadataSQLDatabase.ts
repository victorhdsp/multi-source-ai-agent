import Database from "bun:sqlite";
import fs from "fs";
import path, { format } from "path";
import { SQL_DATABASE_PATH, SQL_METADATA_PATH } from "../config";
import { AgentLLMService } from "../infra/gateway/agentLlm.service";
import { ChatPromptTemplate } from "@langchain/core/prompts";
import { toJsonSchema } from "@langchain/core/utils/json_schema";
import { logger } from "./logger";
import { databaseMetadataSchema } from "../domain/core/types/databaseMetadata";
import type z from "zod";

type Metadata = z.infer<typeof databaseMetadataSchema>;

export class DBMetadataService {
    private format = (toJsonSchema(databaseMetadataSchema) as any).properties;
    private prompt = ChatPromptTemplate.fromMessages([
        {
            role: "system", content: "Você é um agente responsável por criar descrições para elementos de bancos de dados com base no seu conteúdo, você tem acesso a estrutura do banco de dados e nela contem as 5 primeiras linhas de cada coluna, bem como o nome do banco de dado, suas tabelas e colunas, tente fazer a melhor descrição possível para evitar trabalho do revisor.\n" +
                "Responda somente no mesmo formato que vier o input e sempre em portugues - Brasil\n"
        },
        { role: "user", content: "{database}\n" },
        { role: "user", content: "{format_instructions}" }
    ]);

    constructor(
        private model: AgentLLMService,
    ){
        try {
            fs.readdirSync(SQL_METADATA_PATH);
        } catch {
            fs.mkdirSync(SQL_METADATA_PATH, { recursive: true });
        }
    }

    private async scaffold(dbPath: string, sampleSize = 5) {
        if (!fs.existsSync(dbPath)) {
            throw new Error(`Database file not found: ${dbPath}`);
        }

        const db = new Database(dbPath);
        const dbName = path.basename(dbPath);

        const tables = db.prepare(
            `SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%';`
        ).all() as { name: string }[];

        const metadata: any = {
            database: dbName,
            tables: {}
        };

        for (const { name: tableName } of tables) {
            const columns = db.prepare(`PRAGMA table_info(${tableName});`).all() as { name: string }[];

            const sampleRows = db
                .prepare(`SELECT * FROM ${tableName} LIMIT ${sampleSize};`)
                .all() as Record<string, any>[];

            const columnMeta: any = {};
            for (const col of columns) {
                const sampleValues = sampleRows.map((row) => row[col.name]);
                columnMeta[col.name] = {
                    sample_values: sampleValues,
                    description: ""
                };
            }

            metadata.tables[tableName] = {
                columns: columnMeta,
                table_description: ""
            };
        }

        return metadata;
    }

    private async metadataSchema(metadata: Metadata): Promise<Metadata> {
        const chain = this.prompt.pipe(this.model);
        const rawResult = await chain.invoke({ database: metadata, format_instructions: JSON.stringify(this.format) });
        const resultString = rawResult.text.replace("```json", "").replace("```", "")
        const rawParsedResult = JSON.parse(resultString);
        return databaseMetadataSchema.parse(rawParsedResult);
    }

    private async createfile(content: Metadata, file: string) {
        const filename = path.basename(file, path.extname(file));
        const outputPath = path.join(SQL_METADATA_PATH, `${filename}_metadata.json`);
        fs.writeFileSync(outputPath, JSON.stringify(content, null, 2), "utf-8");
    }

    private async executeUnique(dbPath: string) {
        logger.info(`Generating metadata for database: ${dbPath}`);
        const scaffold = await this.scaffold(dbPath, 5);
        const metadata = await this.metadataSchema(scaffold);
        await this.createfile(metadata, dbPath);
        logger.info(`Metadata for ${dbPath} written successfully.`);
    }

    getDBWithoutMetadata(): string[] {
        try {
            const files = fs.readdirSync(SQL_DATABASE_PATH);
            const dbFiles = files.filter(file => file.endsWith('.db'));
            const metadataFiles = fs.readdirSync(SQL_METADATA_PATH).map(file => file.replace('_metadata.json', '.db'));
            const neededFiles = dbFiles.filter(file => !metadataFiles.includes(file));
            return neededFiles;

        } catch (error) {
            logger.error("Error reading database files:", error);
            return [];
        }
    }

    async execute(files: string[] = []) {
        logger.debug("O barato é louco e o processo é lento, aguenta aí...");
        
        for (const file of files) {
            if (path.extname(file) === ".db") {
                const dbPath = path.join(SQL_DATABASE_PATH, file);
        
                try {
                    await this.executeUnique(dbPath);
                } catch (error) {
                    logger.error(`Error processing ${file}:`, error);
                }
            }
        };
    }
}

async function main() {
    const dbMetadataService = new DBMetadataService(new AgentLLMService());
}

main();