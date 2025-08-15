import Database from "bun:sqlite";
import fs from "fs";
import path, { format } from "path";
import { SQL_DATABASE_PATH } from "../config";
import { AgentLLMService } from "../infra/interfaces/agentLlm.service";
import { ChatPromptTemplate } from "@langchain/core/prompts";
import { toJsonSchema } from "@langchain/core/utils/json_schema";
import { databaseMetadataSchema } from "../core/models/databaseMetadata";

function generateMetadata(dbPath: string, sampleSize = 5) {
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
                description: "" //Gepeto
            };
        }

        /// Conjunto das descrições das colunas + nome da tabela
        metadata.tables[tableName] = {
            columns: columnMeta,
            table_description: "" //Gepeto
        };
    }

    return metadata;
}

async function main() {
    console.log("O barato é louco e o processo é lento, aguenta aí...");
    const llm = new AgentLLMService();
    const prompt = ChatPromptTemplate.fromMessages([
        { role: "system", content: "Você é um agente responsável por criar descrições para elementos de bancos de dados com base no seu conteúdo, você tem acesso a estrutura do banco de dados e nela contem as 5 primeiras linhas de cada coluna, bem como o nome do banco de dado, suas tabelas e colunas, tente fazer a melhor descrição possível para evitar trabalho do revisor.\n" +
        "Responda somente no mesmo formato que vier o input e sempre em portugues - Brasil\n"},
        { role: "user", content: "{database}\n" },
        { role: "user", content: "{format_instructions}" }
    ]);
    const format = (toJsonSchema(databaseMetadataSchema) as any).properties;
    
    const chain = prompt.pipe(llm);

    const files = fs.readdirSync(SQL_DATABASE_PATH);
    for (const file of files) {
        if (path.extname(file) === ".db") {
            console.log(`Processing ${file}...`);
            const rawJson = generateMetadata(path.join(SQL_DATABASE_PATH, file), 5);
            const json = JSON.stringify(rawJson);
            const rawResult = await chain.invoke({ database: json, format_instructions: JSON.stringify(format) });
            const resultString = rawResult.text.replace("```json", "").replace("```", "")
            console.log(resultString);
            const rawParsedResult = JSON.parse(resultString);
            const parsed = databaseMetadataSchema.parse(rawParsedResult);
            const filename = path.basename(file, path.extname(file));
            const outputPath = path.join(SQL_DATABASE_PATH, `${filename}_metadata.json`);
            fs.writeFileSync(outputPath, JSON.stringify(parsed, null, 2), "utf-8");
            console.log(`Metadata for ${file} written to ${outputPath}`);
        }
    };
}

main();