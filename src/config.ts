import path from 'path';
import fs from 'fs';

export const SYSTEM_DATA = {
    name: "multi-agent",
    version: "0.1.0",
    currentSeason: null as string | null,
}

export const EMBEDDING_MODEL =process.env.EMBEDDING_MODEL || "text-embedding-004";
export const PRIMARY_MODEL = process.env.PRIMARY_MODEL || "gemini-2.0-flash-lite";
export const SECONDARY_MODEL = process.env.SECONDARY_MODEL || "gemini-2.5-flash";
export const GOOGLE_API_KEY = process.env.GOOGLE_API_KEY;

const resolvePath = (relativePath: string) => {
    try {
        fs.readdirSync(relativePath);
    } catch {
        fs.mkdirSync(relativePath, { recursive: true });
    }
}
export const SQL_DATABASE_PATH = path.join(__dirname, '../data/sqlite');
resolvePath(SQL_DATABASE_PATH);
export const SQL_METADATA_PATH = path.join(__dirname, 'infra/database/sqlite');
resolvePath(SQL_METADATA_PATH);
export const VECTOR_DATABASE_PATH = path.join(__dirname, 'infra/database/vector');
resolvePath(VECTOR_DATABASE_PATH);
export const LOG_PATH = path.join(__dirname, '../logs');
resolvePath(LOG_PATH);

export const ERROR_TYPE = {
    PARSER: "ParserError",
    LLM: "LLMError"
}

export const ERROR_MESSAGE = {
    NO_INPUT: "Não foi possível obter a entrada do LLM.",
    NO_OUTPUT: "Não foi possível obter a saída do LLM.",
    NO_PERMISSION_TO_RESEARCH: "Usuário não autorizou a pesquisa.",
    NO_PERMISSION_TO_EXECUTE: "Usuário não autorizou a execução.",
    NO_PERMISSION_TO_WEB_SEARCH: "Usuário não autorizou a busca na internet.",
    WRONG_INPUT: (validInputs: string[]) => {
        return `Entrada inválida. As opções válidas são: ${validInputs.join(', ')}.`;
    },
    FAIL_TO_PARSE: "Falha ao analisar a saída do modelo.",
    NOT_SUPPORT_BIND_TOOLS: "Este modelo não suporta vinculação de ferramentas via bindTools, utilizando métodos manuais.",
    NO_ACCESS_TO_PAGE: (url: string) => `Não consegui acessar a página ${url}.`,
    NO_ACCESS_TO_DB: (table: string, database:string) => `Não consegui acessar a tabela ${table} do banco de dados ${database}.`,
}