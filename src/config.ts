import path from 'path';

export const VECTOR_DATABASE_PATH = path.join(__dirname, 'infra/database/vector');

export const ERROR_TYPE = {
    PARSER: "ParserError",
    LLM: "LLMError"
}