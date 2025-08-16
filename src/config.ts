import path from 'path';

export const SQL_DATABASE_PATH = path.join(__dirname, '../data/sqlite');
export const VECTOR_DATABASE_PATH = path.join(__dirname, 'infra/database/vector');

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
}