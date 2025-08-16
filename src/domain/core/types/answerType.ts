export const ANSWER_TYPE = {
	QUERY: "QUERY",
	TASK: "TASK",
}

export type AnswerType = keyof typeof ANSWER_TYPE;

export const AnswerTypeDescription: Record<AnswerType, string> = {
	QUERY: "Uma resposta direta onde não é necessário modificar nada para chegar ao resultado.",
	TASK: "Uma tarefa onde é necessário construir um passo a passo para chegar ao resultado.",
}

export const AnswerTypeStrategy: Record<AnswerType, string> = {
	QUERY: "Fornecer uma resposta direta à pergunta.",
	TASK: "Construir o passos necessários para resolver o problema.",
}