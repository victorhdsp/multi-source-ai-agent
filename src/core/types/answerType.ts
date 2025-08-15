export const ANSWER_TYPE = {
	QUERY: "QUERY",
	TASK: "TASK",
}

export type AnswerType = keyof typeof ANSWER_TYPE;

export const AnswerTypeStrategy: Record<AnswerType, string> = {
	"QUERY": "Fornecer uma resposta direta à pergunta.",
	"TASK": "Construir o passos necessários para resolver o problema.",
}