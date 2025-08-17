export const SEARCH_AGENT_STEPS = {
	STOP: "STOP",
	ANALYZE: "ANALYZE",
	USE_SQL: "USE_SQL",
	USE_CURL: "USE_CURL",
	USE_QUESTION: "USE_QUESTION",
	WHATNOT: "WHATNOT",
} as const;

export type SearchAgentStep = keyof typeof SEARCH_AGENT_STEPS;
