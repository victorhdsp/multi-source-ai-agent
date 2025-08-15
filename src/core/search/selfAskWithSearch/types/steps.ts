export const SEARCH_AGENT_STEPS = {
	STOP: "STOP",
	ANALYZE: "ANALYZE",
	GET_MUSIC_DB: "GET_MUSIC_DB",
	GET_PAGE: "GET_PAGE",
}

export type SearchAgentStep = keyof typeof SEARCH_AGENT_STEPS;
