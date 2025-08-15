import z from "zod";
import type { AgentSourceType } from "../../types/source";
import { QuestionAgent, type QuestionAgentDTO } from "../../models/llmAgentResponse.dto";

export const SEARCH_AGENT_PERMISSIONS = {
	INTERNET: "INTERNET"
};

export const SEARCH_AGENT_STEPS = {
	STOP: "STOP",
	ANALYZE: "ANALYZE",
	GET_MUSIC_DB: "GET_MUSIC_DB",
	GET_PAGE: "GET_PAGE",
}

export type SearchAgentSourceStep = keyof typeof SEARCH_AGENT_STEPS;
export type SearchAgentSourcePermissions = keyof typeof SEARCH_AGENT_PERMISSIONS;

export interface LLMSearchResponseDTO extends QuestionAgentDTO {
	step: SearchAgentSourceStep,
}

export const llmSearchResponse = z.object({
	step: z.string(),
	...QuestionAgent.shape,
})

export interface SearchAgentDTO {
	userInput: string,
	permissions: Set<SearchAgentSourcePermissions>,
	searchedSources: AgentSourceType[],
	history: string[],
	llMOutput: LLMSearchResponseDTO,
	error?: string
	numberOfSteps: number,
}

export const searchAgentState = z.object({
	userInput: z.string(),
	permissions: z.set(z.string()),
	searchedSources: z.array(z.string()),
	history: z.array(z.string()).default([]),
	llMOutput: llmSearchResponse,
	error: z.string().optional(),
	numberOfSteps: z.number().default(0),
});

export type SearchAgentState = typeof searchAgentState;
export type _SearchAgentDTO = z.infer<typeof searchAgentState>;