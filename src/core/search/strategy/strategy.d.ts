import z from "zod";
import { LlmAgentResponse, type LlmAgentResponseDTO } from "../../models/llmAgentResponse.dto";
import type { MULTI_AGENT_SOURCE_VARS } from "../../strategy";
import type { searchAgentTools } from "../tools/service";
import type { docFindDBMusicTool } from "../tools/getMusicDB.tool";
import type { docPageTool } from "../tools/getPage.tool";

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
export type SearchAgentSourceType = keyof typeof MULTI_AGENT_SOURCE_VARS | string;

export interface LLMSearchResponseDTO extends LlmAgentResponseDTO {
	step: SearchAgentSourceStep,
}

export const llmSearchResponse = z.object({
	step: z.enum(Object.values(SEARCH_AGENT_STEPS)),
	...LlmAgentResponse.shape,
})

export interface SearchAgentDTO {
	userInput: string,
	permissions: Set<SearchAgentSourcePermissions>,
	searchedSources: SearchAgentSourceType[],
	history: string[],
	llMOutput: LLMSearchResponseDTO,
	error?: string
	numberOfSteps: number,
}

export const searchAgentState = z.object({
	userInput: z.string(),
	permissions: z.set(z.enum(Object.values(SEARCH_AGENT_SOURCE_PERMISSIONS))),
	searchedSources: z.array(z.string()),
	history: z.array(z.string()).default([]),
	llMOutput: llmSearchResponse,
	error: z.string().optional(),
	numberOfSteps: z.number().default(0),
});

export type SearchAgentState = typeof searchAgentState;
export type _SearchAgentDTO = z.infer<typeof searchAgentState>;