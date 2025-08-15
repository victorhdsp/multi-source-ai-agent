import z from "zod";
import { LlmAgentResponse, type LlmAgentResponseDTO } from "./models/llmAgentResponse.dto";
import { toolAction } from "../tools/tools";
import type { LlmAgentResponseType } from "./models/llmAgentResponse.dto";

export const TypeResponseStrategy: Record<LlmAgentResponseType, string> = {
	"answer": "Fornecer uma resposta direta à pergunta.",
	"task": "Construir o passos necessários para resolver o problema.",
}

export const MULTI_AGENT_SOURCE_VARS = {
	DOCUMENT: "DOCUMENT"
};

export const MULTI_AGENT_STEPS = {
	STOP: "STOP",
	ERROR: "ERROR",
	SEARCH: "SEARCH",
	EXECUTE: "EXECUTE"
};

export type MultiAgentSourceType = keyof typeof MULTI_AGENT_SOURCE_VARS | string;

export interface MultiAgentDTO {
	input: string,
	llMOutput: LlmAgentResponseDTO,
	searchedSources: SearchAgentSourceType[],
	error?: string
}

export const multiAgentState = z.object({
	userInput: z.string(),
	llMInput: z.string().optional(),
	llMOutput: LlmAgentResponse.optional(),
	searchedSources: z.array(z.string()),
	error: z.string().optional(),
	permissions: z.set(z.enum(Object.values(SEARCH_AGENT_SOURCE_PERMISSIONS))),
	step: z.enum(Object.values(SEARCH_AGENT_STEPS)),
});
