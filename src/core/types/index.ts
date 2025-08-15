
import z from "zod";
import { QuestionAgent, type QuestionAgentDTO } from "../models/llmAgentResponse.dto";
import type { AgentSourceType } from "./source";

export interface MultiAgentDTO {
	input: string,
	llMOutput: QuestionAgentDTO,
	searchedSources: AgentSourceType[],
	error?: string
}

export const multiAgentState = z.object({
	userInput: z.string(),
	llMOutput: QuestionAgent.optional(),
	searchedSources: z.array(z.string()),
	error: z.string().optional(),
});
