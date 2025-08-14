import z from "zod";
import { LlmAgentResponse, type LlmAgentResponseDTO } from "./llmAgentResponse.dto";

type SearchAgentSourceVars = "DOCUMENT";

export type SearchAgentSourceType = SearchAgentSourceVars | string;

export interface SearchAgentStateDTO {
	input: string,
	output: LlmAgentResponseDTO,
	sources: SearchAgentSourceType[]
}

export const searchAgentState = z.object({
	input: z.string(),
	output: LlmAgentResponse,
	sources: z.array(
		z.string().or(z.literal("DOCUMENT"))
	)
});
