import z from "zod";
import { QuestionAgent } from "../models/llmAgentResponse.dto";

export const multiAgentState = z.object({
	userInput: z.string(),
	llMOutput: QuestionAgent.optional(),
	searchedSources: z.array(z.string()),
	error: z.string().optional(),
});
