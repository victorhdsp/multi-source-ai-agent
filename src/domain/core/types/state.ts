import z from "zod";
import { QuestionAgent } from "../../question/models";

export const multiAgentState = z.object({
	input: z.string(),
	llMOutput: QuestionAgent.optional(),
	searchedSources: z.array(z.string()),
	error: z.string().optional(),
});
