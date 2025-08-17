import z from "zod";
import { QuestionAgent } from "@/src/domain/question/models";

export const selfAskState = z.object({
	step: z.string(),
	...QuestionAgent.shape,
})

export const searchAgentState = z.object({
	userInput: z.string(),
	permissions: z.set(z.string()),
	searchedSources: z.array(z.string()),
	history: z.array(z.string()).default([]),
	llMOutput: selfAskState,
	error: z.string().optional(),
	numberOfSteps: z.number().default(0),
});

export type SearchAgentState = typeof searchAgentState;
export type _SearchAgentDTO = z.infer<typeof searchAgentState>;