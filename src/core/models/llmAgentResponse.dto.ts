import z from "zod";
import type { AnswerType } from "../types/answerType";

export interface QuestionAgentDTO {
	type: AnswerType,
	content: string,
	missing: string[]
}

export const QuestionAgent = z.object({
	type: z.string().refine((val) => ["task", "answer"].includes(val)),
	content: z.string(),
	missing: z.array(z.string())
});