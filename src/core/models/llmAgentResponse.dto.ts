import z from "zod";

export type LlmAgentResponseType = "task" | "answer";

export interface LlmAgentResponseDTO {
	type: LlmAgentResponseType,
	content: string,
	missing: string[]
}

export const LlmAgentResponse = z.object({
	type: z.string().refine((val) => ["task", "answer"].includes(val)),
	content: z.string(),
	missing: z.array(z.string())
});