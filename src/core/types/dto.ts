import type { QuestionAgentDTO } from "../models/llmAgentResponse.dto";
import type { AgentSourceType } from "./source";

export interface MultiAgentDTO {
	input: string,
	llMOutput: QuestionAgentDTO,
	searchedSources: AgentSourceType[],
	error?: string
}
