import type { QuestionAgentDTO } from "../question/questionAgent.dto";
import type { AgentSourceType } from "./source";

export interface MultiAgentDTO {
	input: string,
	llMOutput: QuestionAgentDTO,
	searchedSources: AgentSourceType[],
	error?: string
}
