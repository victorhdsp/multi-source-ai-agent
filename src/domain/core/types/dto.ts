import type { QuestionAgentDTO } from "../../question/models";
import type { AgentSourceType } from "./source";

export interface MultiAgentDTO {
	input: string,
	llMOutput: QuestionAgentDTO,
	searchedSources: AgentSourceType[],
	error?: string
}
