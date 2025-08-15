import type { AgentSourceType } from "../../../types/source";
import type { QuestionAgentDTO } from "../../../question/questionAgent.dto";
import type { SearchAgentStep } from "./steps";
import type { SearchAgentPermissions } from "./permissions";

export interface SelfAskDTO extends QuestionAgentDTO {
	step: SearchAgentStep,
}

export interface SearchAgentDTO {
	userInput: string,
	permissions: Set<SearchAgentPermissions>,
	searchedSources: AgentSourceType[],
	history: string[],
	llMOutput: SelfAskDTO,
	error?: string
	numberOfSteps: number,
}
