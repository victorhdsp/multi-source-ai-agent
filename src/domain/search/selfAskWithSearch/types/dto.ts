import type { AgentSourceType } from "@/src/domain/core/types/source";
import type { QuestionAgentDTO } from "@/src/domain/question/models";
import type { SearchAgentStep } from "@/src/domain/search/selfAskWithSearch/types/steps";
import type { SearchAgentPermissions } from "@/src/domain/search/selfAskWithSearch/types/permissions";

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
