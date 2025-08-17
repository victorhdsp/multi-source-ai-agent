
import { ChatPromptTemplate, type BaseMessagePromptTemplateLike } from "@langchain/core/prompts";
import { toJsonSchema } from "@langchain/core/utils/json_schema";
import type { IAgentLLMService } from "@/src/infra/interfaces/agentLlm.gateway";
import { QuestionAgent, type QuestionAgentDTO } from "@/src/domain/question/models";
import { ERROR_MESSAGE } from "@/src/config";
import { AnswerTypeDescription } from "@/src/domain/core/types/answerType";
import { agentQuestionPrompt } from "./prompt";
import { safeJsonParse } from "@/src/utils/safeParser";
import { secureExec } from "@/src/utils/secureExec";


export class QuestionAgentStrategy {
    constructor(
        private readonly model: IAgentLLMService,
    ) {}

    async formatQuestionPrompt(question: string, content: string): Promise<ChatPromptTemplate> {
        const format = (toJsonSchema(QuestionAgent) as any).properties;
        
        const problemTypes: string[] = Object.entries(AnswerTypeDescription).map(([key, value]) => {
            return `"${key}" - "${value}"`;
        });
        
        const prompt = await ChatPromptTemplate.fromMessages([
            ...agentQuestionPrompt
        ])
        .partial({
            problem_types: problemTypes.join(", "),
            context: content,
            format_instructions: JSON.stringify(format),
            problem: question,
        });
        
        return prompt;
    }

    async sendToModel(prompt: ChatPromptTemplate): Promise<string> {
        const chain = prompt.pipe(this.model);
        const chainResult = await chain.invoke({});
        return chainResult.text;
    }

    async parseOutput(output: string): Promise<QuestionAgentDTO> {
        return secureExec(() => {
            const rawParsedOutput = safeJsonParse<QuestionAgentDTO>(output);
            return QuestionAgent.parse(rawParsedOutput) as QuestionAgentDTO;
        }, ERROR_MESSAGE.FAIL_TO_PARSE);
    }
}