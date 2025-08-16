
import { ChatPromptTemplate, type BaseMessagePromptTemplateLike } from "@langchain/core/prompts";
import { toJsonSchema } from "@langchain/core/utils/json_schema";
import type { IAgentLLMService } from "@/src/infra/interfaces/agentLlm.gateway";
import { QuestionAgent, type QuestionAgentDTO } from "@/src/domain/question/models";
import { ERROR_MESSAGE } from "@/src/config";
import { AnswerTypeDescription } from "@/src/domain/core/types/answerType";


export class QuestionAgentStrategy {
    private agentQuestionPrompt: BaseMessagePromptTemplateLike[] = [
        { role: "system", content: "Você é um assistente que deve ajudar o restante do time, a entender e resolver o problema que for passado, seu trabalho é classificar o tipo do problema, você tem a memória de situações comuns que caso consiga resolver o problema só com essa informação tambem deve fazer, caso não consiga resolver esse problema utilizando memória então você deve adicionar as informações faltantes em `missing` para que o time possa continuar a investigação." },
        { role: "user", content: "Existem alguns tipos de problemas para você classificar, são eles:"  },
        { role: "user", content: "{problem_types}" },
        { role: "user", content: "O conteúdo da memória de situações comuns é a seguinte:" },
        { role: "user", content: "{context}" },
        { role: "user", content: "Esse é um schema feito com o Zod, você deve responder sempre dentro desse schema:" },
        { role: "user", content: "{format_instructions}" },
        { role: "user", content: "Preciso de ajuda para resolver esse problema:" },
        { role: "user", content: "{problem}" },
    ]

    constructor(
        private readonly model: IAgentLLMService,
    ) {}

    async formatQuestionPrompt(question: string, content: string): Promise<ChatPromptTemplate> {
        const format = (toJsonSchema(QuestionAgent) as any).properties;
        
        const problemTypes: string[] = Object.entries(AnswerTypeDescription).map(([key, value]) => {
            return `"${key}" - "${value}"`;
        });
        
        const prompt = await ChatPromptTemplate.fromMessages(
            this.agentQuestionPrompt
        )
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
        const rawContent = output.replace("```json", "").replace("```", "");
        
        try {
            const rawParsedOutput = JSON.parse(rawContent);
            const parser = QuestionAgent.parse(rawParsedOutput) as QuestionAgentDTO;
            return parser;
        } catch (error: any) {
            throw new Error(ERROR_MESSAGE.FAIL_TO_PARSE);
        }
    }
}