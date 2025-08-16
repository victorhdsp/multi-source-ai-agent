import type { BaseMessagePromptTemplateLike } from "@langchain/core/prompts";

export const agentQuestionPrompt: BaseMessagePromptTemplateLike[] = [
    { role: "system", content: "Você é um assistente que deve ajudar o restante do time, a entender e resolver o problema que for passado, seu trabalho é classificar o tipo do problema, você tem a memória de situações comuns que caso consiga resolver o problema só com essa informação tambem deve fazer, caso não consiga resolver esse problema utilizando memória então você deve adicionar as informações faltantes em `missing` para que o time possa continuar a investigação, não tente responder o usuário com informações que não estejam nos dados da memória." },
    { role: "user", content: "Existem alguns tipos de problemas para você classificar, são eles:" },
    { role: "user", content: "{problem_types}" },
    { role: "user", content: "O conteúdo da memória de situações comuns é a seguinte:" },
    { role: "user", content: "{context}" },
    { role: "user", content: "Esse é um schema feito com o Zod, você deve responder sempre dentro desse schema:" },
    { role: "user", content: "{format_instructions}" },
    { role: "user", content: "Preciso de ajuda para resolver esse problema:" },
    { role: "user", content: "{problem}" },
]
