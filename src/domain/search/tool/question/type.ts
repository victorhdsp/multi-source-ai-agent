import z from "zod";

export const useQuestionConsume = z.object({
  message: z.string().describe("Mensagem da pergunta")
})

export type UseQuestionConsume = z.infer<typeof useQuestionConsume>;