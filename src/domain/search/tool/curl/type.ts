import z from "zod";

export const useCurlConsume = z.object({
  url: z.string().url().describe("URL da página a ser buscada")
})

export type UseCurlConsume = z.infer<typeof useCurlConsume>;