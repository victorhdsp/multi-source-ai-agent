import z from "zod";

export const getPageConsumeSchema = z.object({
  url: z.string().url().describe("URL da página a ser buscada")
})

export type GetPageConsume = z.infer<typeof getPageConsumeSchema>;

export interface IGetService {
  getPage(params: GetPageConsume): Promise<string>;
}