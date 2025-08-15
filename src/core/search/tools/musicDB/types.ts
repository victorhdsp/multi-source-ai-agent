import z from "zod";

export const databaseConsumeSchema = z.object({
  table: z.string().describe("Nome da tabela para buscar"),
  columns: z.array(z.string()).optional().describe("Colunas a serem retornadas"),
  filters: z.string().optional().describe("Condições WHERE para filtrar resultados"),
})

export type DatabaseConsume = z.infer<typeof databaseConsumeSchema>;

export interface ISelectDatabaseService {
  selectFromDatabase(params: DatabaseConsume): Promise<Record<string, unknown>[]>;
}
