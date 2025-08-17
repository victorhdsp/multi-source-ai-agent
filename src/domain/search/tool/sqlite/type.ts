import z from "zod";

export const useSQLiteConsume = z.object({
    path: z.string().describe("Caminho do banco de dados SQLite"),
    table: z.string().describe("Nome da tabela para buscar"),
    columns: z.array(z.string()).optional().describe("Colunas a serem retornadas"),
    filters: z.string().optional().describe("Condições WHERE para filtrar resultados")
})

export type UseSQLiteConsume = z.infer<typeof useSQLiteConsume>;