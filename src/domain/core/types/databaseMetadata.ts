import z from "zod";

export const databaseMetadataSchema = z.object({
  database: z.string(),
  tables: z.record(
    z.object({
      columns: z.record(z.object({
        sample_values: z.array(z.any()),
        description: z.string()
      })),
      table_description: z.string()
    })
  )
});

export type MetadataSchema = z.infer<typeof databaseMetadataSchema>;