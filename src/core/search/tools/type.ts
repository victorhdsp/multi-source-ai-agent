import type { DynamicStructuredTool, DynamicTool } from "@langchain/core/tools";
import type z from "zod";

export interface DocumentTool {
  name: string;
  description: string;
  schema: z.ZodSchema;
}

export interface ITool {
  current: (DynamicTool | DynamicStructuredTool);
}

export type resolveToolType = DynamicTool | DynamicStructuredTool