import { DynamicStructuredTool, tool } from "@langchain/core/tools";
import type { DocumentTool, ITool } from "../type";
import type { GetPageConsume, IGetService } from "./type";
import type { GenericTool } from "../genericTool";

export class GetPageTool implements GenericTool {
  constructor(
    private readonly document: DocumentTool,
    private readonly service: IGetService
  ) { }

  async execute(params: GetPageConsume): Promise<string> {
    const response = await this.service.getPage(params);
    return response;
  }

  invoke() {
    return tool(
      async (params: GetPageConsume) => {
        try {
          return this.execute(params);
        } catch (err) {
          const error = err as Error;
          throw new Error(error.message || "Failed to get page content");
        }
      },
      this.document
    );
  }
}