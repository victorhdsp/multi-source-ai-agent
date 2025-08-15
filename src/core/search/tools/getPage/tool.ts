import { tool } from "@langchain/core/tools";
import type { DocumentTool, ITool } from "../type";
import type { GetPageConsume, IGetService } from "./type";

export class GetPageTool {
  public current;

  constructor(
    private readonly document: DocumentTool,
    private readonly service: IGetService
  ) { 

    this.current = tool(
      async (params: GetPageConsume) => {
        try {
          return await this.execute(params);
        } catch (err) {
          const error = err as Error;
          throw new Error(error.message || "Failed to get page content");
        }
      },
      this.document
    );
  }

  async execute(params: GetPageConsume): Promise<string> {
    const response = await this.service.getPage(params);
    return response;
  }
}