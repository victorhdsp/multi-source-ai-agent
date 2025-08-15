import { tool } from "@langchain/core/tools";
import type { DocumentTool } from "../type";
import type { IGetService } from "./type";

export class GetPageTool {
  constructor(
    private readonly document: DocumentTool,
    private service: IGetService
  ) { }

  invoke() {
    return tool(
      async ({ url }: { url: string }) => {
        try {
          const response = await this.service.getPage(url);
          return response;
        } catch (err) {
          const error = err as Error;
          throw new Error(error.message || `Failed to fetch page from ${url}`);
        }
      },
      this.document
    );
  }
}