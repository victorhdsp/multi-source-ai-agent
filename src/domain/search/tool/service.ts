import type { DynamicStructuredTool } from "@langchain/core/tools";
import type { SearchAgentDTO } from "../selfAskWithSearch/types/dto";
import type { UseCurlTool } from "./curl/useCurl";
import type { UseSQLiteTool } from "./sqlite/useSQLite";

export class ToolBoxService {
    constructor(
        private readonly useCurlTool: UseCurlTool,
        private readonly useSQLiteTool: UseSQLiteTool
    ) { }

    async getAllTools(): Promise<DynamicStructuredTool[]> {
        const rawTools = [
            this.useCurlTool,
            this.useSQLiteTool
        ].map(tool => tool.getTool());

        const tools = await Promise.all(rawTools);
        return tools;
    }

    async useCUrl(state: SearchAgentDTO): Promise<SearchAgentDTO> {
        return await this.useCurlTool.useNode(state);
    }

    async useSQLite(state: SearchAgentDTO): Promise<SearchAgentDTO> {
        return await this.useSQLiteTool.useNode(state);
    }
}