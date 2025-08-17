import type { DynamicStructuredTool } from "@langchain/core/tools";
import type { SearchAgentDTO } from "../selfAskWithSearch/types/dto";
import type { UseCurlTool } from "./curl/useCurl";
import type { UseSQLiteTool } from "./sqlite/useSQLite";
import type { UseQuestionTool } from "./question/useQuestion";

export class ToolBoxService {
    private boundTools: Record<string, (state: SearchAgentDTO) => Promise<SearchAgentDTO>>;

    constructor(
        private readonly useCurlTool: UseCurlTool,
        private readonly useSQLiteTool: UseSQLiteTool,
        private readonly useQuestionTool: UseQuestionTool,
    ) {
        this.boundTools = {
            useCurl: this.useCurlTool.useNode.bind(this.useCurlTool),
            useSQLite: this.useSQLiteTool.useNode.bind(this.useSQLiteTool),
            useQuestion: this.useQuestionTool.useNode.bind(this.useQuestionTool),
        } as const;
    }

    async getAllTools(): Promise<DynamicStructuredTool[]> {
        const rawTools = [
            this.useCurlTool,
            this.useSQLiteTool,
            this.useQuestionTool,
        ].map(tool => tool.getTool());

        const tools = await Promise.all(rawTools);
        return tools;
    }

    async useTool(state: SearchAgentDTO, name: string): Promise<SearchAgentDTO> {
        const toolFunction = this.boundTools[name];
        if (!toolFunction) throw new Error(`Tool ${name} not found`);
        return await toolFunction(state);
    }
}