import type { DynamicStructuredTool } from "@langchain/core/tools";
import type z from "zod";
import type { SearchAgentDTO } from "../selfAskWithSearch/types/dto";

export interface IDocumentTool{
    name: string;
    description: string;
    schema: z.ZodObject<any, any, any>;
}

export interface ITool<TypeToolConsume, TypeToolTraitment> {
    getDoc(): Promise<IDocumentTool>;
    getTool(): Promise<DynamicStructuredTool>;
    execute(params: TypeToolConsume): Promise<string>;
    traitResult(params: TypeToolTraitment): Promise<string>;
    useNode(state: SearchAgentDTO): Promise<SearchAgentDTO>;
}
