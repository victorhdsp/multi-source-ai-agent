import z from "zod";
import type { DocumentTool } from "@/src/domain/search/tools/type";
import { getPageConsumeSchema } from "@/src/domain/search/tools/getPage/type";
import { SEARCH_AGENT_STEPS } from "@/src/domain/search/selfAskWithSearch/types/steps";

export const docPageTool: DocumentTool = {  
  name: SEARCH_AGENT_STEPS.GET_PAGE,
  description: "Busca por uma página específica na web usando curl a partir de uma URL.",  
  schema: getPageConsumeSchema
};

