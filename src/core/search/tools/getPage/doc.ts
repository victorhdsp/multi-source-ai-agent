import z from "zod";
import type { DocumentTool } from "../type";
import { getPageConsumeSchema } from "./type";

export const docPageTool: DocumentTool = {  
  name: "get_web_page",  
  description: "Busca por uma página específica na web usando curl - GET.",  
  schema: getPageConsumeSchema
};

