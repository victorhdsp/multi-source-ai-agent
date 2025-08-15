import z from "zod";
import type { DocumentTool } from "../type";

export const docPageTool: DocumentTool = {  
  name: "get_web_page",  
  description: "Busca por uma página específica na web usando curl - GET.",  
  schema: z.object({  
    url: z.string().url().describe("URL da página a ser buscada")  
  })
};

