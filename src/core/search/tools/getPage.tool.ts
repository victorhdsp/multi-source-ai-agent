import { tool } from "@langchain/core/tools";
import z from "zod";
import { spawn } from "child_process";

export const docPageTool = {  
  name: "get_web_page",  
  description: "Busca por uma página específica na web usando curl - GET.",  
  schema: z.object({  
    url: z.string().url().describe("URL da página a ser buscada")  
  })
};

export const getPageTool = tool(
  async ({ url }) => {
    return new Promise<string>((resolve, reject) => {
      const curl = spawn("curl", ["-X", "GET", "-s", url]);

      let output = "";
      let errorOutput = "";

      curl.stdout.on("data", (data) => {
        output += data.toString();
      });

      curl.stderr.on("data", (data) => {
        errorOutput += data.toString();
      });

      curl.on("close", (code) => {
        if (code === 0) {
          resolve(output);
        } else {
          reject(new Error(errorOutput || `Curl exited with code ${code}`));
        }
      });
    });
  },
  docPageTool
);
