import { BaseOutputParser } from "@langchain/core/output_parsers";
import type { LlmAgentResponseDTO, LlmAgentResponseType } from "../../models/llmAgentResponse.dto";
import { ERROR_TYPE } from "@/src/config";

export class LlmAgentResponseParser extends BaseOutputParser<LlmAgentResponseDTO> {
  lc_namespace = ["custom", "parser"];

  async parse(text: string): Promise<LlmAgentResponseDTO> {
    try {
      const trimmedText = text.replace("```json", "").replace("```", "").trim();
      const json = JSON.parse(trimmedText) as LlmAgentResponseDTO;

      if (!json.type || !json.content || !Array.isArray(json.missing)) {
        throw new Error("Formato inválido para LlmAgentResponseDTO");
      }
      if (json.type !== "answer" && json.type !== "task") {
        throw new Error("Tipo inválido para LlmAgentResponseDTO");
      }

      return {
        type: json.type as LlmAgentResponseType,
        content: json.content,
        missing: json.missing
      };
    } catch (err) {
      throw new Error(`Erro no parse: ${err}`, { cause: ERROR_TYPE.PARSER });
    }
  }

  getFormatInstructions(): string {
    return `Responda APENAS em JSON com o seguinte formato:
{
  "type": "answer" | "task",
  "content": "string com a resposta",
  "missing": ["lista", "de", "informações", "faltantes"]
}`;
  }
}
