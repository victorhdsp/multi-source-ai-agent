import { expect, test } from "bun:test"
import { LlmAgentResponseParser } from "./LlmAgentResponse.parser";
import type { LlmAgentResponseDTO } from "../../models/llmAgentResponse.dto";

test("LlmAgentResponseParser should parse response correctly", async () => {
    const parser = new LlmAgentResponseParser();

    const validResponse = JSON.stringify({
        type: "answer",
        content: "This is a test response.",
        missing: ["info1", "info2"]
    } as LlmAgentResponseDTO);

    const parsedResponse = await parser.parse(validResponse);
    expect(parsedResponse).toEqual({
        type: "answer",
        content: "This is a test response.",
        missing: ["info1", "info2"]
    });
});

test("LlmAgentResponseParser should throw error for invalid response", async () => {
    const parser = new LlmAgentResponseParser();

    const invalidResponse = JSON.stringify({
        type: "answer",
        content: "This is a test response."
        // missing field is omitted
    });

    await expect(parser.parse(invalidResponse)).rejects.toThrow("Formato inválido para LlmAgentResponseDTO");
});

test("LlmAgentResponseParser should throw error for invalid type", async () => {
    const parser = new LlmAgentResponseParser();

    const invalidResponse = JSON.stringify({
        type: "invalid_type", // Invalid type
        content: "This is a test response.",
        missing: ["info1", "info2"]
    });

    await expect(parser.parse(invalidResponse)).rejects.toThrow("Erro no parse: Error: Tipo inválido para LlmAgentResponseDTO");
});

test("LlmAgentResponseParser should handle parse errors", async () => {
    const parser = new LlmAgentResponseParser();

    const malformedResponse = "This is not a valid JSON string";

    await expect(parser.parse(malformedResponse)).rejects.toThrow("Erro no parse: SyntaxError: JSON Parse error: Unexpected identifier \"This\"");
});

test("LlmAgentResponseParser should return format instructions", () => {
    const parser = new LlmAgentResponseParser();
    const instructions = parser.getFormatInstructions();
    
    expect(instructions).toBe(`Responda APENAS em JSON com o seguinte formato:
{
  "type": "question" | "task",
  "content": "string com a resposta",
  "missing": ["lista", "de", "informações", "faltantes"]
}`);
});
