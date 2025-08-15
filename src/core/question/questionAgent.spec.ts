import { expect, test, describe } from "bun:test";
import { QuestionAgentUsecase } from "./usecase";
import { QuestionAgentStrategy } from "./strategy";
import { mockAgentLLMService } from "@/__test__/mock/agentLLMService";
import { MockVectorStore } from "@/__test__/mock/vectorStore";
import { MULTI_AGENT_STEPS } from "../types/steps";
import type { MultiAgentDTO } from "../types/dto";


describe("QuestionAgentUsecase.callNode", () => {
  const data: MultiAgentDTO = {
    input: "What is the capital of France?",
    llMOutput: {
      content: "",
      missing: [],
      type: "QUERY"
    },
    searchedSources: []
  }

  test("QuestionAgentUsecase should return a response", async () => {
    const vectorDB = new MockVectorStore();
    const model = new mockAgentLLMService(`{
    "content": "The capital of France is Paris.",
    "missing": [],
    "type": "QUERY"
  }`);

    const strategy = new QuestionAgentStrategy(model);
    const agent = new QuestionAgentUsecase(vectorDB, strategy);

    const response = await agent.callNode(data);

    expect(response).toBeDefined();
    expect(response.error).toBeUndefined();
    expect(response.input).toEqual("What is the capital of France?");
    expect(response.llMOutput.content).toBe("The capital of France is Paris.");
    expect(response.llMOutput.missing).toEqual([]);
    expect(response.llMOutput.type).toBe("QUERY");
    expect(response.searchedSources).toEqual(["DOCUMENT"]);
  });

  test("QuestionAgentUsecase should handle missing data", async () => {
    const vectorDB = new MockVectorStore();
    const model = new mockAgentLLMService(`{
    "content": "",
    "missing": ["Some data"],
    "type": "QUERY"
  }`);

    const strategy = new QuestionAgentStrategy(model);
    const agent = new QuestionAgentUsecase(vectorDB, strategy);

    const response = await agent.callNode(data);

    expect(response).toBeDefined();
    expect(response.error).toBeUndefined();
    expect(response.llMOutput.content).toBe("");
    expect(response.llMOutput.missing).toEqual(["Some data"]);
    expect(response.llMOutput.type).toBe("QUERY");
    expect(response.input).toEqual("What is the capital of France?");
    expect(response.searchedSources).toEqual(["DOCUMENT"]);
  });

  test("QuestionAgentUsecase should handle error", async () => {
    const vectorDB = new MockVectorStore();
    const model = new mockAgentLLMService(`{
    "content": "",
    "missing": [],
    "type": "wrong_type"
  }`);

    const strategy = new QuestionAgentStrategy(model);
    const agent = new QuestionAgentUsecase(vectorDB, strategy);

    const response = agent.callNode(data);

    expect(response).rejects.toThrowError("Falha ao analisar a saída do modelo.");
  });
})

describe("QuestionAgentUsecase.route with empty input", () => {
  const data: MultiAgentDTO = {
    input: "What is the capital of France?",
    llMOutput: {
      content: "Isso é um teste",
      missing: [],
      type: "QUERY"
    },
    searchedSources: []
  }
  
  const vectorDB = new MockVectorStore();
  const model = new mockAgentLLMService("");

  const strategy = new QuestionAgentStrategy(model);
  const agent = new QuestionAgentUsecase(vectorDB, strategy);

  test("should return execute step", async () => {
    const response = await agent.route(data);

    expect(response).toBeDefined();
    expect(response).toBe(MULTI_AGENT_STEPS.EXECUTE);
  });

  test("should return search step", async () => {
    const newData = { ...data, llMOutput: { ...data.llMOutput, missing: ["Some data"] } };
    const response = await agent.route(newData);

    expect(response).toBeDefined();
    expect(response).toBe(MULTI_AGENT_STEPS.SEARCH);
  });
  
  test("should return execute step", async () => {
    const newData = { ...data, error: "Some error" };
    const response = await agent.route(newData);

    expect(response).toBeDefined();
    expect(response).toBe(MULTI_AGENT_STEPS.ERROR);
  });
});