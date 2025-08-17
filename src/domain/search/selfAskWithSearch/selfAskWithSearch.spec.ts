import { describe, test, expect } from "bun:test"
import { SelfAskWithSearchStrategy } from "./strategy";
import { mockAgentLLMService } from "@/__test__/mock/agentLLMService";
import type { SearchAgentDTO } from "./types/dto";
import { ToolBoxService } from "../tool/service";
import { UseQuestionTool } from "../tool/question/useQuestion";
import { UseCurlTool } from "../tool/curl/useCurl";
import { GetPageService } from "../tool/curl/getPageService";
import { FakeEmbeddings } from "langchain/embeddings/fake";
import { EmbeddingService } from '../../../infra/gateway/embedding.service';
import { UseSQLiteTool } from "../tool/sqlite/useSQLite";
import { FindDBService } from "../tool/sqlite/findDBService";

const embedding = new FakeEmbeddings();
const embeddingService = new EmbeddingService(embedding);
const findDBService = new FindDBService();
const useCurlService = new GetPageService();
const useQuestionTool = new UseQuestionTool();
const useSQLiteTool = new UseSQLiteTool(findDBService, embeddingService);
const useCurlTool = new UseCurlTool(useCurlService, embeddingService);
const toolBox = new ToolBoxService(useCurlTool, useSQLiteTool, useQuestionTool);

describe("SelfAskWithSearch.callNode", () => {
    const data: SearchAgentDTO = {
        userInput: "What is the capital of France?",
        permissions: new Set([]),
        searchedSources: [],
        history: [],
        llMOutput: {
            step: "ANALYZE",
            content: "Não sei",
            missing: ["O que é France?"],
            type: "QUERY"
        },
        error: undefined,
        numberOfSteps: 1,
    }

    test("should return a valid response", async () => {
        const model = new mockAgentLLMService(`{
            "step": "USE_CURL",
            "content": "https://www.google.com/search?q=o+que+e+france",
            "missing": ["O que é France?"],
            "type": "QUERY"
        }`);
        const selfAskWithSearch = new SelfAskWithSearchStrategy(model, toolBox);
        const response = await selfAskWithSearch.callNode(data);

        expect(response).toBeDefined();
        expect(response.llMOutput.step).toBe("USE_CURL");
        expect(response.llMOutput.content).toBe("https://www.google.com/search?q=o+que+e+france");
        expect(response.llMOutput.missing).toEqual(["O que é France?"]);
        expect(response.llMOutput.type).toBe("QUERY");
    });

    test("should handle error response", async () => {
        const model = new mockAgentLLMService(`{
            "step": "ERROR",
            "content": "An error occurred",
            "missing": "test",
            "type": "QUERY"
        }`);
        const selfAskWithSearch = new SelfAskWithSearchStrategy(model, toolBox);

        const response = selfAskWithSearch.callNode(data);
        expect(response).rejects.toThrow("Falha ao analisar a saída do modelo.");
    });
});

describe("SelfAskWithSearch.route", () => {
    const model = new mockAgentLLMService("");
    const selfAskWithSearch = new SelfAskWithSearchStrategy(model, toolBox);
    const data: SearchAgentDTO = {
        userInput: "What is the capital of France?",
        permissions: new Set([]),
        searchedSources: [],
        history: [],
        llMOutput: {
            step: "ANALYZE",
            content: "Não sei",
            missing: ["O que é France?"],
            type: "QUERY"
        },
        error: undefined,
        numberOfSteps: 1,
    }

    test("should return the current step", async () => {
        const response = await selfAskWithSearch.route(data);

        expect(response).toBe("ANALYZE");
    });

    test("should return the no missing step", async () => {
        const newData = { ...data, llMOutput: { ...data.llMOutput, missing: [] } };
        const response = await selfAskWithSearch.route(newData);

        expect(response).toBe("STOP");
    });
})