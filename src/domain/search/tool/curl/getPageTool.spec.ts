import { describe, expect, test } from "bun:test"

import { GetPageService } from "./getPageService";
import { UseCurlTool } from "./useCurl";
import { FakeEmbeddings } from "langchain/embeddings/fake";
import { EmbeddingService } from "@/src/infra/gateway/embedding.service";

describe("GetPageService.getPage", () => {
    const embedding = new FakeEmbeddings();
    const embeddingService = new EmbeddingService(embedding);
    const service = new GetPageService();

    const tool = new UseCurlTool(service, embeddingService);

    test("should return page content", async () => {
        const url = "https://www.example.com";

        const response = await tool.execute({url});

        expect(response).toBeDefined();
        expect(typeof response).toBe("string");
        expect(response).toMatch(/<title>(.*?)<\/title>/);
        expect(response.length).toBeGreaterThan(0);
    });

    test("should throw error for invalid URL", async () => {
        const url = "invalid-url";

        const response = await tool.execute({url});

        expect(response).toBeDefined();
        expect(typeof response).toBe("string");
        expect(response).toBe("Não consegui acessar a página invalid-url.");
        expect(response.length).toBeGreaterThan(0);
    });
});