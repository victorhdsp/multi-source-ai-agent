import { describe, expect, test } from "bun:test"

import { GetPageService } from "./getPageService";
import { GetPageTool } from "./tool";
import { docPageTool } from "./doc";

describe("GetPageService.getPage", () => {
    const service = new GetPageService();
    const tool = new GetPageTool(docPageTool, service);

    test.todo("should return page content", async () => {
        const url = "https://www.example.com";

        const response = await tool.execute({url});

        expect(response).toBeDefined();
        expect(typeof response).toBe("string");
        expect(response).toMatch(/<title>(.*?)<\/title>/);
        expect(response.length).toBeGreaterThan(0);
    });

    test("should throw error for invalid URL", async () => {
        const invalidUrl = "invalid-url";

        await expect(tool.execute({url: invalidUrl})).rejects.toThrow("Invalid URL");
    });
});