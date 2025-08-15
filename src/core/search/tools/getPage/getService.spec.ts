import { describe, expect, test } from "bun:test"

import { GetPageService } from "./getService";

describe("GetPageService.getPage", () => {
    test.todo("should return page content", async () => {
        const service = new GetPageService();
        const url = "https://www.example.com";

        const response = await service.getPage(url);

        expect(response).toBeDefined();
        expect(typeof response).toBe("string");
        expect(response).toMatch(/<title>(.*?)<\/title>/);
        expect(response.length).toBeGreaterThan(0);
    });

    test("should throw error for invalid URL", async () => {
        const service = new GetPageService();
        const invalidUrl = "invalid-url";

        await expect(service.getPage(invalidUrl)).rejects.toThrow("Invalid URL");
    });
});