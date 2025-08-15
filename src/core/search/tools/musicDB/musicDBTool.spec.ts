import { describe, test, expect } from "bun:test";
import { Database } from "bun:sqlite";
import { SelectMusicDBService } from "./selectMusicDBService"
import { SQL_DATABASE_PATH } from "@/src/config";
import { GetMusicDBTool } from "./tool";
import { docFindDBMusicTool } from './doc';

describe("SelectMusicDBService.selectFromDatabase", () => {
    const db = new Database(`${SQL_DATABASE_PATH}/music.db`);

    const service = new SelectMusicDBService(db);
    const tool = new GetMusicDBTool(docFindDBMusicTool, service);

    test("should return music data", async () => {
        const params = {
            table: "Album",
            filters: "ArtistId = '1'",
            columns: ["Title", "ArtistId"]
        };

        const expected = "Consulta ao banco de dados: music.db\nTabelas: Album\nFiltros: ArtistId = '1'\nColunas: Title, ArtistId\nResultado:\n  - \"Title\": \"For Those About To Rock We Salute You\", \"ArtistId\": \"1\"\n  - \"Title\": \"Let There Be Rock\", \"ArtistId\": \"1\"";

        const response = await tool.execute(params);

        expect(response).toBeDefined();
        expect(response).toContain(expected);
    });

    test("should handle empty result", async () => {
        const params = {
            table: "Album",
            filters: "ArtistId = '999'",
            columns: ["Title", "ArtistId"]
        };

        const expected = "Consulta ao banco de dados: music.db\nTabelas: Album\nFiltros: ArtistId = '999'\nColunas: Title, ArtistId\nResultado:\n"

        const response = await tool.execute(params);

        expect(response).toBeDefined();
        expect(response).toContain(expected);
    });

    test("should handle invalid table", async () => {
        const params = {
            table: "InvalidTable",
            filters: "ArtistId = '1'",
            columns: ["Title", "ArtistId"]
        };

        const response = tool.execute(params);
        expect(response).rejects.toThrow("no such table: InvalidTable");
    });
});