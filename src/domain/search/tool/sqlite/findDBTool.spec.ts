import { describe, test, expect } from "bun:test";
import { FindDBService } from "./findDBService"
import { SQL_DATABASE_PATH } from "@/src/config";
import { FakeEmbeddings } from "langchain/embeddings/fake";
import { EmbeddingService } from "@/src/infra/gateway/embedding.service";
import { UseSQLiteTool } from "./useSQLite";


describe("FindMusicDBService.selectFromDatabase", async () => {
    const embedding = new FakeEmbeddings();
    const embeddingService = new EmbeddingService(embedding);

    const service = new FindDBService();
    const tool = new UseSQLiteTool(service, embeddingService);

    test("should return music data", async () => {
        const params = {
            path: `${SQL_DATABASE_PATH}/music.db`,
            table: "Album",
            filters: "ArtistId = '1'",
            columns: ["Title", "ArtistId"]
        };

        const expected = "Consulta ao banco de dados: \n\t/home/victor/projetos/cases/skip/data/sqlite/music.db\nTabelas: Album\nFiltros: ArtistId = '1'\nColunas: Title, ArtistId\nResultado:\n  - \"Title\": \"For Those About To Rock We Salute You\", \"ArtistId\": \"1\"\n  - \"Title\": \"Let There Be Rock\", \"ArtistId\": \"1\""
;

        const response = await tool.execute(params);

        expect(response).toBeDefined();
        expect(response).toContain(expected);
    });

    test("should handle empty result", async () => {
        const params = {
            path: `${SQL_DATABASE_PATH}/music.db`,
            table: "Album",
            filters: "ArtistId = '999'",
            columns: ["Title", "ArtistId"]
        };

        const expected = "Consulta ao banco de dados: \n\t/home/victor/projetos/cases/skip/data/sqlite/music.db\nTabelas: Album\nFiltros: ArtistId = '999'\nColunas: Title, ArtistId\nResultado:\n"


        const response = await tool.execute(params);

        expect(response).toBeDefined();
        expect(response).toContain(expected);
    });

    test("should handle invalid table", async () => {
        const params = {
            path: `${SQL_DATABASE_PATH}/music.db`,
            table: "InvalidTable",
            filters: "ArtistId = '1'",
            columns: ["Title", "ArtistId"]
        };

        const expected = "Consulta ao banco de dados: \n\t/home/victor/projetos/cases/skip/data/sqlite/music.db\nTabelas: InvalidTable\nFiltros: ArtistId = '1'\nColunas: Title, ArtistId\nResultado:\n";
        const response = await tool.execute(params);

        expect(response).toBeDefined();
        expect(response).toContain(expected);
    });
});