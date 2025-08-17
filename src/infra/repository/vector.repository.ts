import { HNSWLib } from "@langchain/community/vectorstores/hnswlib";
import type { IVectorStore, VectorDocument, VectorSearchResult } from "@/src/infra/interfaces/vector.repository";
import type { Embeddings } from "@langchain/core/embeddings";
import { promises as fs } from "fs";
import type { SpaceName } from "hnswlib-node";
import { logger } from "@/src/tools/logger";

export class VectorStore implements IVectorStore {
    private vectorStore: HNSWLib | null = null;
    private embeddings: Embeddings;
    private NUM_DIMENSIONS = 768; 
    private SPACE: SpaceName = "cosine";
    private FILENAME = "hnswlib.index";

    constructor(embeddings: Embeddings) {
        this.embeddings = embeddings;
    }

    async addDocuments(documents: VectorDocument[]): Promise<void> {
        if (!this.vectorStore) return;

        const langchainDocs = documents.map((doc) => ({
            pageContent: doc.pageContent,
            metadata: doc.metadata
        }));
        
        await this.vectorStore.addDocuments(langchainDocs);
    }

    async similaritySearch(query: string, k: number): Promise<VectorSearchResult[]> {
        if (!this.vectorStore) return [];

        const results = await this.vectorStore.similaritySearchWithScore(query, k);
        
        return results.map(([doc, score]) => ({
            pageContent: doc.pageContent,
            metadata: doc.metadata,
            similarity: score
        }));
    }

    async save(path: string): Promise<void> {
        if (!this.vectorStore) return;

        await fs.mkdir(path, { recursive: true });
        await this.vectorStore.save(path);
    }

    async create(path: string): Promise<void> {
        try {
            await fs.access(`${path}/${this.FILENAME}`);

        } catch (error) {
            logger.info(`Saving vector store to ${path}`);
            this.vectorStore = new HNSWLib(this.embeddings, {
                space: this.SPACE,
                numDimensions: this.NUM_DIMENSIONS
            });
        }
    }

    async load(path: string): Promise<boolean> {
        try {
            await fs.access(`${path}/${this.FILENAME}`);
            this.vectorStore = await HNSWLib.load(
                path,
                this.embeddings
            );
            return true;
        } catch (error) {
            return false;
        }
    }
}