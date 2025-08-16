import type { Embeddings } from "@langchain/core/embeddings";
import { RecursiveCharacterTextSplitter } from "langchain/text_splitter";
import { MemoryVectorStore } from "langchain/vectorstores/memory";

export interface EmbeddingSettings {
    topK?: number;
    chunkSize?: number;
    chunkOverlap?: number;
}

export class EmbeddingService {
    constructor(
        private readonly embeddingModel: Embeddings
    ) {}

    async getSimilar(name: string, content: string, question: string, settings: EmbeddingSettings = {}): Promise<string[]> {
        try {
            const splitter = new RecursiveCharacterTextSplitter({
                chunkSize: settings.chunkSize || 500,
                chunkOverlap: settings.chunkOverlap || 50,
            });
            const docs = await splitter.createDocuments([content]);
            const vectorStore = await MemoryVectorStore.fromDocuments(docs, this.embeddingModel);
            const results = await vectorStore.similaritySearch(question, settings.topK || 5);
            return results.map(doc => doc.pageContent);
        } catch (err) {
            const error = err as Error;
            throw new Error(`Failed to get similar content: ${error.message}`);
        }
    }
}