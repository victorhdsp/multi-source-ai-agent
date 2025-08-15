import type { IVectorStore } from "@/src/infra/interfaces/vector.repository";

export class MockVectorStore implements IVectorStore {
    async addDocuments(): Promise<void> {}

    async create(): Promise<void> {}

    async load(): Promise<boolean> {
        return true;
    }

    async save(ng: any): Promise<void> {}

    async similaritySearch(): Promise<any[]> {
        return [];
    }
}