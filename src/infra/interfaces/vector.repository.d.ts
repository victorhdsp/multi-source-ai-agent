export interface VectorDocument {
    pageContent: string;
    metadata: Record<string, any>;
    id?: string;
}

export interface VectorSearchResult extends VectorDocument {
    similarity: number;
}

export interface IVectorStore {
  addDocuments(documents: VectorDocument[]): Promise<void>;
  similaritySearch(query: string, k: number): Promise<VectorSearchResult[]>;
  save(path: string): Promise<void>;
  load(path: string): Promise<boolean>;
  create(path: string): Promise<void>;
}