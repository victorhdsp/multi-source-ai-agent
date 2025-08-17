import fs from 'fs';
import path from 'path';
import type { IVectorStore, VectorDocument } from "@/src/infra/interfaces/vector.repository";
import { PDFLoader } from '@langchain/community/document_loaders/fs/pdf';
import { DOCUMENT_METADATA_PATH, DOCUMENT_TO_EMBED_PATH, VECTOR_DATABASE_PATH } from '../config';
import { RecursiveCharacterTextSplitter } from 'langchain/text_splitter';
import { logger } from './logger';


export class DocEmbeddingService {
  constructor(
    private vectorStore: IVectorStore,
    private CHUNK_SIZE = 500,
    private CHUNK_OVERLAP = 100,
  ) { }

  private async getDocuments(name: string, extension: string, content: string): Promise<VectorDocument[]> {
    const splitter = new RecursiveCharacterTextSplitter({
        chunkSize: this.CHUNK_SIZE,
        chunkOverlap: this.CHUNK_OVERLAP
    });

    const docs = await splitter.createDocuments([content]);

    return docs.map(doc => ({
      ...doc,
      metadata: {
        ...doc.metadata,
        source: name,
        extension,
      }
    }));
  }

  private async loadFile(filePath: string, extension: string): Promise<string | null> {
    try {
      if (extension === '.pdf') {
        const loader = new PDFLoader(filePath, { splitPages: true });
        const docs = await loader.load();
        return docs.map(doc => doc.pageContent).join('\n');

      } else {
        const data = await fs.promises.readFile(filePath, 'utf-8');
        return data;
      }
    } catch (error) {
      logger.error(`[DocEmbeddingService] Error loading ${filePath}:`, error);
      logger.errorState(error, `[DocEmbeddingService] - LoadFile: ${filePath}`);
      return null;
    }
  }

  private async executeUnit(filePath: string): Promise<void> {
    logger.thinking(`Processing file: ${filePath}`);
    const extension = path.extname(filePath).toLowerCase();
    const name = path.basename(filePath);

    const content = await this.loadFile(filePath, extension);
    if (!content) return;

    const docs = await this.getDocuments(name, extension, content);
    await this.vectorStore.addDocuments(docs);
    logger.thinking(`Successfully added ${docs.length} documents from ${filePath}`);
  }

  private async createFileMetadata(rawName: string): Promise<void> {
    try {
      fs.readdirSync(DOCUMENT_METADATA_PATH);
    } catch (error) {
      fs.mkdirSync(DOCUMENT_METADATA_PATH, { recursive: true });
    }

    const name = path.basename(rawName, path.extname(rawName));
    const metadataPath = path.join(DOCUMENT_METADATA_PATH, `${name}.json`);
    if (!fs.existsSync(metadataPath)) {
      const metadata = { name, createdAt: new Date().toISOString() };
      fs.writeFileSync(metadataPath, JSON.stringify(metadata, null, 2));
    }
  }

  getDocumentsWithoutMetadata(): string[] {
    try {
      fs.readdirSync(DOCUMENT_METADATA_PATH);  
    } catch (error) {
      fs.mkdirSync(DOCUMENT_METADATA_PATH, { recursive: true });
    }

    try {
      const metadatas = fs.readdirSync(DOCUMENT_METADATA_PATH)
        .map(file => path.basename(file, '.json'));

      const documents = fs.readdirSync(DOCUMENT_TO_EMBED_PATH)
        .map(file => path.basename(file));

      const filesToEmbed = documents.filter(doc => !metadatas.includes(path.basename(doc, path.extname(doc))));
      
      return filesToEmbed;
    } catch (error) {

      logger.error("[DocEmbeddingService] Error selecting documents to embed:", error);
      logger.errorState(error, "[DocEmbeddingService] - GetDocumentsWithoutMetadata");
      return [];
    }
  }

  public async execute(files: string[]): Promise<void> {
    try {
      const hasLoaded = await this.vectorStore.load(VECTOR_DATABASE_PATH);
      if (!hasLoaded) {
        await this.vectorStore.create(VECTOR_DATABASE_PATH);
      }

      for (const file of files) {
        const filePath = path.join(DOCUMENT_TO_EMBED_PATH, file);
        await this.executeUnit(filePath);
        await this.createFileMetadata(file);
      }

      await this.vectorStore.save(VECTOR_DATABASE_PATH);
    } catch (error) {
      logger.error("[DocEmbeddingService] Error initializing vector store:", error)
      logger.errorState(error, "[DocEmbeddingService] - Execute");
    }
  }
}