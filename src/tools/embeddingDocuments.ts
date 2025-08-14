import env from 'dotenv';
import fs from 'fs';
import path from 'path';

import { type IVectorStore, type VectorDocument } from "@/src/infra/interfaces/vector.repository";
import { VectorStore } from "@/src/infra/repository/vector.repository";
import { GoogleGenerativeAIEmbeddings } from "@langchain/google-genai";
import { TaskType } from "@google/generative-ai";
import { TextLoader } from "langchain/document_loaders/fs/text";
import { PDFLoader } from '@langchain/community/document_loaders/fs/pdf';
import { VECTOR_DATABASE_PATH } from '../config';
import { RecursiveCharacterTextSplitter } from 'langchain/text_splitter';

env.config();

const DOCUMENT_PATH = path.join(__dirname, '../../data/documents/');
const PERMITTED_EXTENSIONS = new Set(['.txt', '.md', '.pdf']);
const CHUNK_SIZE = 1000;
const CHUNK_OVERLAP = 200;

const embedding = new GoogleGenerativeAIEmbeddings({
  model: "text-embedding-004",
  taskType: TaskType.RETRIEVAL_DOCUMENT,
  title: "Document title",
});

const textSplitter = new RecursiveCharacterTextSplitter({
  chunkSize: CHUNK_SIZE,
  chunkOverlap: CHUNK_OVERLAP,
  separators: ['\n\n', '\n', '. ', '? ', '! ', '; ', ', ', ' '],
});

async function processFile(filePath: string): Promise<VectorDocument[]> {
  const extension = path.extname(filePath).slice(1).toLowerCase();
  
  try {
    let loader;
    if (extension === 'pdf') {
      loader = new PDFLoader(filePath, {
        splitPages: true,
      });
    } else {
      loader = new TextLoader(filePath);
    }

    const rawDocs = await loader.load();
    
    const splitDocs = await textSplitter.splitDocuments(rawDocs);
    
    return splitDocs.map(doc => ({
      ...doc,
      metadata: {
        ...doc.metadata,
        source: path.basename(filePath),
        extension,
      }
    }));
  } catch (error) {
    console.error(`Error processing ${filePath}:`, error);
    return [];
  }
}

try {
    const vectorStore: IVectorStore = new VectorStore(embedding);
    const hasLoaded = await vectorStore.load(VECTOR_DATABASE_PATH);
    if (!hasLoaded) {
        await vectorStore.create(VECTOR_DATABASE_PATH);
    }

    const documents = fs.readdirSync(DOCUMENT_PATH)
      .filter(file => PERMITTED_EXTENSIONS.has(path.extname(file)));

    for (const file of documents) {
        const filePath = path.join(DOCUMENT_PATH, file);
        const docs = await processFile(filePath);
        await vectorStore.addDocuments(docs);
        console.log(`Successfully added ${docs.length} documents from ${file}`);
    }

    await vectorStore.save(VECTOR_DATABASE_PATH);
} catch (error) {
  console.error("Error initializing vector store:", error)
}