import { EMBEDDING_MODEL, SQL_DATABASE_PATH, VECTOR_DATABASE_PATH } from "./config";
import { TaskType } from "@google/generative-ai";
import { QuestionAgentStrategy } from "./domain/question/strategy";
import { QuestionAgentUsecase } from "./domain/question/usecase";
import { SelfAskWithSearchStrategy } from "@/src/domain/search/selfAskWithSearch/strategy";
import { SearchAgentUsecase } from "@/src/domain/search/usecase";
import { MultiAgentUseCase } from "./domain/core/usecase";
import { AgentLLMService } from "./infra/gateway/agentLlm.service";
import { VectorStore } from "./infra/repository/vector.repository";
import { GoogleGenerativeAIEmbeddings } from "@langchain/google-genai";
import { SearchAgentWorkflowManager } from './domain/search/workflowManager';
import { EmbeddingService } from "./infra/gateway/embedding.service";
import Database from 'bun:sqlite';
import { DBMetadataService } from "./tools/metadataSQLDatabase";
import { ToolBoxService } from "./domain/search/tool/service";
import { UseCurlTool } from "./domain/search/tool/curl/useCurl";
import { UseSQLiteTool } from "./domain/search/tool/sqlite/useSQLite";
import { GetPageService } from "./domain/search/tool/curl/getPageService";
import { FindDBService } from "./domain/search/tool/sqlite/findDBService";

class Dependencies {
  constructor (
    embeddingModel = new GoogleGenerativeAIEmbeddings({
      model: EMBEDDING_MODEL,
      taskType: TaskType.RETRIEVAL_DOCUMENT,
      title: "Document title",
    }),

    embeddingService = new EmbeddingService(
      embeddingModel
    ),

    getPageService = new GetPageService(),
    findMusicDBService = new FindDBService(),
    private vectorStore = new VectorStore(
      embeddingModel
    ),
    model = new AgentLLMService(),
    
    useCurlTool = new UseCurlTool(
      getPageService,
      embeddingService
    ),
    useSQLiteTool = new UseSQLiteTool(
      findMusicDBService,
      embeddingService
    ),
    toolBoxService = new ToolBoxService(
      useCurlTool,
      useSQLiteTool
    ),
    strategyQuestionAgent = new QuestionAgentStrategy(
      model
    ),
    strategySearchAgent = new SelfAskWithSearchStrategy(
      model,
      toolBoxService
    ),
    searchAgentWorkflowManager = new SearchAgentWorkflowManager(
      strategySearchAgent,
      toolBoxService
    ),

    questionUsecase = new QuestionAgentUsecase(
      vectorStore,
      strategyQuestionAgent
    ),
    searchUsecase = new SearchAgentUsecase(
      searchAgentWorkflowManager
    ),

    public callToMultiAgentUseCase = new MultiAgentUseCase(
      questionUsecase,
      searchUsecase
    ),
    public dbMetadataService = new DBMetadataService(model)
  ){}

  async init() {
    await this.vectorStore.load(VECTOR_DATABASE_PATH);
  }
}

export const dependencies = new Dependencies();

export const { 
  callToMultiAgentUseCase,
  dbMetadataService
} = dependencies;