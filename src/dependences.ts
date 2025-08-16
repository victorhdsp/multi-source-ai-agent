import { TaskType } from "@google/generative-ai";
import { QuestionAgentStrategy } from "./domain/question/strategy";
import { QuestionAgentUsecase } from "./domain/question/usecase";
import { SelfAskWithSearchStrategy } from "@/src/domain/search/selfAskWithSearch/strategy";
import { docPageTool } from "@/src/domain/search/tools/getPage/doc";
import { GetPageService } from "@/src/domain/search/tools/getPage/getPageService";
import { GetPageTool } from "@/src/domain/search/tools/getPage/tool";
import { docFindDBMusicTool } from "@/src/domain/search/tools/musicDB/doc";
import { FindMusicDBService } from "@/src/domain/search/tools/musicDB/findMusicDBService";
import { FindMusicDBTool } from "@/src/domain/search/tools/musicDB/tool";
import { SearchAgentTools } from "@/src/domain/search/tools/tools";
import { SearchAgentUsecase } from "@/src/domain/search/usecase";
import { MultiAgentUseCase } from "./domain/core/usecase";
import { AgentLLMService } from "./infra/gateway/agentLlm.service";
import { VectorStore } from "./infra/repository/vector.repository";
import { GoogleGenerativeAIEmbeddings } from "@langchain/google-genai";
import Database from 'bun:sqlite';
import { EMBEDDING_MODEL, SQL_DATABASE_PATH, VECTOR_DATABASE_PATH } from "./config";
import { SearchAgentWorkflowManager } from './domain/search/workflow/manager';

class Dependencies {
  constructor (
    embedding = new GoogleGenerativeAIEmbeddings({
      model: EMBEDDING_MODEL,
      taskType: TaskType.RETRIEVAL_DOCUMENT,
      title: "Document title",
    }),

    db = new Database(`${SQL_DATABASE_PATH}/music.db`),

    getPageService = new GetPageService(),
    findMusicDBService = new FindMusicDBService(
      db
    ),

    private vectorStore = new VectorStore(
      embedding
    ),
    model = new AgentLLMService(),
    
    getPageTool = new GetPageTool(
      docPageTool,
      getPageService
    ),
    findMusicDBTool = new FindMusicDBTool(
      docFindDBMusicTool,
      findMusicDBService
    ),

    strategyQuestionAgent = new QuestionAgentStrategy(
      model
    ),
    toolSearchAgent = new SearchAgentTools(
      getPageTool,
      findMusicDBTool
    ),
    strategySearchAgent = new SelfAskWithSearchStrategy(
      model,
      toolSearchAgent.searchAgentTools
    ),
    searchAgentWorkflowManager = new SearchAgentWorkflowManager(
      strategySearchAgent,
      toolSearchAgent
    ),

    questionUsecase = new QuestionAgentUsecase(
      vectorStore,
      strategyQuestionAgent
    ),
    searchUsecase = new SearchAgentUsecase(
      strategySearchAgent,
      toolSearchAgent,
      searchAgentWorkflowManager
    ),

    public callToMultiAgentUseCase = new MultiAgentUseCase(
      questionUsecase,
      searchUsecase
    )
  ){}

  async init() {
    await this.vectorStore.load(VECTOR_DATABASE_PATH);
  }
}

export const dependencies = new Dependencies();;

export const { callToMultiAgentUseCase } = dependencies;