import { TaskType } from "@google/generative-ai";
import { ExecuteAgentUsecase } from "./core/execute/usecase";
import { CheckResultInterferenceUsecase } from "./core/interference/checkResult.usecase";
import { ResearchInterferenceUsecase } from "./core/interference/research.usecase";
import { QuestionAgentStrategy } from "./core/question/strategy";
import { QuestionAgentUsecase } from "./core/question/usecase";
import { SelfAskWithSearchStrategy } from "./core/search/selfAskWithSearch/strategy";
import { docPageTool } from "./core/search/tools/getPage/doc";
import { GetPageService } from "./core/search/tools/getPage/getPageService";
import { GetPageTool } from "./core/search/tools/getPage/tool";
import { docFindDBMusicTool } from "./core/search/tools/musicDB/doc";
import { FindMusicDBService } from "./core/search/tools/musicDB/findMusicDBService";
import { FindMusicDBTool } from "./core/search/tools/musicDB/tool";
import { SearchAgentTools } from "./core/search/tools/tools";
import { SearchAgentUsecase } from "./core/search/usecase";
import { MultiAgentUseCase } from "./core/usecase";
import { AgentLLMService } from "./infra/gateway/agentLlm.service";
import { VectorStore } from "./infra/repository/vector.repository";
import { GoogleGenerativeAIEmbeddings } from "@langchain/google-genai";
import Database from 'bun:sqlite';
import { SQL_DATABASE_PATH, VECTOR_DATABASE_PATH } from "./config";


class Dependencies {
  constructor (
    embedding = new GoogleGenerativeAIEmbeddings({
      model: "text-embedding-004",
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

    questionUsecase = new QuestionAgentUsecase(
      vectorStore,
      strategyQuestionAgent
    ),
    searchUsecase = new SearchAgentUsecase(
      strategySearchAgent,
      toolSearchAgent
    ),
    executeUsecase = new ExecuteAgentUsecase(),
    researchUsecase = new ResearchInterferenceUsecase(),
    checkUsecase = new CheckResultInterferenceUsecase(),

    public callToMultiAgentUseCase = new MultiAgentUseCase(
      questionUsecase,
      searchUsecase,
      executeUsecase,
      researchUsecase,
      checkUsecase
    )
  ){}

  async init() {
    await this.vectorStore.load(VECTOR_DATABASE_PATH);
  }
}

const dependencies = new Dependencies();
dependencies.init();

export const { callToMultiAgentUseCase } = dependencies;