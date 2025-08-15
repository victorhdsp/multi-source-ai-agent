import { TaskType } from "@google/generative-ai";
import { GoogleGenerativeAIEmbeddings } from "@langchain/google-genai";
import { AgentLLMService } from "./infra/gateway/agentLlm.service";
import { QuestionAgentParser } from "./core/utils/outputParser/QuestionAgent.parser";
import { VectorStore } from "./infra/repository/vector.repository";
import { QuestionAgentService } from "./core/question/strategy";
import { QuestionAgentUsecase } from "./core/question/usecase";
import { VECTOR_DATABASE_PATH } from "./config";
import { CallToMultiAgentUseCase } from "./core/usecase";
import { SearchAgentUsecase } from "./core/search/usecase";
import { ExecuteAgentUsecase } from "./core/execute/usecase";

class Dependencies {
  constructor (
    embedding = new GoogleGenerativeAIEmbeddings({
      model: "text-embedding-004",
      taskType: TaskType.RETRIEVAL_DOCUMENT,
      title: "Document title",
    }),
    agentLLM = new AgentLLMService(),
    private vectorDB = new VectorStore(embedding),
    parser = new QuestionAgentParser(),
    questionService = new QuestionAgentService(agentLLM, parser),
    questionUsecase = new QuestionAgentUsecase(vectorDB, questionService),
    
    searchUsecase = new SearchAgentUsecase(),
    executeUsecase = new ExecuteAgentUsecase(),

    public callToMultiAgentUseCase = new CallToMultiAgentUseCase(questionUsecase, searchUsecase, executeUsecase)
  ){}

  async init() {
    await this.vectorDB.load(VECTOR_DATABASE_PATH);
  }
}

const dependencies = new Dependencies();
dependencies.init();

export const { callToMultiAgentUseCase } = dependencies;