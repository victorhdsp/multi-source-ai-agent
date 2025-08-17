import { randomUUIDv7, sleep } from "bun";
import { SYSTEM_DATA } from "./config";
import { callToMultiAgentUseCase, dbMetadataService, dependencies, docEmbeddingService } from "./dependences";
import { logger } from "./tools/logger";
import { rlBus, rlPrompt, startReadline, waitForUserInput } from "./tools/readline";
import { HUMAN_RESPONSE } from "./domain/core/types/human";

async function main() {
    let isProcessing = false;

    startReadline();

    const dbFiles = dbMetadataService.getDBWithoutMetadata();
    if (dbFiles.length > 0) {
        const question = (
            `Foram encontrados ${dbFiles.length} bancos de dados sem metadados.\n` +
            "Deseja que eu processe eles agora? (y/n)"
        )
        const answer = await waitForUserInput(question);
        if (HUMAN_RESPONSE.TRUE.has(answer.toLowerCase())) {
            logger.thinking("Iniciando processamento dos bancos de dados...");
            await dbMetadataService.execute(dbFiles);
        } else {
            logger.warn("Você optou por não processar os bancos de dados. Alguns recursos podem não funcionar corretamente.");
        }
    }

    const filesToEmbed = docEmbeddingService.getDocumentsWithoutMetadata();
    if (filesToEmbed.length > 0) {
        const question = (
            `Foram encontrados ${filesToEmbed.length} documentos sem metadados.\n` +
            "Deseja que eu processe eles agora? (y/n)"
        )
        const answer = await waitForUserInput(question);
        if (HUMAN_RESPONSE.TRUE.has(answer.toLowerCase())) {
            logger.thinking("Iniciando processamento dos documentos...");
            await docEmbeddingService.execute(filesToEmbed);
        } else {
            logger.warn("Você optou por não processar os documentos. Alguns recursos podem não funcionar corretamente.");
        }
    }

    rlPrompt();

    rlBus.on("input", async (question) => {
        if (isProcessing) {
            logger.warn("Já estou processando uma pergunta, aguarde...");
            return;
        }
        
        isProcessing = true;
        try {
            const response = await callToMultiAgentUseCase.execute(question);
            logger.talk("\nAgent=", response);
            await sleep(100); 

        } catch (err) {
            logger.error("Erro:", err);
        } finally {
            isProcessing = false;
            rlPrompt();
        }
    });
}


dependencies.init()
    .then(() => {
        logger.info("Dependencies initialized successfully.");
        SYSTEM_DATA.currentSeason = randomUUIDv7();
        logger.thinking(`Sessão iniciada: ${SYSTEM_DATA.currentSeason}`);
        main();
    })
    .catch((error) => {
        logger.error("Error initializing dependencies:", error);
    });
