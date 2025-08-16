import { randomUUIDv7, sleep } from "bun";
import { SYSTEM_DATA } from "./config";
import { callToMultiAgentUseCase, dbMetadataService, dependencies } from "./dependences";
import { logger } from "./tools/logger";
import { rlBus, rlPrompt, startReadline } from "./tools/readline";

async function main() {
    let isProcessing = false;

    startReadline();
    rlPrompt();

    rlBus.on("input", async (question) => {
        if (isProcessing) {
            logger.warn("Já estou processando uma pergunta, aguarde...");
            return;
        }
        const dbFiles = await dbMetadataService.getDBWithoutMetadata();
        if (dbFiles.length > 0) {
            logger.info("Arquivos de banco de dados encontrados:", dbFiles);
            await dbMetadataService.execute(dbFiles);
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
