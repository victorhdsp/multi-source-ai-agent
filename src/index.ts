import { callToMultiAgentUseCase, dependencies } from "./dependences";
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

        isProcessing = true;
        try {
            const response = await callToMultiAgentUseCase.execute(question);
            logger.log("Resposta:", response);

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
        main();
    })
    .catch((error) => {
        logger.error("Error initializing dependencies:", error);
    });
