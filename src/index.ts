import readline from "readline";
import { callToMultiAgentUseCase, dependencies } from "./dependences";
import { logger } from "./tools/logger";

async function main() {
    logger.info("Agent Service is running...");
    
    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout,
        prompt: "Pergunta> "
    });

    rl.prompt();

    rl.on("line", async (line) => {
        const question = line.trim();
        if (!question) {
            rl.prompt();
            return;
        }

        if (question.toLowerCase() === "exit" || question.toLowerCase() === "sair") {
            logger.info("Encerrando...");
            rl.close();
            return;
        }

        try {
            const response = await callToMultiAgentUseCase.execute(question);
            logger.info("Resposta:", response);
        } catch (err) {
            logger.error("Erro:", err);
        }

        rl.prompt();
    }).on("close", () => {
        process.exit(0);
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
