import readline from "readline";
import { EventEmitter } from "events";
import { logger } from "./logger";

export const rlBus = new EventEmitter();

let rl: readline.Interface | null = null;
const PROMPT = "User= ";

export function startReadline() {
  if (rl) return rl;

  rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: PROMPT,
  });

  rl.on("line", (line) => {
    const question = line.trim();
    if (!question) {
      return;
    }

    if (question.toLowerCase() === "exit" || question.toLowerCase() === "sair") {
      logger.info("Encerrando...");
      rl?.close();
      return;
    }

    rlBus.emit("input", question);
  }).on("close", () => {
    process.exit(0);
  });

  return rl;
}

export function rlPrompt() {
  if (!rl) return;
  rl.prompt();
}

export function rlWrite(line: string) {
  if (!rl) return;
  rl.write(null, { ctrl: true, name: "u" }); 
  console.log(line);
}

export function waitForUserInput(promptMessage: string): Promise<string> {
  logger.talk(promptMessage);
  rlPrompt();

  return new Promise((resolve) => {
    const handler = (input: string) => {
      rlBus.off("input", handler);
      resolve(input);
    };
    rlBus.on("input", handler);
  });
}
