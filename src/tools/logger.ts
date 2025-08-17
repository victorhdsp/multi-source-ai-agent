import chalk from "chalk";
import { LOG_PATH, SYSTEM_DATA } from "../config";
import fs from 'fs';
import path from "path";

let stateCounter = 0;

export const logger = {
    log: (message?: any, ...optionalParams: any[]) => {
        const timestamp = new Date().toISOString();
        console.log(chalk.white(`[${timestamp}] ${message}`), ...optionalParams);
    },
    error: (message?: any, ...optionalParams: any[]) => {
        const timestamp = new Date().toISOString();
        console.error(chalk.red(`[${timestamp}] ERROR: ${message}`), ...optionalParams);
    },
    info: (message?: any, ...optionalParams: any[]) => {
        const timestamp = new Date().toISOString();
        console.info(chalk.blue(`[${timestamp}] INFO: ${message}`), ...optionalParams);
    },
    warn: (message?: any, ...optionalParams: any[]) => {
        const timestamp = new Date().toISOString();
        console.warn(chalk.yellow(`[${timestamp}] WARN: ${message}`), ...optionalParams);
    },
    debug: (message?: any, ...optionalParams: any[]) => {
        const timestamp = new Date().toISOString();
        console.debug(chalk.gray(`[${timestamp}] DEBUG: ${message}`), ...optionalParams);
    },
    thinking: (message?: any, ...optionalParams: any[]) => {
        LogInFile(message, ' - THINKING:\n ');
        if (!message) return;
        console.log(chalk.italic.cyan(`   - Pensando: ${message}`), ...optionalParams);
    },
    state: (message?: any, ...optionalParams: any[]) => {
        LogInFile(message, ` - STATE COUNTER: ${stateCounter}:\n`, '\n');
        stateCounter++;
        //console.log(chalk.italic.magenta(`   - Estado ${stateCounter}:`), message, ...optionalParams);
    },
    errorState: (message?: any, ...optionalParams: any[]) => {
        LogInFile(message, `\n - ERROR:\n-----\n`, '\n-----\n');
    },
    talk: (message?: any, ...optionalParams: any[]) => {
        console.log(chalk.white(message), ...optionalParams);
    }
}

export function LogInFile(message: string, prefix = '', suffix = '') {
    if (!SYSTEM_DATA.currentSeason) return;
    const outputPath = path.join(LOG_PATH, `log_${SYSTEM_DATA.currentSeason}.txt`);
    const content = typeof message === 'string' ? message : JSON.stringify(message, null, 2);

    fs.mkdirSync(path.dirname(outputPath), { recursive: true });

    fs.readFile(outputPath, 'utf8', (err, data) => {
        const result = `[${new Date().toISOString()}]${prefix}${content}\n${suffix}`;
        err ? fs.writeFileSync(outputPath, result) : fs.appendFileSync(outputPath, result);
    });
}