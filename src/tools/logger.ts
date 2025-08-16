import chalk from "chalk";

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
        console.log(chalk.italic.cyan(`   - Pensando: ${message}`), ...optionalParams);
    },
    talk: (message?: any, ...optionalParams: any[]) => {
        console.log(chalk.white(message), ...optionalParams);
    }
}