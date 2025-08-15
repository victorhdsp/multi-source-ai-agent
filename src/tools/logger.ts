export const logger = {
    log: (message?: any, ...optionalParams: any[]) => {
        const timestamp = new Date().toISOString();
        console.log(`[${timestamp}] ${message}`, ...optionalParams);
    },
    error: (message?: any, ...optionalParams: any[]) => {
        const timestamp = new Date().toISOString();
        console.error(`[${timestamp}] ERROR: ${message}`, ...optionalParams);
    },
    info: (message?: any, ...optionalParams: any[]) => {
        const timestamp = new Date().toISOString();
        console.info(`[${timestamp}] INFO: ${message}`, ...optionalParams);
    },
    warn: (message?: any, ...optionalParams: any[]) => {
        const timestamp = new Date().toISOString();
        console.warn(`[${timestamp}] WARN: ${message}`, ...optionalParams);
    },
    debug: (message?: any, ...optionalParams: any[]) => {
        const timestamp = new Date().toISOString();
        console.debug(`[${timestamp}] DEBUG: ${message}`, ...optionalParams);
    },
    thinking: (message?: any, ...optionalParams: any[]) => {
        console.log(`   - Pensando: ${message}`, ...optionalParams);
    },
    talk: (message?: any, ...optionalParams: any[]) => {
        console.log(message, ...optionalParams);
    }
}