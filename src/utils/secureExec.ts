export function secureExec<T>(fn: () => T, errorMessage: string): T {
    try {
        return fn();
    } catch (error) {
        throw new Error(errorMessage);
    }
}

export async function secureExecAsync<T>(fn: () => Promise<T>, errorMessage: string): Promise<T> {
    try {
        return await fn();
    } catch {
        throw new Error(errorMessage);
    }
}