import Database from "bun:sqlite";
import type { UseSQLiteConsume } from "./type";
import { logger } from "@/src/tools/logger";

export class FindDBService {
    async selectFromDatabase(params: UseSQLiteConsume) {
        const { path, table, filters, columns } = params;
        logger.thinking(`Consultando banco de dados:\n\t${path}`);

        try {
            const db = new Database(path);

            const cols = columns?.length ? columns.join(", ") : "*";
            let query = `SELECT ${cols} FROM ${table}`;
            if (filters) {
                query += ` ${filters}`;
            }

            const data = db.prepare(query).all();
            return data as Record<string, any>[];

        } catch (error) {
            logger.errorState(error, "[FindDBService] - SelectFromDatabase");
            return [`Nenhum dado encontrado com ${path}, ${table}, ${filters}, ${columns}`];
        }
    }
}