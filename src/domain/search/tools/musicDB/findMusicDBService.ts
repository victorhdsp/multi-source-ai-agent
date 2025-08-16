import type Database from "bun:sqlite";
import type { DatabaseConsume, IFindDatabaseService } from "@/src/domain/search/tools/musicDB/types";

export class FindMusicDBService implements IFindDatabaseService {
    constructor(
        private readonly db: Database
    ) {}

    async selectFromDatabase(params: DatabaseConsume) {
        const { table, filters, columns } = params;

        const cols = columns?.length ? columns.join(", ") : "*";
        let query = `SELECT ${cols} FROM ${table}`;
        if (filters) {
        query += ` WHERE ${filters}`;
        }
        const data = this.db.prepare(query).all();
        return data as Record<string, any>[];
    }
}