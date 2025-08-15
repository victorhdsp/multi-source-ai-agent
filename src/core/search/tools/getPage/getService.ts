import { spawn } from "bun";
import type { IGetService } from "./type";

const COMMAND_ERRORS = {
    INVALID_URL: "Invalid URL",
    CURL_FAILED: "Curl command failed",
}

export class GetPageService implements IGetService {
    async getPage(url: string): Promise<string> {
        const curl = spawn(["curl", "-X", "GET", "-s", url]);

        const stderr = curl.stderr;

        if (stderr) {
            throw new Error(COMMAND_ERRORS.CURL_FAILED);
        }

        const stdout = await curl.stdout.text();
        if (!stdout || stdout.length === 0) {
            throw new Error(COMMAND_ERRORS.INVALID_URL);
        }
        
        return stdout;
    }
}
