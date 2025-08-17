import { spawn } from "bun";
import type { UseCurlConsume } from "./type";
const COMMAND_ERRORS = {
    INVALID_URL: "Invalid URL",
    CURL_FAILED: "Curl command failed",
}

export class GetPageService {
    async getPage(params: UseCurlConsume): Promise<string> {
        const curl = spawn(["curl", "-X", "GET", "-s", params.url]);

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
