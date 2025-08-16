import { spawn } from "bun";
import type { GetPageConsume, IGetService } from "@/src/domain/search/tools/getPage/type";

const COMMAND_ERRORS = {
    INVALID_URL: "Invalid URL",
    CURL_FAILED: "Curl command failed",
}

export class GetPageService implements IGetService {
    async getPage(params: GetPageConsume): Promise<string> {
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
