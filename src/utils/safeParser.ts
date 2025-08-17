export function safeJsonParse<T = unknown>(input: string): T {
  let cleaned = input
    .replace(/```json\s*/gi, "")
    .replace(/```/g, "")
    .trim();

  const match = cleaned.match(/\{[\s\S]*\}/);
  if (!match) throw new Error("Nenhum JSON encontrado");

  const sanitized = match[0].replace(/\\(?!["\\/bfnrtu])/g, "\\\\");

  return JSON.parse(sanitized) as T;
}
