import type { ITool, resolveToolType } from "./type";

export abstract class GenericTool implements ITool {
  constructor() { }

  abstract execute(params: any): Promise<string>;
  abstract invoke(): resolveToolType;
}