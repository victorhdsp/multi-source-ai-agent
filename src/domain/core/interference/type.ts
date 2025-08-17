export const INTERRUPT_TYPES = {
    PERMISSION: "PERMISSION",
    QUESTION: "QUESTION",
} as const;

export type InterruptType = keyof typeof INTERRUPT_TYPES;

export interface InterruptDTO {
    type: InterruptType;
    message: string;
}

export const HUMAN_RESPONSE = {
    TRUE: new Set(["sim", "s", "yes", "y"]),
    FALSE: new Set(["não", "n", "no"]),
} as const;