//@flow
export type A =
  | { type: "a", value: string }
  | { type: "b", value: number }
  | { type: "c", value: boolean };
export type VariantTest =
  | { type: "a", value: string }
  | { type: "b", value: number }
  | { type: "c", value: boolean };
