//@flow
export type A = {
  a: number,
  b: string,
  c: { d: string },
  e: string[],
  f: string | null,
  g: (a: number) => (a: number) => number,
  h: (a: number, b: number) => number,
  i: (a: number, b: (a: number, b: number) => number) => number,
  k: { [key: string]: number },
  l: any,
  m: () => string
};
export type VariantTest =
  | { type: "a", value: string }
  | { type: "b", value: number }
  | { type: "c", value: boolean };
