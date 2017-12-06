//@flow
import * as Gen from "./generated.flow";

const a1: Gen.A = {
  a: 1,
  b: "asdf",
  c: {
    d: "asdf"
  },
  e: ["asdf"],
  f: "asdf",
  g: a => b => a + b,
  h: (a, b) => a,
  i: (a, b) => a,
  k: { a: 1212 },
  l: "literally anything",
  m: () => "asdf"
};

const a2: Gen.A = {
  a: 123,
  b: "asdf",
  c: {
    d: "asdf"
  },
  e: ["asdf"],
  f: null,
  g: a => b => a - b,
  h: (a, b) => a,
  i: (a, b) => a,
  k: { j: 1, m: 2 },
  l: false,
  m: () => "asdf"
};

const fakeUnion1: Gen.VariantTest = {
  type: "a",
  value: "asdf"
};

const fakeUnion2: Gen.VariantTest = {
  type: "b",
  value: 123
};

const fakeUnion3: Gen.VariantTest = {
  type: "c",
  value: true
};
