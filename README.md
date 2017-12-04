# Bismuth

[![Build Status](https://travis-ci.org/justinwoo/purescript-bismuth.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-bismuth)

A library for generating flow type signatures for direct interop between PureScript and Flow. Similar to [OhYes](https://github.com/justinwoo/purescript-ohyes)

## Tl;dr

```hs
type A =
  { a :: Number
  , b :: String
  , c :: { d :: String }
  , e :: Array String
  , f :: Nullable String
  , g :: Number -> Number -> Number
  , h :: Fn2 Number Number Number
  , i :: Fn2 Number (Fn2 Number Number Number) Number
  , l :: Foreign
  , k :: StrMap Number
  }

type VariantTest = Variant
  ( a :: String
  , b :: Number
  , c :: Boolean
  )
```

```js
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
  l: any
};
export type VariantTest =
  | { type: "a", value: string }
  | { type: "b", value: number }
  | { type: "c", value: boolean };
```
