# LazyScript

LazyScript is a dynamic, lazily evaluated programming language featuring pure IO.

## Usage

```bash
git clone https://github.com/jjant/LazyScript.git

(cd frontend && elm-live Main.elm --open --debug)&; (cd backend/compiler-web-backend && stack build --fast --pedantic && stack exec compiler-web-backend-exe)&;
```

## Requirements

* Elm platform
* Haskell, stack, cabal
