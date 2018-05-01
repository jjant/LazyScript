# LazyScript

LazyScript is a dynamic, lazily evaluated programming language featuring pure IO.

## Examples

```javascript
const infinite = [1, 2, ...infinite];

const greetIndefinitely = infinite.map(_ => IO.putStrLn('Hi!'));

const main = constantlyGreet;
```

## Usage

```bash
git clone https://github.com/jjant/LazyScript.git

(cd frontend && elm-live Main.elm --open --debug)&; (cd backend/compiler-web-backend && stack build --fast --pedantic && stack exec compiler-web-backend-exe)&;
```

## Requirements

* Elm platform
* Haskell, stack, cabal
