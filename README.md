# Elm Representer

This outputs a normalized representation of Elm Code, to make automated analysis easier within Exercism.

Thanks to [Elm Platform Worker Example](https://github.com/jxxcarlson/elm-platform-worker-example) for the initial template.

## Install

```bash
npm install
```

## Build

-d uses debug in `elm make` (required if `Debug` is used in the code, which it isn't at the moment)

```bash
sh make.sh
sh make.sh -d
```

## Usage

To normalise bob.elm and save results to bob-normalized.elm

```bash
node src/cli.js "$(cat bob.elm)" > bob-normalized.elm
```

To normalize all the example files in this repo

```
sh normalize-examples.sh
```

## Test

```
elm-test
```