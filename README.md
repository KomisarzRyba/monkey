# Monkey in Zig

Tiny interpreter + REPL for the Monkey language, written in Zig 0.14.0.
Source lives in `src/`, examples in `examples/`, tests alongside the code.

## Build / run

```bash
# build and install static lib + executable to zig-out/
zig build

# run the REPL
zig build run

# run a monkey program
zig build run examples/simple.mon

# unit tests
zig build test
```

## Requirements

- Zig ≥ 0.14.0 (see `.tool-versions`)
- No other dependencies – the build script (`build.zig`) takes care of everything.

## Acknowledgments

Huge shout-out to Thorsten Ball for his fantastic book [Writing an Interpreter in Go](https://interpreterbook.com/).
