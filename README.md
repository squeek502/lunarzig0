# lunarzig0

A (very unfinished) experiment in creating an interpreter (written in Lua) for the [Zig language](https://ziglang.org/) for the purposes of bootstrapping ([see here for context](https://ziggit.dev/t/what-would-second-implementation-of-zig-need-to-compile-bootstrap-zig/10103/2)).

> a zig interpreter that assumes `-target wasm32-wasi` and uses `pub const dev = .bootstrap;` in the config options

The ultimate goal is to be able to:

- Build Lua with a C compiler
- `lua lunarzig0.lua path/to/zig/src/main.zig > zig2.c`
  + (note: this `zig2.c` should be identical to the output of `zig1` in the [current Zig bootstrap process](https://ziglang.org/news/goodbye-cpp/))
- Build `zig2.c` with a C compiler
- Continue with the normal Zig bootstrap process from there

## Status

Only the tokenizer and parser have been implemented. They are more or less direct ports of the Zig implementations, and are tested/verified by dumping out textual representations of the parsed tokens/AST and comparing them to a textual representation of the Zig implementation's. As of now, those representations are identical when tokenizing/parsing all files in `lib/std/` and `src/` of the Zig codebase.

Some general caveats worth noting:

- The Lua implementation of the tokenizer/parser is much, much slower and much, much more memory hungry than the Zig implementation
  + It takes ~500MiB of memory to store the AST of `src/codegen/x86_64/CodeGen.zig` (the largest file in the Zig codebase by far)
  + If this becomes a problem in practice, then one possible avenue would be to move the relevant part(s) into a Lua module written in C
- I'm not sure when I'll get around to working on this more, so feel free to take it and do whatever with it if you're interested

For the interpreter, here are some possible programs to use as a starting point for a proof-of-concept:

This should do nothing and exit with 0:

```zig
pub extern "wasi_snapshot_preview1" fn proc_exit(rval: u32) noreturn;

pub export fn _start() callconv(.c) void {
    proc_exit(0);
}
```

This should print `Hello, World!` to stdout and exit with 0:

```zig
const std = @import("std");

pub export fn _start() callconv(.c) void {
    const stdout = 1;
    const msg = "Hello, World!\n";
    const vecs: [1]std.posix.iovec_const = .{
        .{ .base = msg, .len = msg.len },
    };
    var nwritten: usize = undefined;
    _ = std.os.wasi.fd_write(stdout, &vecs, 1, &nwritten);
    std.os.wasi.proc_exit(0);
}
```

## Testing

### Running the ported tokenizer tests

```
lua test_tokenizer.lua
```

### Running checks against the Zig tokenizer/parser

Last tested with `zig-0.16.0-dev.1301+cbfa87cbe`; having `lua` in your `PATH` is required.

```
zig build-exe dump_tokens.zig
zig build-exe check_tokenizer.zig
./check_tokenizer path/to/zig/lib/std
./check_tokenizer path/to/zig/src
```

```
zig build-exe dump_ast.zig
zig build-exe check_ast.zig
./check_ast path/to/zig/lib/std
./check_ast path/to/zig/src
```
