# A New Stan-to-C++ Compiler, stanc3
This repo contains a new compiler for Stan, stanc3, written in OCaml. To read more about why we built this, see this [introductory blog post](https://statmodeling.stat.columbia.edu/2019/03/13/stanc3-rewriting-the-stan-compiler/). For some discussion as to how we chose OCaml, see [this accidental flamewar](https://discourse.mc-stan.org/t/choosing-the-new-stan-compilers-implementation-language/6203).
We're testing [these models](https://jenkins.mc-stan.org/job/stanc3/job/master/)(listed under Test Results) on every pull request and think we are currently up to par and mostly backwards compatible with the previous Stan compiler (see [this wiki](https://github.com/stan-dev/stanc3/wiki/changes-from-stanc2) for a list of minor differences).

[![Build Status](http://d1m1s1b1.stat.columbia.edu:8080/job/stanc3/job/master/badge/icon)](http://d1m1s1b1.stat.columbia.edu:8080/job/stanc3/job/master/)

## High-level concepts, invariants, and 30,000-ft view
Stanc3 has 3 main src packages: `frontend`, `middle`, and `stan_math_backend`. The Middle contains the MIR and currently any types or functions used by the two ends.
The entrypoint for the compiler is in `src/stanc/stanc.ml` which sequences the various components together.

### Distinct Stanc Phases
1. [Lex](src/frontend/lexer.mll) the Stan language into tokens.
1. [Parse](src/frontend/parser.mly) Stan language into AST that represents the syntax quite closely and aides in development of pretty-printers and linters. `stanc --debug-ast` to print this out.
1. Typecheck & add type information [Semantic_check.ml](src/frontend/Semantic_check.ml).  `stanc --debug-decorated-ast`
1. [Desugaring phase](src/frontend/Desugar.ml) (AST -> AST). `stanc --debug-desugared`
1. [Lower](src/frontend/Ast_to_Mir.ml) into [Middle Intermediate Representation](src/middle/Mir.ml) (AST -> MIR) `stanc --debug-mir` (or `--debug-mir-pretty`)
1. Analyze & optimize (MIR -> MIR)
1. Backend MIR transform (MIR -> MIR) [Transform_Mir.ml](src/stan_math_backend/Transform_Mir.ml)  `stanc --debug-transformed-mir`
1. Hand off to a backend to [emit C++](src/stan_math_backend/Stan_math_code_gen.ml) (or LLVM IR, or Tensorflow, or interpret it!).

### The two central data structures

1. `src/frontend/Ast.ml` defines the AST. The AST is intended to have a direct 1-1 mapping with the syntax, so there are things like parentheses being kept around.
The pretty-printer in the frontend uses the AST and attempts to keep user syntax the same while just adjusting whitespace (maybe that's the wrong idea and we should move to a canonicalizer like `go fmt` soon; TBD). The AST uses a particular functional programming trick to add metadata to the AST (and its other tree types), sometimes called [the "two-level types" pattern](http://lambda-the-ultimate.org/node/4170#comment-63836). Essentially, many of the tree variant types are parameterized by something that ends up being a placeholder not for just metadata but for the recursive type including metadata, sometimes called the fixed point. So instead of recursively referencing `expression` you would instead reference type parameter `'e`, which will later be filled in with something like `type expr_with_meta = metadata expression`.
The AST intends to keep very close to Stan-level semantics and syntax in every way.
2. `src/middle/Mir.ml` contains the MIR (Middle Intermediate Language - we're saving room at the bottom for later). `src/frontend/Ast_to_Mir.ml` performs the lowering and attempts to strip out as much Stan-specific semantics and syntax as possible, though this is still something of a work-in-progress. The MIR uses the same two-level types pattern to add metadata, notably expression types and autodiff levels as well as locations on many things. The MIR is used as the output data type from the frontend and the input for dataflow analysis, optimization (which also outputs MIR), and code generation.

## Getting development on stanc3 up and running locally

### Using Opam+Make+Dune to build, test, and run
To be able to build the project, make sure you have `GNU make`.

To install OCaml and the dependencies we need to build and do development run the following from the stanc3 directory:

```
cd scripts
bash -x setup_dev_env.sh
```
Note that `curl` and `m4` are prerequisites to run the install script.

To build `stanc.exe`, run `make`. The binary will be built in `_build/default`.

To run tests, run `dune runtest` (and if there are changes you think are correct now, use `dune promote` to accept them).
To run e.g. only the integration tests, run `dune runtest test/integration`.

There are some git hooks in `scripts/hooks`; install with `bash scripts/hooks/install_hooks.sh`.

To auto-format the OCaml code (sadly, this does not work for the two ocamllex
and menhir files), run ` dune build @fmt ` or  `make format`.
To accept the changes proposed by ocamlformat, run `dune promote`.

Run `./_build/default/src/stanc/stanc.exe` on individual .stan file to compile it. Use `-?` to get command line options.

Use `dune build @update_messages` to see if your additions to the parser have added any new error message possibilities, and `dune promote` to accept them.

### Using Nix to build, test and run
[Nix](https://nixos.org/nix/) is a declarative package manager with a focus on reproducible builds.
You can use Nix to build, test and run Stanc3 without relying on Opam to manage the dependencies.

After you install nix, you can build Stanc3 by running the following command in the `stanc3` directory:

    nix-build

The binary will be in `result/bin/stanc`. It may take a minute the first time you run it.
Alternatively, the following is sometimes a faster way to build:

    nix-shell --command "dune build"

To run the test suite, run:

    nix-shell --command "dune build --profile release @runtest"

To install Stanc3 to your system, run:

    nix-env -i -f default.nix

To drop into a sandboxed development shell with all of the dependencies of Stanc3 plus packages for an OCaml development environment (`dune`, `ocp-indent`, `ocamlformat`, `merlin` and `utop`), run:

    nix-shell

To drop into a [UTop REPL](https://opam.ocaml.org/blog/about-utop/) with the Stanc3 modules available, run: 

    nix-shell --command "dune utop"

### Development on Windows
Having tried both native Windows development and development through [Ubuntu on WSL](https://www.microsoft.com/en-us/p/ubuntu-1804-lts/9n9tngvndl3q?activetab=pivot:overviewtab), the Ubuntu on WSL route seems vastly smoother and it is what we recommend as a default.
It's only downside seems to be that it builds Ubuntu, rather than Windows binaries.
If Windows binaries are preferred, [OCaml for Windows](https://fdopen.github.io/opam-repository-mingw/) can be used.

### Editor advice
For working on this project, we recommend using either VSCode or Emacs as an editor, due to their good OCaml support through Merlin: syntax highlighting, auto-completion, type inference, automatic case splitting, and more.
For people who prefer a GUI and have not memorized all Emacs or Vim keystrokes, VSCode might have the less steep learning curve.
Anything with Merlin support and keyboard shortcuts should be okay.

#### Setting up VSCode
Install instructions for VSCode can be found [here](https://code.visualstudio.com/docs/setup/setup-overview).

For Windows users: please note that we advise to follow the Linux install instructions through WSL.
Seeing that VSCode is a GUI application, you will need to install an XServer and add `export DISPLAY=:0.0` to `~/.bashrc`.
We recommend [Mobaxterm](https://mobaxterm.mobatek.net/).
In case you are using a high-res display, it may be worth overriding the high DPI setting of Mobaxterm (right click Mobaxterm binary > properties > Compatibility > Change high DPI settings > Override high DPI scaling behaviour > Application) and adding `export GDK_SCALE=3` or `export GDK_SCALE=2` to `~/.bashrc`.
We also advise setting `"window.titleBarStyle": "native"` in VSCode under settings to be able to have proper control over the window.

Once in VSCode (on any platform), simply install the [OCaml extension](https://github.com/reasonml-editor/vscode-reasonml) and you should be ready to go.

## Design goals
* **Multiple phases**, each with human-readable intermediate representations for easy debugging and optimization design.
* **Optimizing** - takes advantage of info known at the Stan language level. Minimize information we must teach users for them to write performant code.
* **Holistic-** bring as much of the code as possible into the MIR for whole-program optimization.
* **Research platform-** enable a new class of optimizations based on probability theory.
* **Modular** - architect & build in a way that makes it easy to outsource things like symbolic differentiation to external libraries and to use parts of the compiler as the basis for other tools built around the Stan language.
* **Simplicity first -** When making a choice between correct simplicity and a perceived performance benefit, we want to make the choice for simplicity unless we can show significant (> 5%) benchmark improvements to compile times or run times. Premature optimization is the root of all evil.
