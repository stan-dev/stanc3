# Nix

At one time, these scripts allowed you to build with Nix. They are no longer maintained

The previous documentation of this option:
```odoc
{2:nix Alternative: Using Nix}

{{:https://nixos.org/nix/}Nix} is a declarative package manager with a focus on reproducible builds.
We provide the ability to use Nix to build, test and run Stanc3. We recommend trying the [opam]
instructions first if you are not an existing Nix user, with these as a backup.

If you have nix installed, you can build Stanc3 by running the following command in the [stanc3] directory:

[nix-build]

The binary will be in [result/bin/stanc]. It may take a minute the first time you run it.
Alternatively, the following is sometimes a faster way to build:

[nix-shell --command "dune build"]

To run the test suite, run:

[nix-shell --command "dune build --profile release @runtest"]

To install Stanc3 to your system, run:

[nix-env -i -f default.nix]

To drop into a sandboxed development shell with all of the build dependencies
of Stanc3 plus packages for an OCaml development environment
([dune], [ocp-indent], [ocamlformat], [merlin] and [utop]), run:

[nix-shell]

```
