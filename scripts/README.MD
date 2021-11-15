# Windows Build Environment

### Requirements

  - Windows version that supports WSL
  - Internet Access

### Setup the WSL using Ubuntu 18.04

1. Enable WSL on your machine
- Windows 10 ( https://docs.microsoft.com/en-us/windows/wsl/install-win10 )
- Windows Server ( https://docs.microsoft.com/en-us/windows/wsl/install-on-server )
2. Reboot your machine
3. [Download `Ubuntu 18.04`](https://aka.ms/wsl-ubuntu-1804) (  or [choose another distro](https://docs.microsoft.com/en-us/windows/wsl/install-manual) )
4. Extract the downloaded archive
5. Run `ubuntu.exe` if you're going with `Ubuntu 18.04`
6. Wait for the installation to complete
7. Open a new `cmd` and run `bash` you should now enter the Ubuntu WSL

### Installing build dependencies ( inside the Ubuntul 10.04 WSL )

1. Add the opam apt-repository and update the repositories
```
   sudo add-apt-repository ppa:avsm/ppa
   sudo apt-get update
```
2. Install `opam` `mingw-w64` `gcc` `m4` `make` dependencies required
```
   sudo apt-get install opam mingw-w64 gcc m4 make -y
```
3. Init opam, create and switch to 4.12.0, update environment shell

If your distro supports `bwrap`
```
opam init
```
If your distro does not support `bwrap`
```
opam init --disable-sandboxing -y
```
```
   opam switch create 4.12.0
   opam switch 4.12.0
   eval $(opam env)
```
4. Add the opam repository for the cross-compile windows
```
opam repository add windows git://github.com/ocaml-cross/opam-cross-windows
```
5. Request the compiler to be built with flambda optimizers:
```
   opam install conf-flambda-windows
```
6. Install the compiler
```
   opam install "ocaml-windows64<=4.12.0"
```
7. Install opam packages that are required for build/runtest
```
   opam install -y core_kernel-windows menhir-windows ppx_deriving-windows fmt-windows
```
8. Install dos2unix for running the Windows tests on Jenkins
```
   sudo apt-get install dos2unix
```

### Test the environment

Switch to the root of the project and run
```
   eval $(opam env) make clean
   dune build -x windows
   dune runtest
```

#############################################################################################################################

# Linux Build Environment

1. Add the opam apt-repository and update the repositories
```
   sudo add-apt-repository ppa:avsm/ppa
   sudo apt-get update
```
2. Install `opam` `mingw-w64` `gcc` `m4` `make` dependencies required
```
   sudo apt-get install opam mingw-w64 gcc m4 make -y
```
3. Init opam, create and switch to 4.12.0, update environment shell
```
   opam init --disable-sandboxing -y
   opam switch create 4.12.0
   opam switch 4.12.0
   eval $(opam env)
```
4. Install opam packages that are required for build/runtest
```
opam install -y core_kernel menhir ppx_deriving fmt
```

### Test the environment

Switch to the root of the project and run
```
   eval $(opam env)
   dune build @install
   dune runtest
```




