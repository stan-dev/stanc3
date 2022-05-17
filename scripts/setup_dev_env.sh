#!/bin/bash

# exit when any command fails
set -e

if ! command -v opam &> /dev/null
then
    echo "OPAM not detected, installing now."
    bash -x ./install_opam.sh
else
    echo "OPAM already installed, not re-installing."
fi

bash -x ./install_ocaml.sh
bash -x ./install_build_deps.sh


read -p "Do you want to install developer utilities for formatting and testing? (y/n) " yndev

case $yndev in
	[yY] ) bash -x ./install_dev_deps.sh
		;;
	* ) echo "Continuing without dev dependencies, run ./install_dev_deps.sh to do this at a later time."
esac

read -p "Do you want to install a git pre-commit hook to run the formatter before commiting? (y/n) " ynhook

case $ynhook in
	[yY] ) bash -x ./hooks/install_hooks.sh
		;;
	* ) echo "Continuing without pre-commit hook, run ../hooks/install_hooks.sh to do this at a later time."
esac

echo "Do you want to install the tools to cross compile Windows binaries?"
echo "This requires that you have installed the gcc-mingw-w64-x86-64 package ahead of time."
read -p "Install Windows cross compilation tools? (y/n) " ynwindows

case $ynwindows in
	[yY] ) bash -x ./install_build_deps_windows.sh
		;;
	* ) echo "Continuing without Windows cross compilation tools, run ./install_build_deps_windows.sh to do this at a later time."
esac

echo "Do you want to install the tools to compile the Javascript version of stanc3?"
read -p "Install Javascript compilation tools? (y/n) " ynjs

case $ynjs in
	[yY] ) bash -x ./install_js_deps.sh
		;;
	* ) echo "Continuing without Javascript compilation tools, run ./install_js_deps.sh to do this at a later time."
esac

eval $(opam env)
