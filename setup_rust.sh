#!/bin/bash

curl https://sh.rustup.rs -sSf | sh
rustup update
rustup component add rustfmt-preview rust-src
cargo install racer
