#!/usr/bin/env bash

set -e

pushd compiler
cabal run elements-compiler:compiler-exe ../examples/simple.el
popd

pushd machine
cargo run ../examples/simple.json
popd
