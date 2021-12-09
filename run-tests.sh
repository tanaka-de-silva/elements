#!/usr/bin/env bash

set -e

pushd compiler
cabal test --test-show-details=streaming
popd

pushd machine
cargo test
popd
