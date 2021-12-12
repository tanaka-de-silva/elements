#!/usr/bin/env bash

set -e

brittany --write-mode=inplace app/*.hs
brittany --write-mode=inplace src/**/*.hs
brittany --write-mode=inplace test/**/*.hs
