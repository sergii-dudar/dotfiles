#!/usr/bin/env bash

cd ~/tools/tests/yazi || git clone https://github.com/sxyazi/yazi.git && cd ~/tools/tests/yazi || exit
cargo xtask build && mv target/release/yazi target/release/ya /Users/iuada144/.cargo/bin