set -eux

cargo build --release
mkdir -p $HOME/.deliver/bin/

cp ../../target/release/deliver $HOME/.deliver/bin


$HOME/.deliver/bin/deliver toolchain setup

