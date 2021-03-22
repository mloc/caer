#!/bin/bash

rm -rf /drun
mkdir /drun
cp * /drun
cp ../target/debug/libmclient.so /lib
cd /drun
RUST_BACKTRACE=1 DreamDaemon client.dmb 8000 -trusted
