#!/bin/bash

set -euf -o pipefail

if [ "$#" -ne 1 ]; then
    echo "gib fn"
    exit 1
fi

./graph_cfg.sh

cd dbgout/dot

xdot tino_cfg/cfg_${1}.dot &
sleep 0.5
xdot tino_cfg/cfg_opt_${1}.dot &
sleep 0.5
xdot llvm_unopt/.${1}.dot &
sleep 0.5
xdot llvm_opt/.${1}.dot &
