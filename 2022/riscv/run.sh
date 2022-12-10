#!/bin/bash

set -e

cmd="$2"

echo $cmd > .cmd.sh

riscv64-linux-gnu-as -o main.o $1
riscv64-linux-gnu-ld -o main main.o
temu diskimage-linux-riscv-2018-09-23/root_9p-riscv64.cfg # | sed -n '/reboot: Power down/q;p'
