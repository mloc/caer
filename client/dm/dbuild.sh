#!/bin/bash

mkdir /dbuild
cp * /dbuild
pushd /dbuild
DreamMaker client.dme
popd
cp /dbuild/client.dmb .
