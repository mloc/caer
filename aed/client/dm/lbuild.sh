#!/bin/sh

docker run --rm -v /c/Users/mloc/desktop/devel/mick/client:/work -ti -w "/work/dm" mloc6/byond:512 ./dbuild.sh
