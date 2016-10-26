#!/usr/env/bin bash

ROOT=`dirname $0`
ROOT=`dirname $ROOT`

for i in $@; do
    cat $ROOT$i | xargs kill
done