#!/usr/env/bin bash

ROOT=`dirname $0`
ROOT=`dirname $ROOT`

TARGET=$ROOT$1
ARGS=(${@/$1/})

rm -f $TARGET


for i in ${ARGS[@]}; do
    if test -f $ROOT$i; then
        cat $ROOT$i >> $TARGET
        echo "" >> $TARGET
    fi
done

echo 'OK'