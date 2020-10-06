#!/usr/bin/env bash

build_package() {
    source=$1
    target=$2
    cd $source
    npm run webpack
    cd ..

    mv $source/dist/main.js js/$target.js
}

build_package api proc-api
build_package xterm xterm
