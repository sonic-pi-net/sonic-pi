#!/bin/bash
if [ -d "node_modules" ]; then
    rm -r node_modules
fi

if [ -d ".npm" ]; then
    rm -r .npm
fi

if [ -d ".npmrc" ]; then
    rm -r .npmrc
fi
