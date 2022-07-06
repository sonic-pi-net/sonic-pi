#!/bin/bash

python configure.py --generator Ninja
python build.py
python run-tests.py --target LinkCoreTest --valgrind
python run-tests.py --target LinkDiscoveryTest --valgrind
