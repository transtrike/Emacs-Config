#!/bin/bash
rm -rf init.el etc/ var/ auto-save/ elpa/ straight/
./tangle-config.sh
