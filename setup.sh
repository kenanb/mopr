#!/bin/bash

# NOTE: We don't activate the python environment needed by USD installation, if any.
#       It is assumed that this was done manually before any MOPR build and testing.

# Subdirectory to deploy build output.
export MOPR_OUT_DIR=out

# USD build related.
export MOPR_MONOLITHIC_USD=0
export MOPR_USD_ROOT=${HOME}/usr/pkg/usd/local

# Yoga lib related.
export MOPR_YOGA_LIB_DIR=${HOME}/usr/local/lib
export MOPR_YOGA_INC_DIR=${HOME}/usr/local/include

# On Debian family, multiarch lib directory for x86_64 is: /usr/lib/x86_64-linux-gnu
export MOPR_MULTIARCH_LIB_DIR=/usr/lib64

export MOPR_MAKE_ARGS=-j8

# export TF_DEBUG="PLUG_LOAD SDF_FILE_FORMAT SDF_LAYER"
