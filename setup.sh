#!/bin/bash

# Subdirectory to deploy build output.
export MOPR_OUT_DIR=out

# USD build related.
export MOPR_MONOLITHIC_USD=0
export MOPR_USD_ROOT=${HOME}/usr/pkg/usd/local

# Yoga lib related.
export MOPR_YOGA_LIB_DIR=${HOME}/usr/local/lib
export MOPR_YOGA_INC_DIR=${HOME}/usr/local/include

export MOPR_MAKE_ARGS=-j8

# export TF_DEBUG="PLUG_LOAD SDF_FILE_FORMAT SDF_LAYER"
