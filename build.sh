#!/bin/bash

export MOPR_ROOT=`realpath $0 | xargs -0 dirname`

source ${MOPR_ROOT}/setup.sh
echo MOPR_ROOT = $MOPR_ROOT
if [ -z "${MOPR_USD_ROOT}" ]
then
      echo "\$MOPR_USD_ROOT is empty. Skipping assignment of runtime env variables."
else
      echo "\$MOPR_USD_ROOT is set. Appending directories to environment variables."
      export PATH=${MOPR_USD_ROOT}/bin:$PATH
      export PYTHONPATH=${MOPR_USD_ROOT}/lib/python/:$PYTHONPATH
      export LD_LIBRARY_PATH=${MOPR_USD_ROOT}/lib:$LD_LIBRARY_PATH
      export LD_PRELOAD=${MOPR_MULTIARCH_LIB_DIR}/libjemalloc.so
fi

# Pass -pn to debug.
# TF_DEBUG="PLUG_LOAD SDF_FILE_FORMAT SDF_LAYER"
(cd $MOPR_ROOT && make ${MOPR_MAKE_ARGS} "$@")
