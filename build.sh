#!/bin/sh

BUILD_DIR=${PWD}/build
INSTALL_DIR=${PWD}/install

mkdir ${BUILD_DIR} || exit
mkdir ${INSTALL_DIR} || exit

cd ${BUILD_DIR}
cmake .. \
  -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} \
  -DSPAMM_TESTING=yes \
  || exit
make || exit
make test || exit
make install || exit
