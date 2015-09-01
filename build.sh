#!/bin/sh

BUILD_DIR=${PWD}/build
INSTALL_DIR=${PWD}/install

mkdir -p ${BUILD_DIR} || exit
mkdir -p ${INSTALL_DIR} || exit

cd ${BUILD_DIR}
cmake .. \
  -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE:=Debug} \
  -DCMAKE_C_COMPILER=${CC:=gcc} \
  -DCMAKE_Fortran_COMPILER=${FC:=gfortran} \
  -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} \
  -DCMAKE_VERBOSE_MAKEFILE=yes \
  -DSPAMM_BLOCK_SIZE=${SPAMM_BLOCK_SIZE:=4} \
  -DSPAMM_CHUNK_SIZE=${SPAMM_CHUNK_SIZE:=32} \
  -DSPAMM_TESTING=yes \
  -DSPAMM_BENCHMARKS=yes \
  || exit
make || exit
ctest --verbose || exit
make install || exit
