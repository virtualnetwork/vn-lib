################################################################################
# Filename          # config.inc
# Purpose           # Defines the Toolchain source versions/mirrors
# Copyright         # Copyright (C) Bruhnspace Advanced Projects AB (BAP) 2014
#                   # Copyright (C) 2011 Luke A. Guest, David Rees,
# Depends           # http://gcc.gnu.org/install/prerequisites.html
# Description       # 1) cp config-master.inc config.inc
#                   # 2) edit config.inc as required for your machine.
#                   # 3) ./build-tools.sh --help
################################################################################

export TOP=`pwd`
export SRC=$TOP/src

# For BAP ARM Ada bare metal toolchain
export INSTALL_DIR=/opt/gcc-4.8-arm

# REMOVE EVERYTHING TOOLCHAIN.