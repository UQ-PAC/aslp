#!/bin/bash

# Expects to be run from asl-interpreter directory

TARGET="cpus/armv8.6.cpu"

echo "Extracting MRA specs"
rm -rf mra_tools
git clone https://github.com/alastairreid/mra_tools.git
cd mra_tools && mkdir -p v8.6 && cd v8.6
wget https://developer.arm.com/-/media/developer/products/architecture/armv8-a-architecture/2019-12/SysReg_xml_v86A-2019-12.tar.gz
wget https://developer.arm.com/-/media/developer/products/architecture/armv8-a-architecture/2019-12/A64_ISA_xml_v86A-2019-12.tar.gz
wget https://developer.arm.com/-/media/developer/products/architecture/armv8-a-architecture/2019-12/AArch32_ISA_xml_v86A-2019-12.tar.gz
tar zxf A64_ISA_xml_v86A-2019-12.tar.gz
tar zxf AArch32_ISA_xml_v86A-2019-12.tar.gz
tar zxf SysReg_xml_v86A-2019-12.tar.gz
cd .. 
make all 

echo "Marshalling to $TARGET"
cd ..
ASL_FILES="prelude.asl" 
ASL_FILES+=" ./mra_tools/arch/regs.asl"
ASL_FILES+=" ./mra_tools/types.asl"
ASL_FILES+=" ./mra_tools/arch/arch.asl"
ASL_FILES+=" ./mra_tools/arch/arch_instrs.asl"
ASL_FILES+=" ./mra_tools/arch/arch_decode.asl"
ASL_FILES+=" ./mra_tools/support/aes.asl"
ASL_FILES+=" ./mra_tools/support/barriers.asl"
ASL_FILES+=" ./mra_tools/support/debug.asl"
ASL_FILES+=" ./mra_tools/support/feature.asl"
ASL_FILES+=" ./mra_tools/support/hints.asl"
ASL_FILES+=" ./mra_tools/support/interrupts.asl"
ASL_FILES+=" ./mra_tools/support/memory.asl"
ASL_FILES+=" ./mra_tools/support/stubs.asl"
ASL_FILES+=" ./mra_tools/support/fetchdecode.asl"
ASL_FILES+=" tests/override.asl"
ASL_FILES+=" tests/override.prj"
echo ":stash $TARGET" | dune exec asli $ASL_FILES
