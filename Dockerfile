FROM ocaml/opam:ubuntu-22.04-ocaml-4.09

# Install system dependencies
USER root
RUN apt-get update && apt-get install -y python3 libgmp-dev yasm m4 wget

# Install asli dependencies
USER opam
RUN opam install menhir ocamlfind ott.0.32 linenoise pprint z3.4.11.0 zarith alcotest dune mmap

# Build some more asli dependencies
RUN eval $(opam env) \
    && export LD_LIBRARY_PATH=`opam var z3:lib` \
    && git clone https://github.com/alastairreid/mra_tools.git \
    && git clone -b partial_eval https://github.com/UQ-PAC/asl-interpreter.git \
    && make -C asl-interpreter asli \
    && cd mra_tools && mkdir -p v8.6 && cd v8.6 \
    && wget https://developer.arm.com/-/media/developer/products/architecture/armv8-a-architecture/2019-12/SysReg_xml_v86A-2019-12.tar.gz \
    && wget https://developer.arm.com/-/media/developer/products/architecture/armv8-a-architecture/2019-12/A64_ISA_xml_v86A-2019-12.tar.gz \
    && wget https://developer.arm.com/-/media/developer/products/architecture/armv8-a-architecture/2019-12/AArch32_ISA_xml_v86A-2019-12.tar.gz \
    && tar zxf A64_ISA_xml_v86A-2019-12.tar.gz \
    && tar zxf AArch32_ISA_xml_v86A-2019-12.tar.gz \
    && tar zxf SysReg_xml_v86A-2019-12.tar.gz \
    && cd .. && make all 

# Build asli
RUN eval $(opam env) && cd asl-interpreter && dune build

# Install bap dependencies
RUN git clone -b a64-lifter-plugin https://github.com/UQ-PAC/bap.git \
    && cd bap && opam pin add . -n && opam depext bap \
    && opam install bap --deps-only && opam pin remove bap

# Build bap
RUN cd bap && eval $(opam env) && ./configure --enable-everything \
    --disable-ghidra --prefix=`opam var prefix` \
    --with-llvm-version=9 --with-llvm-config=llvm-config-9 \
    && make && make reinstall

# Create asli package
RUN cd asl-interpreter && opam pin add . -k path

# Build a64 bap plugin
RUN cd bap/plugins/a64 && eval $(opam env) \
	&& bapbuild -package asli.libASL a64_main.plugin \
	&& mv a64_main.plugin a64.plugin \
	&& bapbundle install a64.plugin

# Unpack encodings to be used for coverage testing
RUN cd asl-interpreter && tar -xzvf encodings.tar.gz

CMD ["bash"]
