(in-package :llvm)

(cc-flags "-DNDEBUG"
          "-D_GNU_SOURCE"
          "-D__STDC_CONSTANT_MACROS"
          "-D__STDC_FORMAT_MACROS"
          "-D__STDC_LIMIT_MACROS"
          "-I/usr/local/Cellar/llvm/HEAD/include"
          "-O3"
          "-fno-common")
(include "/usr/local/opt/llvm/include/llvm-c/Target.h")

(cenum byte-ordering
       ((:big-endian "LLVMBigEndian"))
       ((:little-endian "LLVMLittleEndian")))
