(in-package :llvm)

(cc-flags "-DNDEBUG"
          "-D_GNU_SOURCE"
          "-D__STDC_CONSTANT_MACROS"
          "-D__STDC_FORMAT_MACROS"
          "-D__STDC_LIMIT_MACROS"
          "-I/usr/local/Cellar/llvm/HEAD/include"
          "-O3"
          "-fno-common")
(include "/usr/local/opt/llvm/include/llvm-c/TargetMachine.h")

(cenum opt-level 
       ((:none "LLVMCodeGenLevelNone"))
       ((:less "LLVMCodeGenLevelLess"))
       ((:default "LLVMCodeGenLevelDefault"))
       ((:aggressive "LLVMCodeGenLevelAggressive")))

(cenum reloc-mode
       ((:default "LLVMRelocDefault"))
       ((:static "LLVMRelocStatic"))
       ((:position-independent "LLVMRelocPIC"))
       ((:dynamic "LLVMRelocDynamicNoPic")))

(cenum code-model
       ((:default "LLVMCodeModelDefault"))
       ((:jit "LLVMCodeModelJITDefault"))
       ((:small "LLVMCodeModelSmall"))
       ((:kernel "LLVMCodeModelKernel"))
       ((:medium "LLVMCodeModelMedium"))
       ((:large "LLVMCodeModelLarge")))

(cenum file-type
       ((:assembly-file "LLVMAssemblyFile"))
       ((:object-file "LLVMObjectFile")))
