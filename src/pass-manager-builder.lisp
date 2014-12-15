(in-package :llvm)

(defctype pass-manager-builder :pointer)

(defcfun (make-pass-manager-builder "LLVMPassManagerBuilderCreate")
    pass-manager-builder)

(defcfun (dispose-pass-manager-builder "LLVMPassManagerBuilderDispose") :void
  (pmd pass-manager-builder))

(defcfun (set-pmd-opt-level "LLVMPassManagerBuilderSetOptLevel") :void
  (pmd pass-manager-builder))

(defcfun (set-pmd-size-level "LLVMPassManagerBuilderSetSizeLevel") :void
  (pmd pass-manager-builder))

