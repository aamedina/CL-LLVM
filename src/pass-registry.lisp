(in-package :llvm)

(defcfun* "LLVMInitializeCore" :void
  (r pass-registry))

(defcfun* "LLVMInitializeTransformUtils" :void
  (r pass-registry))

(defcfun* "LLVMInitializeScalarOpts" :void
  (r pass-registry))

(defcfun* "LLVMInitializeVectorization" :void
  (r pass-registry))

(defcfun* "LLVMInitializeInstCombine" :void
  (r pass-registry))

(defcfun* "LLVMInitializeIPO" :void
  (r pass-registry))

(defcfun* "LLVMInitializeInstrumentation" :void
  (r pass-registry))

(defcfun* "LLVMInitializeAnalysis" :void
  (r pass-registry))

(defcfun* "LLVMInitializeIPA" :void
  (r pass-registry))

(defcfun* "LLVMInitializeCodeGen" :void
  (r pass-registry))

(defcfun* "LLVMInitializeTarget" :void
  (r pass-registry))

(defun initialize-system ()
  (let ((global-registry (global-pass-registry)))
    (initialize-core global-registry)
    (initialize-transform-utils global-registry)
    (initialize-scalar-opts global-registry)
    (initialize-vectorization global-registry)
    (initialize-inst-combine global-registry)
    (initialize-i-p-o global-registry)
    (initialize-instrumentation global-registry)
    (initialize-analysis global-registry)
    (initialize-i-p-a global-registry)
    (initialize-code-gen global-registry)
    (initialize-target global-registry)))


