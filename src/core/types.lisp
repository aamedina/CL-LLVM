(in-package :llvm)

(defcfun (kind "LLVMGetTypeKind") type-kind
  (ty llvm-type))

(defcfun (context "LLVMGetTypeContext") context
  (ty llvm-type))

(defcfun (dump-type "LLVMDumpType") :void
  (ty llvm-type))

(defcfun (print-type "LLVMPrintTypeToString") :string
  (ty llvm-type))

(defcfun* "LLVMInt1TypeInContext" llvm-type (c context))
(defun int1-type (&key (context (global-context)))
  (int1-type-in-context context))
(defcfun* "LLVMInt8TypeInContext" llvm-type (c context))
(defun int8-type (&key (context (global-context)))
  (int8-type-in-context context))
(defcfun* "LLVMInt16TypeInContext" llvm-type (c context))
(defun int16-type (&key (context (global-context)))
  (int16-type-in-context context))
(defcfun* "LLVMInt32TypeInContext" llvm-type (c context))
(defun int32-type (&key (context (global-context)))
  (int32-type-in-context context))
(defcfun* "LLVMInt64TypeInContext" llvm-type (c context))
(defun int64-type (&key (context (global-context)))
  (int64-type-in-context context))
(defcfun* "LLVMIntTypeInContext" llvm-type (c context)
  (num-bits :unsigned-int))
(defun int-type (num-bits &key (context (global-context)))
  (int-type-in-context context num-bits))

(defcfun (width "LLVMGetIntTypeWidth") :unsigned-int (integer-ty llvm-type))

(defcfun* "LLVMFloatTypeInContext" llvm-type (c context))
(defun float-type (&key (context (global-context)))
  (float-type-in-context context))
(defcfun* "LLVMDoubleTypeInContext" llvm-type (c context))
(defun double-type (&key (context (global-context)))
  (double-type-in-context context))
(defcfun* "LLVMX86FP80TypeInContext" llvm-type (c context))
(defun x86-fp80-type (&key (context (global-context)))
  (x86-fp80-type-in-context context))
(defcfun* "LLVMFP128TypeInContext" llvm-type (c context))
(defun fp128-type (&key (context (global-context)))
  (fp128-type-in-context context))
(defcfun (ppc-fp128-type-in-context "LLVMPPCFP128TypeInContext") llvm-type
  (c context))
(defun ppc-fp128-type (&key (context (global-context)))
  (ppc-fp128-type-in-context context))

(defcfun (%function-type "LLVMFunctionType") llvm-type
  (return-type llvm-type)
  (param-types (carray llvm-type)) (param-count :unsigned-int)
  (is-var-arg :boolean))
(defun function-type (return-type param-types &key var-arg-p)
  (%function-type return-type param-types (length param-types) var-arg-p))

(defcfun (function-var-arg-p "LLVMIsFunctionVarArg") :boolean
  (function-ty llvm-type))
(defcfun (return-type "LLVMGetReturnType") llvm-type (function-ty llvm-type))
(defcfun* "LLVMCountParamTypes" :unsigned-int (function-ty llvm-type))
(defcfun* "LLVMGetParamTypes" :void
  (function-ty llvm-type)
  (dest (:pointer llvm-type)))
(defun param-types (function-ty)
  (with-pointer-to-list (pointer llvm-type (count-param-types function-ty))
    (get-param-types function-ty pointer)))

(defcfun* "LLVMStructTypeInContext" llvm-type
  (c context)
  (element-types (carray llvm-type)) (element-count :unsigned-int)
  (packed :boolean))
(defun struct-type (element-types packed &key (context (global-context)))
  (struct-type-in-context context element-types (length element-types) packed))
(defcfun* "LLVMCountStructElementTypes" :unsigned-int (struct-ty llvm-type))
(defcfun* "LLVMGetStructElementTypes" :void
  (struct-ty llvm-type) (dest (:pointer llvm-type)))
(defun struct-element-types (struct-ty)
  (with-pointer-to-list
      (pointer llvm-type (count-struct-element-types struct-ty))
    (get-struct-element-types struct-ty pointer)))
(defcfun (packed-struct-p "LLVMIsPackedStruct") :boolean (struct-ty llvm-type))
(defcfun (opaque-struct-p "LLVMIsOpaqueStruct") :boolean (struct-ty llvm-type))
(defcfun* "LLVMStructCreateNamed" llvm-type
  (c context)
  (name :string))
(defcfun* "LLVMGetStructName" :string
  (struct llvm-type))
(defcfun (%struct-set-body "LLVMStructSetBody") :void
  (struct llvm-type)
  (element-types (carray llvm-type)) (element-count :unsigned-int)
  (packed :boolean))
(defun struct-set-body (struct-type element-types &optional (packed nil))
  (%struct-set-body struct-type element-types (length element-types) packed))

(defcfun* "LLVMArrayType" llvm-type
  (element-type llvm-type) (element-count :unsigned-int))
(defcfun (%pointer-type "LLVMPointerType") llvm-type
  (element-type llvm-type) (address-space :unsigned-int))
(defun pointer-type (element-type &optional (address-space 0))
  (%pointer-type element-type address-space))
(defcfun* "LLVMVectorType" llvm-type
  (element-type llvm-type) (element-count :unsigned-int))

(defcfun (element-type "LLVMGetElementType") llvm-type (ty llvm-type))
(defcfun (array-length "LLVMGetArrayLength") :unsigned-int (array-ty llvm-type))
(defcfun (address-space "LLVMGetPointerAddressSpace") :unsigned-int
  (pointer-ty llvm-type))
(defcfun (size "LLVMGetVectorSize") :unsigned-int (vector-ty llvm-type))

(defcfun* "LLVMVoidTypeInContext" llvm-type (c context))
(defun void-type (&key (context (global-context)))
  (void-type-in-context context))
(defcfun* "LLVMLabelTypeInContext" llvm-type (c context))
(defun label-type (&key (context (global-context)))
  (label-type-in-context context))
(defcfun* "LLVMOpaqueTypeInContext" llvm-type (c context))
(defun opaque-type (&key (context (global-context)))
  (opaque-type-in-context context))
