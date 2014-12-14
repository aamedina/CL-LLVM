(in-package :llvm)

(defcfun* "LLVMContextCreate" context)
(defcfun (global-context "LLVMGetGlobalContext") context)
(defcfun (dispose-context "LLVMContextDispose") :void (c context))

(defcfun* "LLVMModuleCreateWithNameInContext" module
  (module-id :string) (c context))
(defun make-module (name &optional (context (global-context)))
  (module-create-with-name-in-context name context))

(defcfun* "LLVMDisposeModule" :void (m module))

(defcfun (data-layout "LLVMGetDataLayout") :string (m module))
(defcfun* "LLVMSetDataLayout" :void (m module) (triple :string))
(defun (setf data-layout) (triple m)
  (set-data-layout m triple)
  triple)

(defcfun (target "LLVMGetTarget") :string (m module))
(defcfun* "LLVMSetTarget" :void (m module) (triple :string))
(defun (setf target) (triple m)
  (set-target m triple)
  triple)

(defcfun* "LLVMAddTypeName" :int (m module) (name :string) (ty llvm-type))
(defcfun* "LLVMDeleteTypeName" :void (m module) (name :string))
(defcfun* "LLVMGetTypeByName" llvm-type (m module) (name :string))

(defcfun (%dump-module "LLVMDumpModule") :void (m module))
(defun dump-module (m)
  (finish-output *error-output*)
  (%dump-module m))

(defcfun (print-module "LLVMPrintModuleToString") :string
  (m module))

(defcfun (spit-module "LLVMPrintModuleToFile") :boolean
  (m module))


