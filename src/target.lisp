(in-package :llvm)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *targets* '("MBlaze" "CppBackend" "MSIL" "CBackend" "Blackfin"
                      "SystemZ" "MSP430" "XCore" "PIC16" "CellSPU" "Mips" "ARM"
                      "Alpha" "PowerPC" "Sparc" "X86"))
  (defvar *target-info-functions* nil)
  (defvar *target-mc-functions* nil)
  (defvar *target-functions* nil)
  (defvar *asm-printer-functions* nil)
  (defvar *asm-parser-functions* nil)
  (defvar *disassembler-functions* nil))

(defmacro declare-targets ()
  `(progn ,@(mapcan (lambda (target)
                      `((push (defcfun* ,(format nil
                                                 "LLVMInitialize~aTargetInfo"
                                                 target)
                                  :void)
                              *target-info-functions*)
                        (push (defcfun* ,(format nil "LLVMInitialize~aTarget"
                                                 target)
                                  :void)
                              *target-functions*)
                        (push (defcfun* ,(format nil "LLVMInitialize~aTargetMC"
                                                 target)
                                  :void)
                              *target-mc-functions*)
                        (push (defcfun* ,(format nil
                                                 "LLVMInitialize~aAsmPrinter"
                                                 target)
                                  :void)
                              *asm-printer-functions*)
                        (push (defcfun* ,(format nil
                                                 "LLVMInitialize~aAsmParser"
                                                 target)
                                  :void)
                              *asm-parser-functions*)
                        (push (defcfun* ,(format nil
                                                 "LLVMInitialize~aDisassembler"
                                                 target)
                                  :void)
                              *disassembler-functions*)))
                    *targets*)))

(declare-targets)
(export *target-info-functions*)
(export *target-functions*)

(defun initialize-all-target-infos ()
  (mapc #'funcall *target-info-functions*)
  (values))

(defun initialize-all-targets ()
  (mapc #'funcall *target-functions*)
  (values))

(defun initialize-native-target ()
  #+mips (progn (initialize-mips-target-info) (initialize-mips-target) t)
  #+alpha (progn (initialize-alpha-target-info) (initialize-alpha-target) t)
  #+(or ppc ppc64)
  (progn (initialize-powerpc-target-info) (initialize-powerpc-target) t)
  #+(or sparc sparc64)
  (progn (initialize-sparc-target-info) (initialize-sparc-target) t)
  #+(or x86 x86-64)
  (progn
    (initialize-x86-target-info)
    (initialize-x86-target)
    (initialize-x86-target-m-c)
    (initialize-x86-asm-printer)
    (initialize-x86-asm-parser)
    (initialize-x86-disassembler)
    t)
  #-(or mips alpha ppc ppc64 sparc sparc64 x86 x86-64) nil)

(defcfun* "LLVMCreateTargetData" target-data (string-rep :string))

(defcfun* "LLVMAddTargetData" :void
  (target-data target-data) (pass-manager pass-manager))

(defcfun* "LLVMCopyStringRepOfTargetData" (:pointer :char)
  (target-data target-data))
(defun string-representation (target-data)
  (let ((message (copy-string-rep-of-target-data target-data)))
    (prog1
        (mem-ref message :string)
      (dispose-message message))))

(defcfun* "LLVMByteOrder" byte-ordering (target-data target-data))

(defcfun* "LLVMPointerSize" :unsigned-int (target-data target-data))

(defcfun (int-pointer-type "LLVMIntPtrType") llvm-type (target-data target-data))

(defcfun* "LLVMSizeOfTypeInBits" :unsigned-long-long
  (target-data target-data) (type llvm-type))

(defcfun (storage-size-of-type "LLVMStoreSizeOfType") :unsigned-long-long
  (target-data target-data) (type llvm-type))

(defcfun* "LLVMABISizeOfType" :unsigned-long-long
  (target-data target-data) (type llvm-type))

(defcfun* "LLVMABIAlignmentOfType" :unsigned-int
  (target-data target-data) (type llvm-type))

(defcfun* "LLVMCallFrameAlignmentOfType" :unsigned-int
  (target-data target-data) (type llvm-type))

(defcfun* "LLVMPreferredAlignmentOfType" :unsigned-int
  (target-data target-data) (type llvm-type))

(defcfun* "LLVMPreferredAlignmentOfGlobal" :unsigned-int
  (target-data target-data) (global-var value))

(defcfun* "LLVMElementAtOffset" :unsigned-int
  (target-data target-data) (struct-ty llvm-type) (offset :unsigned-long-long))

(defcfun* "LLVMOffsetOfElement" :unsigned-long-long
  (target-data target-data) (struct-ty llvm-type) (element :unsigned-int))

(defcfun* "LLVMInvalidateStructLayout" :void
  (target-data target-data) (struct-ty llvm-type))

(defcfun* "LLVMDisposeTargetData" :void (target-data target-data))
