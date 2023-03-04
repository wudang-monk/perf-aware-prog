(defvar *word-registers* '("al" "cl" "dl" "bl" "ah" "ch" "dh" "bh"))
(defvar *byte-registers* '("ax" "cx" "dx" "bx" "sp" "bp" "si" "di"))

(defun split-opcode (number)
  (list :op-code (ldb (byte 6 2) number)
        :d (ldb (byte 1 1) number)
        :w (ldb (byte 1 0) number)))

(defun split-reg-mod-rm (number)
  (list :mod (ldb (byte 2 6) number)
        :reg (ldb (byte 3 3) number)
        :rm (ldb (byte 3 0) number)))

(defun write-asm-from-file (in-filename out-filename)
  (with-open-file (in-stream in-filename :element-type '(unsigned-byte 8) :direction :input :if-does-not-exist nil)
    (when in-stream
      (let ((content (make-array (file-length in-stream) :element-type 'unsigned-byte)))
        (read-sequence content in-stream)
        (with-open-file (out-stream out-filename :direction :output :if-exists :supersede :if-does-not-exist :create)
          (format out-stream ";;Filename: ~a~%bits 16~%" in-filename)
          (loop :for i :below (length content) :by 2
                :for command = (append (split-opcode (aref content i)) (split-reg-mod-rm (aref content (+ i 1))))
                :for registers = (if (eql (getf command :w) 0)
                                     *word-registers*
                                     *byte-registers*)
                :for reg = (nth (getf command :reg) registers)
                :for rm = (nth (getf command :rm) registers)
                :do (format out-stream "mov ~a, ~a~%" rm reg)))))))
