(in-package :decoder)

;; (defvar *byte-registers* '("al" "cl" "dl" "bl" "ah" "ch" "dh" "bh"))
;; (defvar *word-registers* '("ax" "cx" "dx" "bx" "sp" "bp" "si" "di"))

;; (defun get-reg (byte1 byte2)
;;   (let ((regs (split-byte2 byte2))
;;         (wide (ldb (byte 1 0) byte1)))
;;     (if (= wide 1)
;;         (list (nth (getf regs :reg) *word-registers*) (nth (getf regs :rm) *word-registers*))
;;         (list (nth (getf regs :reg) *byte-registers*) (nth (getf regs :rm) *byte-registers*)))))

;; (defun split-instruct (number)
;;   (list :op-code (ldb (byte 6 2) number)
;;         :d (ldb (byte 1 1) number)
;;         :w (ldb (byte 1 0) number)
;;         :mod (ldb (byte 2 14) number)
;;         :reg (ldb (byte 3 11) number)
;;         :rm (ldb (byte 3 8) number)))

;; (defun split-byte2 (byte)
;;   (list :mod (ldb (byte 8 6) byte)
;;         :reg (ldb (byte 3 3) byte)
;;         :rm (ldb (byte 3 0) byte)))

;; (defun write-asm-from-file (in-filename out-filename)
;;   (with-open-file (in-stream in-filename :element-type '(unsigned-byte 16) :direction :input :if-does-not-exist nil)
;;     (when in-stream
;;       (let ((content (make-array (file-length in-stream) :element-type '(unsigned-byte 16))))
;;         (read-sequence content in-stream)
;;         (with-open-file (out-stream out-filename :direction :output :if-exists :supersede :if-does-not-exist :create)
;;           (format out-stream ";;Filename: ~a~%bits 16~%" in-filename)
;;           (loop :for instruction :across content
;;                 :for command = (split-instruct instruction)
;;                 :for registers = (if (eql (getf command :w) 0)
;;                                      *word-registers*
;;                                      *byte-registers*)
;;                 :for reg = (nth (getf command :reg) registers)
;;                 :for rm = (nth (getf command :rm) registers)
;;                 :do (format out-stream "mov ~a, ~a~%" rm reg)))))))


(defvar *decoder-binary* "/home/tlaloc/Documents/Programming/Performance-Aware-Programming/inst-decode-8086/sim86")
(defvar *listings-directory* "/home/tlaloc/Documents/Programming/Performance-Aware-Programming/inst-decode-8086/listings/")


(defun file-bytes (filename)
  (with-open-file (in-stream filename :element-type '(unsigned-byte 8) :direction :input :if-does-not-exist nil)
    (if in-stream
        (let ((content (make-array (file-length in-stream) :element-type '(unsigned-byte 8))))
        (read-sequence content in-stream)
          content)
        (format t "File NOT found: ~a~%" filename))))

(defun file-diff (file-a file-b)
  (let ((bytes-a (file-bytes file-a))
        (bytes-b (file-bytes file-b)))
    (when (and bytes-a bytes-b)
      (loop :for i :from 0 :to (length bytes-a)
            :for byte1 :across bytes-a
            :for byte2 :across bytes-b
            :unless (= byte1 byte2)
              :collect (list i byte1 byte2) :into diff-bytes
            :finally (return diff-bytes)))))

(defun bytes-diff (bytes-a bytes-b)
  (when (and bytes-a bytes-b)
    (loop :for i :from 0 :to (length bytes-a)
          :for byte1 :across bytes-a
          :for byte2 :across bytes-b
          :unless (= byte1 byte2)
            :collect (list i byte1 byte2) :into diff-bytes
          :finally (return diff-bytes))))


(defun nasm-compile (file)
  (sb-ext:run-program "/usr/bin/nasm" (list file) :output *standard-output*))

(defun valid-truename-p (path)
  (handler-case
      (progn
        (truename path)
        t)
    (file-error ()
      (format t "FILE NOT found: Invalid file path: ~a~%" path))))

(defun decode-diff (file &key (out-dir (concatenate 'string *listings-directory* "asm-out")))
  (let* ((test-file (format nil "~a/~a-decode" out-dir file))
         (asm-out (format nil "~a.asm" test-file))
         ;; (bin-file-path (pathname out-dir))
         (file-fullpath (concatenate 'string *listings-directory* file)))
    (sb-unix:unix-mkdir out-dir #o755)
    (sb-ext:run-program *decoder-binary* (list file-fullpath asm-out))
    (nasm-compile asm-out)
    (file-diff file-fullpath test-file)
    ))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
     ,@(loop :for f :in forms :collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (let ((result (gensym)))
    `(let ((,result t))
       ,@(loop :for f :in forms :collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro deftest (name args &body body)
  "Define a test function. Within a test functions we can call other test functions or use 'check' to run individual test cases."
  `(defun ,name ,args
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defvar *test-name* nil)

(defun report-result (result test)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAILED~;passed~] ... ~a: ~a~%" result *test-name* test)
  result)

(deftest test-listings ()
  (check
    (not (decode-diff "listing_0037_single_register_mov"))
    (not (decode-diff "listing_0037_single_register_mov"))
    (not (decode-diff "listing_0038_many_register_mov"))
    (not (decode-diff "listing_0040_challenge_movs"))
    (not (decode-diff "listing_0041_add_sub_cmp_jnz"))
    ;; (not (decode-diff "listing_0042_completionist_decode"))
    (not (decode-diff "listing_0043_immediate_movs"))
    (not (decode-diff "listing_0044_register_movs"))
    (not (decode-diff "listing_0045_challenge_register_movs"))
    (not (decode-diff "listing_0046_add_sub_cmp"))
    (not (decode-diff "listing_0047_challenge_flags"))
    (not (decode-diff "listing_0048_ip_register"))
    (not (decode-diff "listing_0049_conditional_jumps"))
    (not (decode-diff "listing_0050_challenge_jumps"))
    (not (decode-diff "listing_0051_memory_mov"))
    (not (decode-diff "listing_0052_memory_add_loop"))
    (not (decode-diff "listing_0053_add_loop_challenge"))
    (not (decode-diff "listing_0054_draw_rectangle"))
    (not (decode-diff "listing_0055_challenge_rectangle"))
    ))

(defun compile-asm-files ()
  "Compiles all the asm files in a given directory"
  (loop :for file :in (directory (concatenate 'string *listings-directory* "*.asm"))
        :when (pathname-name file)
          :collect (nasm-compile (namestring file))))

(defun char-bin (char)
  (format nil "~,,' ,2B" char))
