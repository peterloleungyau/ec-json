(defpackage :ec-json
  (:use :cl)
  ;; TODO: export symbols
  )

(in-package :ec-json)

;;;;;;;;;;;;;;
;; Special values for JSON

(defstruct json-value
  print-form)

(defvar *json-true* (make-json-value :print-form "true")
  "The true for JSON, for completeness, could have used Lisp's T.")

(defvar *json-false* (make-json-value :print-form "false")
  "The false for JSON, because NIL in Lisp could represent multiple things.")

(defvar *json-null* (make-json-value :print-form "null")
  "The null for JSON, because NIL in Lisp could represent multiple things.")

(defstruct json-obj
  ;; could be plist, alist, or hash-table
  val)

;;;;;;;;;;;;;;
;; Generic function for printing, so that it can be overriden

(defgeneric print-as-json (x &optional out)
  (:documentation "Generic function to print an object X as JSON to stream OUT.
You may define methods for this generic function for specific types, so as to customize the output, e.g. for timestamps."))

;;;;;;;;;;;;;;
;; helper functions and specific function for one type of value

(defun key-as-string (k)
  "K can be keyword, a symbol, or a string. Return the string form, to be used as key in JSON object.
If it is string, it is returned as is.
If it is keyword or plain symbol, if there are mixed cases, return the string form with mixed case ; otherwise return the lowercase form."
  ;; TODO
  (declare (ignorable k))
  )

(defun print-one-json-char (char &optional (out *standard-output*))
  ;; reference: https://github.com/gigamonkey/monkeylib-json/blob/88a52b5fe7037f88fd57908b8f79fd8f1c521401/json.lisp#L75-L89
  ;; added some comment
  (case char
    ;; since we only output using double quote, we do not escape single quote.
    (#\" (write-string "\\\"" out))
    (#\\ (write-string "\\\\" out))
    ;; other than backlash and the (single and double) quotes, JSON only requires to escape a few characters.
    (#.(code-char 8) (write-string "\\b" out))
    (#.(code-char 9) (write-string "\\t" out))
    (#.(code-char 10) (write-string "\\n" out))
    (#.(code-char 12) (write-string "\\f" out))
    (#.(code-char 13) (write-string "\\r" out))
    (t
     (cond
       ;; ASCII 0 up to and including 0x1F are control code, we output as \uzzzz escape in JSON string. 
       ;; reference: https://www.asciitable.com/
       ((<= 0 (char-code char) #x1f)
        (format out "\\u~4,'0x" (char-code char)))
       (t (write-char char out))))))

(defun print-string-as-json (str &optional (out *standard-output*))
  ;; reference: https://github.com/gigamonkey/monkeylib-json/blob/master/json.lisp#L70C1-L73C27
  (write-char #\" out)
  (loop :for char :across str :do (print-one-json-char char out))
  (write-char #\" out))

(defun print-symbol-as-json (sym &optional (out *standard-output*))
  (print-string-as-json (key-as-string sym) out))

(defun print-list-as-json-array (lst &optional (out *standard-output*))
  (let ((pr-sep nil))
    (write-char #\[ out)
    (dolist (x lst)
      (when pr-sep (format out ", "))
      (print-as-json x out)
      (setf pr-sep t))
    (write-char #\] out)))

(defun print-array-as-json (arr &optional (out *standard-output*))
  (let ((pr-sep nil))
    (write-char #\[ out)
    (loop :for x :across arr
          :do
             (when pr-sep (write-string ", " out))
             (print-as-json x out)
             (setf pr-sep t))
    (write-char #\] out)))

(defun print-alist-as-json (alist &optional (out *standard-output*))
  (let ((key-seen (make-hash-table :test 'equal))
        ;; key-seen uses the string form of the key for indexing
        ;; If a key is already printed, skip it
        (pr-sep nil))
    (write-char #\{ out)
    (dolist (z alist)
      (let ((k (key-as-string (car z))))
        (unless (gethash k key-seen)
          ;; unseen, print it
          (when pr-sep (write-string ", " out))
          (print-string-as-json k out)
          (write-char #\: out)
          (print-as-json (cdr z) out)
          ;; now printed, mark it as seen
          (setf (gethash k key-seen) t
                pr-sep t))))
    (write-char #\} out)))

(defun print-plist-as-json (plist &optional (out *standard-output*))
  (let ((key-seen (make-hash-table :test 'equal))
        ;; key-seen uses the string form of the key for indexing
        ;; If a key is already printed, skip it
        (pr-sep nil))
    (write-char #\{ out)
    (do ((zs plist (cddr zs)))
        ((null zs))
      (let ((k (key-as-string (car zs))))
        (unless (gethash k key-seen)
          ;; unseen, print it
          (when pr-sep (write-string ", " out))
          (print-string-as-json k out)
          (write-char #\: out)
          (print-as-json (cadr zs) out)
          ;; now printed, mark it as seen
          (setf (gethash k key-seen) t
                pr-sep t))))
    (write-char #\} out)))

(defun print-hash-table-as-json (hash-table &optional (out *standard-output*))
  (let ((pr-sep nil))
    (write-char #\{ out)
    (loop :for k :being :each hash-key :of hash-table
            :using (hash-value v)
          :do
             (when pr-sep (write-string ", " out))
             ;; assuming the keys are either keyword, symbol or string, or otherwise coerce it to string
             (print-string-as-json (key-as-string k) out)
             (write-char #\: out)
             (print-as-json v out)
             (setf pr-sep t))
    (write-char #\} out)))

(eval-when (:compile-toplevel)
  ;; reference: https://github.com/hankhero/cl-json/blob/194115007dcd3c75c8ace371a5ac6d6aa1b1a9dc/src/encoder.lisp#L393-L404
  ;; determine once which of the 4 float types are the same: short-float, single-float, double-float, long-float
  (if (subtypep 'long-float 'single-float)
      ;; only one float type
      (pushnew :ec-json-only-one-float-type *features*)
      ;; else -- we check here only for the case where there are two
      ;; float types, single- and double- --- we don't consider the
      ;; "only single and short" case.  Could be added if necessary.
      (progn
        (when (subtypep 'single-float 'short-float)
          (pushnew :ec-json-short-single-float-same *features*))
        (when (subtypep 'double-float 'single-float)
          (pushnew :ec-json-single-double-float-same *features*))
        (when (subtypep 'long-float 'double-float)
          (pushnew :ec-json-double-long-float-same *features*)))))

(defun print-real-as-json (r &optional (out *standard-output*))
  ;; reference: https://github.com/hankhero/cl-json/blob/194115007dcd3c75c8ace371a5ac6d6aa1b1a9dc/src/encoder.lisp#L406-L423
  ;; we want the printed form to always use 'e' as the exponent marker
  ;; print will use *read-default-float-format* and compare with current type of the float
  ;; if the format differ, print will use the appropriate exponent marker, otherwise will use 'e':
  ;; 's' for short-float, 'f' for single-float, 'd' for double-float, 'l' for long-float.
  ;; of course, implementation may merge some of the neighbouring type of float.
  (typecase r
    (integer (format out "~D" r))
    (real (let ((*read-default-float-format*
                  ;; make sure the float type is the same, but want to save some checks if some float types are merged.
                  (etypecase r
                    (short-float 'short-float)
                    (rational 'single-float)
                    #-(or ec-json-short-single-float-same
                       ec-json-only-one-float-type)
                    (single-float 'single-float)
                    #-(or ec-json-single-double-float-same
                       ec-json-only-one-float-type)
                    (double-float 'double-float)
                    #-(or ec-json-double-long-float-same
                       ec-json-only-one-float-type)
                    (long-float 'long-float))))
            (format out "~F" r)))
    (t (error "Unsupported number type in print-real-as-json: ~A" r))))

(defmethod print-as-json (x &optional (out *standard-output*))
  "Default method to give default handling of base types, so that user may override for base types."
  (cond ((eq x t) (format out "true"))
        ((null x) (format out "null"))
        ((integerp x) (format out "~D" x))
        ((realp x) (print-real-as-json x out))
        ((stringp x) (print-string-as-json x out))
        ((json-value-p x) (format out "~A" (json-value-print-form x)))
        ((symbolp x) (print-symbol-as-json x out))
        ((arrayp x) (print-array-as-json x out))
        ((hash-table-p x) (print-hash-table-as-json x out))
        ;; TODO: guess alist, plist, plain list as array
        (t (error "Unsupported value in print-as-json: ~A" x))))
   
