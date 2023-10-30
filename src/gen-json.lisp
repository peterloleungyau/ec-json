(defpackage :ec-json
  (:use :cl)
  ;; TODO: export symbols
  )

(in-package :ec-json)

;;;;;;;;;;;;;;
;; default output stream
;; reference: https://github.com/hankhero/cl-json/blob/194115007dcd3c75c8ace371a5ac6d6aa1b1a9dc/src/encoder.lisp#L8-L9
(defvar *json-output* (make-synonym-stream '*standard-output*)
  "The default output stream for encoding operations.")

;;;;;;;;;;;;;;
;; condition for error, so that it can be specifically catched and handled.
;; reference: https://github.com/hankhero/cl-json/blob/194115007dcd3c75c8ace371a5ac6d6aa1b1a9dc/src/encoder.lisp#L11-L28

(define-condition unencodable-value-error (type-error)
  ((context :accessor unencodable-value-error-context :initarg :context))
  (:documentation
   "Signalled when a datum is unhandled in encoding as JSON.")
  (:default-initargs :expected-type t)
  (:report
   (lambda (condition stream)
     (with-accessors ((datum type-error-datum)
                      (context unencodable-value-error-context))
         condition
       (format stream
               "Value ~S is not of a type which can be encoded as JSON~@[ by ~A~]."
               datum context)))))

(defun unencodable-value-error (value &optional context)
  "Signal an UNENCODABLE-VALUE-ERROR."
  (error 'unencodable-value-error :datum value :context context))

;;;;;;;;;;;;;;
;; Special values for JSON
;;
;; you may create any special value to output verbatium when encoded.

(defstruct json-value
  print-form)

(defvar *json-true* (make-json-value :print-form "true")
  "The true for JSON, for completeness, could have used Lisp's T.")

(defvar *json-false* (make-json-value :print-form "false")
  "The false for JSON, because NIL in Lisp could represent multiple things.")

(defvar *json-null* (make-json-value :print-form "null")
  "The null for JSON, because NIL in Lisp could represent multiple things.")

;;;;;;;;;;;;;;
;; to mark different types of obj: either hash-table, plist wrapped in struct, alist wrapped in struct.
;; 
;; If you want to directly make the type, e.g. (make-plist-json-obj :plist lst)
;;
;; You may use as-json-obj to try to guess the type, and make the appropriate struct, or just return the hash-table.

(defstruct plist-json-obj
  ;; (key1 val1 key2 val2 ...)
  plist)

(defstruct alist-json-obj
  ;; ((key1 val1) (key2 val2) ...)
  alist)

(defun as-json-obj (val)
  "VAL could be either hash-table, or a plist (where car looks like a key) or an alist (where car is a cons).
If it is hash-table, return as is.
If it looks like a plist, make a plist-json-obj.
If it looks like an alist, make a alist-json-obj."
  (etypecase val
    (hash-table val)
    (cons (if (consp (car val))
              (make-alist-json-obj :alist val)
              (make-plist-json-obj :plist val)))))

;;;;;;;;;;;;;;
;; Generic function for printing, so that it can be overriden

(defgeneric print-as-json (x &optional out)
  (:documentation "Generic function to print an object X as JSON to stream OUT.
You may define methods for this generic function for specific types, so as to customize the output, e.g. for timestamps."))

;;;;;;;;;;;;;;
;; to convert key (which could be keyword or symbol) to string.
;; It is convenient and customary to use symbol or keyword symbol as keys in plist, alist or hash-table.
;; But by default Common Lisp will read them all as upper-case (due to historical reason).
;; However, often the keys we want to output is lower-case. It would be tedious to write :|lower-case-symbol|.
;; Therefore, we provide convenience to choose how to convert, depending on the value of *how-to-convert-key-case*.
;; By default it is to convert keyword and symbols to lower case string when encoded as JSON keys, unless it is mixed case.
;; Also, this conversion (if conversion is needed) is memoized and customizable.

(defvar *how-to-convert-key-case* :lower-unless-mixed-case
  "Control how to convert keyword or symbol to string as JSON key, for use in #'convert-key-to-str,
 which is called by the default *key-as-str-func* #'key-as-string.
It can be one of the following:
  :lower-unless-mixed-case => convert to lower case if the symbol is not mixed case;
  :lower => convert to lower in any case;
  :as-is => simply use the symbol-name, does not do any convertion.")

(defun convert-key-to-str (k)
  (let ((s (symbol-name k)))
    (ecase *how-to-convert-key-case*
      (:as-is s)
      (:lower (string-downcase s))
      (:lower-unless-mixed-case
       ;; NOTE: characters in Common Lisp could be either upper case, or lower case, or neither.
       ;; And the reader by default reads all character to upper case (if applicable).

       ;; Therefore, if there is any lower case character, then either
       ;; we have mixed case (there is some upper case character); or
       ;; we have no upper case character. In both cases, we do not
       ;; need to convert.
       (if (some #'lower-case-p s)
           s
           (string-downcase s))))))

(defvar *convert-key-to-str* (make-hash-table :test 'eq)
  "Memoized conversion (when *how-to-convert-key-case* is not :as-is) from keyword or symbol to the string.

Therefore uses 'eq for test.  You may replace this for your convenient
mapping, or add to this hash-table for desired mapping.")

(defun key-as-string (k)
  "K can be keyword, a symbol, or a string. Return the string form, to be used as key in JSON object.
For keyword and symbol, the result is also memoized in *key-strs*.
If it is string, it is returned as is.
If it is keyword or plain symbol, if *how-to-convert-key-case* is :as-is, return the symbol-name as it;
otherwise if the key is in the hash-table *convert-key-to-str*, then use the found string;
otherwise convert to string using #'convert-key-to-str, which may convert case depending on
 *how-to-convert-key-case* and memoize the result to *convert-key-to-str*"
  (etypecase k
    (string k)
    (symbol
     (if (eq *how-to-convert-key-case* :as-is)
         (symbol-name k)
         ;; might need to convert, see if memoized
         (let ((s (gethash k *convert-key-to-str*)))
           (if s
               s
               (setf (gethash k *convert-key-to-str*)
                     (convert-key-to-str k))))))))

(defvar *key-as-str-func* #'key-as-string
  "A function (k -> str) to convert a keyword, symbol, or string to a string as JSON key.
Default is #'key-as-string, which use string as is;
 and convert keyword or symbol to lower case, unless they are mixed case, and with memoization.")

;;;;;;;;;;;;;;
;; helper functions and specific function for one type of value

(defun print-one-json-char (char &optional (out *json-output*))
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

(defun print-string-as-json (str &optional (out *json-output*))
  ;; reference: https://github.com/gigamonkey/monkeylib-json/blob/master/json.lisp#L70C1-L73C27
  (write-char #\" out)
  (loop :for char :across str :do (print-one-json-char char out))
  (write-char #\" out))

(defun print-symbol-as-json (sym &optional (out *json-output*))
  (print-string-as-json (funcall *key-as-str-func* sym) out))

(defun print-list-as-json-array (lst &optional (out *json-output*))
  (let ((pr-sep nil))
    (write-char #\[ out)
    (dolist (x lst)
      (when pr-sep (format out ", "))
      (print-as-json x out)
      (setf pr-sep t))
    (write-char #\] out)))

(defun print-array-as-json (arr &optional (out *json-output*))
  (let ((pr-sep nil))
    (write-char #\[ out)
    (loop :for x :across arr
          :do
             (when pr-sep (write-string ", " out))
             (print-as-json x out)
             (setf pr-sep t))
    (write-char #\] out)))

;; print as obj

(defvar *allow-dup-keys* nil
  "If T, will allow a key to appear more than once in the object output for plist and alist.
If NIL, for duplicate key, will only output the first that appears in plist or alist,
  i.e. you may append to the front of plist and alist to override some fields, and get
  the desired output as JSON object.")

(defun print-alist-as-json-no-dup-keys (alist &optional (out *json-output*))
  (let ((key-seen (make-hash-table :test 'equal))
        ;; key-seen uses the string form of the key for indexing
        ;; If a key is already printed, skip it
        (pr-sep nil))
    (write-char #\{ out)
    (dolist (z alist)
      (let ((k (funcall *key-as-str-func* (car z))))
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

(defun print-alist-as-json-allow-dup-keys (alist &optional (out *json-output*))
  (let ((pr-sep nil))
    (write-char #\{ out)
    (dolist (z alist)
      (when pr-sep (write-string ", " out))
      (print-string-as-json (funcall *key-as-str-func* (car z)) out)
      (write-char #\: out)
      (print-as-json (cdr z) out)
      (setf pr-sep t))
    (write-char #\} out)))

(defun print-alist-as-json (alist
                            &optional
                              (out *json-output*)
                              (allow-dup-keys *allow-dup-keys*))
  (if allow-dup-keys
      (print-alist-as-json-allow-dup-keys alist out)
      (print-alist-as-json-no-dup-keys alist out)))

(defun print-plist-as-json-no-dup-keys (plist &optional (out *json-output*))
  (let ((key-seen (make-hash-table :test 'equal))
        ;; key-seen uses the string form of the key for indexing
        ;; If a key is already printed, skip it
        (pr-sep nil))
    (write-char #\{ out)
    (do ((zs plist (cddr zs)))
        ((null zs))
      (let ((k (funcall *key-as-str-func* (car zs))))
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

(defun print-plist-as-json-allow-dup-keys (plist &optional (out *json-output*))
  (let ((pr-sep nil))
    (write-char #\{ out)
    (do ((zs plist (cddr zs)))
        ((null zs))
      (when pr-sep (write-string ", " out))
      (print-string-as-json (funcall *key-as-str-func* (car zs)) out)
      (write-char #\: out)
      (print-as-json (cadr zs) out)
      (setf pr-sep t))
    (write-char #\} out)))

(defun print-plist-as-json (plist
                            &optional
                              (out *json-output*)
                              (allow-dup-keys *allow-dup-keys*))
  (if allow-dup-keys
      (print-plist-as-json-allow-dup-keys plist out)
      (print-plist-as-json-no-dup-keys plist out)))

(defun print-hash-table-as-json (hash-table &optional (out *json-output*))
  ;; assume the keys are unique, although it is possible that some
  ;; keys are string, and some are symbol, some are keyword, so still
  ;; may have duplicate keys in JSON.

  ;; But because there is no natural order of keys, we do not assume
  ;; and order to remove dups, the caller should do it themeselves it
  ;; they care about not having dup keys.
  (let ((pr-sep nil))
    (write-char #\{ out)
    (loop :for k :being :each hash-key :of hash-table
            :using (hash-value v)
          :do
             (when pr-sep (write-string ", " out))
             ;; assuming the keys are either keyword, symbol or string, or otherwise coerce it to string
             (print-string-as-json (funcall *key-as-str-func* k) out)
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

(defun print-real-as-json (r &optional (out *json-output*))
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
    (t (unencodable-value-error r 'print-real-as-json))))

(defmethod print-as-json (x &optional (out *json-output*))
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
        ((plist-json-obj-p x) (print-plist-as-json (plist-json-obj-plist x) out))
        ((alist-json-obj-p x) (print-alist-as-json (alist-json-obj-alist x) out))
        ((consp x)
         ;; guess alist, plist, or plain list as array
         (let ((cx (car x)))
           (cond ((consp cx)
                  (print-alist-as-json x out))
                 ((or (symbolp cx) (stringp cx))
                  (print-plist-as-json x out))
                 (t (print-list-as-json-array x out)))))                
        (t (unencodable-value-error x 'print-as-json))))
   
