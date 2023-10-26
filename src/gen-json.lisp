(in-package :cl-plotly)

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

;; Generic function for printing, so that it can be overriden

(defgeneric print-as-json (x out)
  (:documentation "Generic function to print an object X as JSON to stream OUT.
You may define methods for this generic function for specific types, so as to customize the output, e.g. for timestamps."))

(defun print-to-json (x &optional (out *standard-output*))
  (print-as-json x out))

(defun key-as-string (k)
  "K can be keyword, a symbol, or a string. Return the string form, to be used as key in JSON object.
If it is string, it is returned as is.
If it is keyword or plain symbol, if there are mixed cases, return the string form with mixed case ; otherwise return the lowercase form."
  ;; TODO
  )

(defun print-symbol-as-json (sym &optional (out *standard-output*))
  (format out "~S" (key-as-string sym)))

(defun print-list-as-json-array (lst &optional (out *standard-output*))
  (let ((pr-sep nil))
    (format out "[")
    (dolist (x lst)
      (when pr-sep (format out ", "))
      (print-as-json x out)
      (setf pr-sep t))
    (format out "]")))

(defun print-array-as-json (arr &optional (out *standard-output*))
  (let ((pr-sep nil))
    (format out "[")
    (loop for x across arr
          do
             (when pr-sep (format out ", "))
             (print-as-json x out)
             (setf pr-sep t))
    (format out "]")))

(defun print-alist-as-json (alist &optional (out *standard-output*))
  (let ((key-seen (make-hash-table :key equal))
        ;; key-seen uses the string form of the key for indexing
        ;; If a key is already printed, skip it
        (pr-sep nil))
    (format out "{")
    (dolist (z alist)
      (let ((k (key-as-string (car z))))
        (unless (gethash k key-seen)
          ;; unseen, print it
          (when pr-sep (format out ", "))
          (format out "~S: " k)
          (print-as-json (cdr z) out)
          ;; now printed, mark it as seen
          (setf (gethash k key-seen) t
                pr-sep t))))
    (format out "}")))

(defun print-plist-as-json (plist &optional (out *standard-output*))
  (let ((key-seen (make-hash-table :key equal))
        ;; key-seen uses the string form of the key for indexing
        ;; If a key is already printed, skip it
        (pr-sep nil))
    (format out "{")
    (do ((zs plist (cddr zs)))
        ((null zs))
      (let ((k (key-as-string (car z))))
        (unless (gethash k key-seen)
          ;; unseen, print it
          (when pr-sep (format out ", "))
          (format out "~S: " k)
          (print-as-json (cadr z) out)
          ;; now printed, mark it as seen
          (setf (gethash k key-seen) t
                pr-sep t))))
    (format out "}")))

(defun print-hash-table-as-json (hash-table &optional (out *standard-output*))
  (let ((pr-sep nil))
    (format out "{")
    (loop for k being each hash-key of hash-table
          using (hash-value v)
          do
          (when pr-sep (format out ", "))
          ;; assuming the keys are either keyword, symbol or string, or otherwise coerce it to string
          (format out "~S: " (key-as-string k))
          (print-as-json v out))
    (format out "}")))

(defmethod print-as-json (x out)
  "Default method to give default handling of base types, so that user may override for base types."
  (cond ((eq x t) (format out "true"))
        ((null x) (format out "null"))
        ((integerp x) (format out "~D" x))
        ((rationalp x) (format out "~A" (coerce x 'double-float)))
        ((floatp x) (format out "~A" x)) ;; TODO: get rid of the 'd' as exponent
        ((stringp x) (format out "~S" x))
        ((json-value-p x) (format out "~A" (json-value-print-form x)))
        ((symbolp x) (print-symbol-as-json x out))
        ((arrayp x) (print-array-as-json x out))
        ((hash-table-p x) (print-hash-table-as-json x out))
        ;; TODO: guess alist, plist, plain list as array
        (t (error "Unsupported value in print-as-json: ~A" x))))
  
