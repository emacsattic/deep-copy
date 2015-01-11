;;; deep-copy.el --- make complete recursive copy of data structures

;; Author: Noah Friedman <friedman@splode.com>
;; Keywords: lisp, extensions
;; License: public domain

;; $Id: deep-copy.el,v 1.2 2013/08/03 17:22:02 friedman Exp $

;;; Commentary:
;;; Code:

(defconst deep-copy-types (make-vector 13 0))

(defmacro defdeep-copy (name pred &rest body)
  (let ((sym  (if (consp name) (car name) name))
        (prec (if (consp name) (cdr name))))
    (if (symbolp sym)  (setq sym (symbol-name sym)))
    (if (symbolp pred) (setq pred (list 'quote pred)))
    `(let ((sym (intern ,sym deep-copy-types)))
       (fset sym (lambda (obj types) ,@body))
       (set  sym ,pred)
       ,@(when prec `((put sym 'precedence (quote ,prec)))))))

(put 'defdeep-copy 'lisp-indent-function 2)

(defdeep-copy (cons first) 'consp
  (cons (deep-copy (car obj) types)
        (deep-copy (cdr obj) types)))

(defdeep-copy (bytecode before obarray) 'byte-code-function-p
  (let ((slots nil)
        (i 0))
    ;; mapcar doesn't work on bytecode objects
    (while (< i (length obj))
      (setq slots (cons (deep-copy (aref obj i) types) slots)
            i (1+ i)))
    (apply 'make-byte-code (nreverse slots))))

(defdeep-copy (obarray before vector) 'obarrayp
  (setq obj (copy-obarray obj))
  ;; copy-obarray doesn't copy the symbol slots; do that now.
  (mapatoms (lambda (sym)
              (when (boundp sym)
                (set sym (deep-copy (symbol-value sym) types)))
              (when (fboundp sym)
                (fset sym (deep-copy (symbol-function sym) types)))
              (setplist sym (deep-copy (symbol-plist sym) types)))
            obj)
  obj)

(defdeep-copy (vector last) 'vectorp
  (let ((nv (make-vector (length obj) nil))
        (i 0))
    (while (< i (length nv))
      (aset nv i (deep-copy (aref obj i) types))
      (setq i (1+ i)))
    nv))

(defdeep-copy string 'stringp (copy-sequence obj))

(defdeep-copy marker 'markerp
  (let ((m (make-marker)))
    (set-marker m (marker-position obj) (marker-buffer obj))
    (and (fboundp 'marker-insertion-type)
         (set-marker-insertion-type m (marker-insertion-type obj)))
    m))

(defdeep-copy hashtable 'hash-table-p
  (let ((newtbl (make-hash-table
                 :test             (hash-table-test             obj)
                 :size             (hash-table-size             obj)
                 :rehash-size      (hash-table-rehash-size      obj)
                 :rehash-threshold (hash-table-rehash-threshold obj)
                 :weakness         (hash-table-weakness         obj))))
    (maphash (lambda (key val)
               (puthash (deep-copy key types)
                        (deep-copy val types) newtbl))
             obj)
    newtbl))

(defun make-deep-copy-typelist (types)
)

;;;###autoload
(defun deep-copy (obj &optional types)
  "Make a deep copy of an object.
Traverse object recursively, copying elements to create a new copy of the
overall object.

Only conses are copied unless the optional argument TYPES is non-nil.
If TYPES is `t' then the objects of the following types are copied:

   * cons
   * bytecode
   * obarray
   * vector
   * string
   * marker
   * hashtable

That is, these objects and their inner elements \(if any\) will be distinct
copies from the elements present in the original object.

If TYPES is a list of symbols of the aforementioned type names, only objects
of those types will be copied.  That is, copying stops at any object not
matching one of those types; it and all its substructure will be shared
between the old and new structures.

If TYPES is a list whose first element is `not', the sense of the copy will
be reversed: only those types not in the rest of the list will be copied."

  ;; At the outermost call, compute which types we're going to copy;
  ;; subcalls won't have to repeat this.
  (unless (vectorp types)
    (let ((new (make-vector 13 nil)))
      (cond ((null types)
             (intern "cons" new))
            ((not (consp types))
             (mapc (lambda (s) (intern (symbol-name s) new))
               deep-copy-types))
            ((eq 'not (car types))
             ;; The first time a (not ...) predicate is encountered,
             ;; reverse its sense
             (mapc (lambda (s)
                     (unless (memq s (cdr types))
                       (intern (symbol-name s) new)))
               deep-copy-types)))
      (setq types new)))

)

(provide 'deep-copy)

;;; deep-copy.el ends here.
