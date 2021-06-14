(in-package :dorg)

(defclass/std doc-writer ()
  ((sections :doc "A p-list with keys being sections names and values being an order list of files to generate docs for that section")
   (hugo-base-dir :doc "The root folder of the hugo site, docs will go to /content/:sec/filename ")
   ;(track :std '("defclass/std" "defmethod" "defgeneric" "defun" "defparameter") "A list of strings of the types of things we generate documentation for")
   (track :std '("defun") "A list of strings of the types of things we generate documentation for")
   (output-dir)
   ))



(defun keyword->string (k)
  "Turns a keyword into a string"
  (subseq (write-to-string k) 1))

(defun section-files (dw sec)
  "dw: dorg:doc-writer
sec: keyword
Returns: List an ordered list of the pathnames all files is sec"
  (getf (sections dw) sec))


(defun all-toplevel-forms (path)
  "path: Path
 returns: List of strings that are the all top level forms of the files in path"
  (do* ((start 0 (recursive-regex::end match))
        (s (uiop:read-file-string path)
           (subseq s start))
        (match (recursive-regex::regex-recursive-groups "(?<parens>)" s)
          (recursive-regex::regex-recursive-groups "(?<parens>)" s))
        (all nil (if match
                     (push (recursive-regex::full-match match) all)
                     all)))
       ((null match) (nreverse all))))




(defun allowedp (allowed def)
  "allowed: List of the names of allowed form types
 def: string of the form
returns: t if def in allowed, false otherwise"
  (dolist (a allowed) nil
    (let ((alen (length a)))
      (when (and
             (> (length def) alen)
             (string-equal a
                           (subseq def 1 (1+ (length a)))))
        (return t)))))


(defun filter-definitions (allowed definitions)
  "allowed: List of the names of allowed form types
 definitions: List of strings contaning forms to check
returns: definitions that are allowed"
  (remove-if-not #'(lambda (def)
                     (allowedp allowed def)
                     )
                 definitions))

(defun add-def-to-ht (def ht)
  "def: a string containing a definition to document
ht: A hashtable with keys: definition-type, value: List of definitions of that type
returns: ht with def added to the front of the list of values under the key of def's form-type"
  (let ((defs (gethash (get-form-type def) ht)))
        ;(break "~A~%~A" (get-form-type def) def)
    (if defs
      (setf (gethash (get-form-type def) ht) (append defs (list def)))
      (setf (gethash (get-form-type def) ht) (list def)))))


(defun group-forms (forms)
  "forms: list of top level defintion forms
order: list of strings that define the order of the grouping
returns: hash-table, key=form-type, val = ordered list of strings of the form (by appearance in the file)"
  (let ((groups (make-hash-table :test 'equalp)))
    (mapcar #'(lambda (form)
                (add-def-to-ht form groups))
            forms)
    groups))


(defun get-definitions (dw)
  "Take a doc-writer and returns a nested list list like

that tracks the section, filename and all definitions it (track dw)"
  (mapcar #'(lambda (sec files)
                (mapcar #'(lambda (file-path)
                            (list (keyword->string sec)
                                  (file-namestring file-path)
                                  (group-forms
                                   (filter-definitions
                                    (track dw)
                                    (all-toplevel-forms file-path))))) ; We filter all the forms here because I scratch a lot when developing and I dont want those formss here
                        files))
                (serapeum:plist-keys  (sections dw))
                (serapeum:plist-values (sections dw))))




(defun format-section-file (dw sec-spec)
  "Takes a list with first entry the section title,
second entry the file name and
third a ht with key = form type, vals = ordered list of forms"
  (let* ((title (format nil "#+title: ~A" (second sec-spec))) ;; second is the file title
         (hugo-sec (format nil "#+HUGO_SECTION: ~A" (first sec-spec))) ;; first is the section title
         (hugo-base-dir (format nil "#+HUGO_BASE_DIR: ~A" (hugo-base-dir dw)))
         (defs (third sec-spec))
         (parser (make-instance 'form-parser))
         (docs (mapcar #'(lambda (form-type)
                           (mapcar #'(lambda (def)
                                        ;         (break "def: ~A " def )
                                       (get-doc (parse parser def)))
                                   (gethash form-type defs)))
                       (track dw))))
    (alexandria:flatten (list
                         title
                         hugo-base-dir
                         hugo-sec
                         docs))))



(defun write-section-file (dw l)
  "l: A list of strings"
  (let ((output-file-name (concatenate 'string
                                       (output-dir dw)
                                       (first (uiop:split-string (second (uiop:split-string (first l) :separator " ")) :separator  "."))
                                       ".org")))
    (with-open-file (out output-file-name
                         :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (dolist (line l)
        (format out "~A~%" line)))))



(defun write-section (dw sec-specs)
  "dw: dorg:doc-writer
sec-specs: List with first"

  (let ((sec-info (mapcar #'(lambda (sec-spec)
                              (format-section-file dw sec-spec))
                          sec-specs)))
    (mapcar #'(lambda (sec)
                (write-section-file dw sec))
            sec-info)
    sec-info))



(defun document-system-org (dw)
  "Writes the documentation, in .org file format to output-dir"
  (mapcar #'(lambda (sec-specs)
              (write-section dw sec-specs))
          (get-definitions dw)))








(progn
(setf dwo (make-instance 'doc-writer
                        :sections '(:source ("/home/dd/quicklisp/local-projects/dorg/dorg.lisp"
                                           "/home/dd/quicklisp/local-projects/dorg/parser.lisp"))
                        :hugo-base-dir "/home/dd/dev/dorg"
                        :output-dir "/home/dd/tmp/dorg/"))
(setq dwds (third  (caar (get-definitions dwo)))))

(document-system-org dwo)

;; (setf dw (make-instance 'doc-writer
;;                         :sections '(:core ("/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp"
;;                                            "/home/dd/quicklisp/local-projects/small/src/core/ht-helpers.lisp"
;;                                            "/home/dd/quicklisp/local-projects/small/src/core/linear-algebra.lisp"
;;                                            "/home/dd/quicklisp/local-projects/small/src/core/utils.lisp")
;;                                     :dna  ("/home/dd/quicklisp/local-projects/small/src/dna/dna.lisp"
;;                                            "/home/dd/quicklisp/local-projects/small/src/dna/dna-helix-strand.lisp"
;;                                            "/home/dd/quicklisp/local-projects/small/src/dna/dna-nt.lisp"
;;                                            "/home/dd/quicklisp/local-projects/small/src/dna/dna-origami.lisp"
;;                                            "/home/dd/quicklisp/local-projects/small/src/dna/dna-single-strand.lisp"
;;                                            "/home/dd/quicklisp/local-projects/small/src/dna/dna-strand.lisp"
;;                                            "/home/dd/quicklisp/local-projects/small/src/dna/packages.lisp"))))
