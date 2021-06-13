(in-package :dorg)

(defun keyword->string (k)
  (subseq (write-to-string k) 1))

(defun section-files (dw sec)
  "dw: dorg:doc-writer
sec: keyword"
  (getf (sections dw) sec))


(defclass/std doc-writer ()
  ((sections :doc "A p-list with keys being sections names and values being an order list of files to generate docs for that section")
   (hugo-base-dir :doc "The root folder of the hugo site, docs will go to /content/:sec/filename ")
   (track :std '("defclass" "defmethod" "defgeneric" "defun" "defparameter") "A list of strings of the types of things we generate documentation for")
   ))


(progn
(setf dwo (make-instance 'doc-writer
                        :sections '(:source ("/home/dd/quicklisp/local-projects/dorg/dorg.lisp"
                                           "/home/dd/quicklisp/local-projects/dorg/parser.lisp"))))
(setq dwds (third  (caar (get-definitions dwo)))))
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




(defun all-toplevel-forms (path)
  "Takes a path, returns all top level forms"
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
  "allowed: list of things we want included
 def: string of the definition"
  (dolist (a allowed) nil
    (let ((alen (length a)))
      (when (and
             (> (length def) alen)
             (string-equal a
                           (subseq def 1 (1+ (length a)))))
        (return t)))))


(defun filter-definitions (allowed definitions)
  "allowed: list of things we want included
 definitions: list of the all definitions in a file"
  (remove-if-not #'(lambda (def)
                     (allowedp allowed def)
                     )
                 definitions))

(defun get-definitions (dw)
  "Take a doc-writer and returns a nested list list like

that tracks the section, filename and all definitions it (track dw)"
  (mapcar #'(lambda (sec files)
                (mapcar #'(lambda (file-path)
                            (list (keyword->string sec)
                                  (file-namestring file-path)
                                  (filter-definitions (track dw) (all-toplevel-forms file-path)))) ; We filter all the forms here because I scratch a lot when developing and I dont want those formss here
                        files))
                (serapeum:plist-keys  (sections dw))
                (serapeum:plist-values (sections dw))))





(get-form-name (first dwds))

(defun add-def-to-ht (def ht)
  "Add"
  (let ((defs (gethash (get-form-type def) ht)))
        ;(break "~A~%~A" (get-form-type def) def)
    (if defs
      (setf (gethash (get-form-type def) ht)  (append def defs))
      (setf (gethash (get-form-type def) ht) (list def)))))


(defun group-forms (forms)
  "forms: list of top level defintion forms
order: list of strings that define the order of the grouping
"
  (let ((groups (make-hash-table :test 'equalp)))
    (mapcar #'(lambda (form)
                (add-def-to-ht form groups))
            forms)
    groups))

(group-forms dwds)


)))))


(track dwo)





(defun write-section (sec-spec)
  "dw: dorg:doc-writer
sec: keyword"
  (let ((title (format nil "#+title ~A" (second sec-spec))) ;; second is the file title
        (hugo-sec (format nil "#+HUGO_SECTION: ~A" (first sec-spec)))) ;; first is the section title
  (format nil "~A" (third sec-spec) )
      ))



(defun document-system (dw)
  "Writes the documentation"

  (mapcar #'(lambda (section)
              (mapcar #'(lambda (sec-spec)
                     ;;     (break "~A" sec-spec)
                          (write-section sec-spec))
                      section))
          (get-definitions dw))
)

(document-system dwo)




(defun definitions-from-file (path))


(defun document-defun (form)
  "Take a defun form and returns a string to be written"
  (car (get-definitions dwo)))







;; (all-matches-as-strings "\((?:[^)(]*(?:R)?)* +\)"
;;                         (uiop:read-file-string "/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp"))
