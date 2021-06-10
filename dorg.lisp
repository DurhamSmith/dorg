(in-package :defclass-std)

(ql:quickload '(:defclass-std :serapeum :alexandria :recursive-regex))
(ql:quickload :serapeum )
(ql:quickload :recu )
(defun keyword->string (k)
  (subseq (write-to-string k) 1))

(defun section-files (dw sec)
  "dw: dorg:doc-writer
sec: keyword"
  (getf (sections dw) sec))

;; We can use file-namestring instead
;; (defun file-name (path-ish)
;;   "Get the file name only, removing directories"
;;   (car (last (uiop:split-string path-ish :separator "/"))))

(defclass/std doc-writer ()
  ((sections :doc "A p-list with keys being sections names and values being an order list of files to generate docs for that section")
   (hugo-base-dir :doc "The root folder of the hugo site, docs will go to /content/:sec/filename ")
   (track :std '("defclass" "defun" "defparameter") "A list of strings of the types of things we generate documentation for")
))




(setf dw (make-instance 'doc-writer
               :sections '(:core ("/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp"
                                  "/home/dd/quicklisp/local-projects/small/src/core/ht-helpers.lisp"
                                  "/home/dd/quicklisp/local-projects/small/src/core/linear-algebra.lisp"
                                  "/home/dd/quicklisp/local-projects/small/src/core/utils.lisp")
                           :dna  ("/home/dd/quicklisp/local-projects/small/src/dna/dna.lisp"
                                   "/home/dd/quicklisp/local-projects/small/src/dna/dna-helix-strand.lisp"
                                   "/home/dd/quicklisp/local-projects/small/src/dna/dna-nt.lisp"
                                   "/home/dd/quicklisp/local-projects/small/src/dna/dna-origami.lisp"
                                   "/home/dd/quicklisp/local-projects/small/src/dna/dna-single-strand.lisp"
                                   "/home/dd/quicklisp/local-projects/small/src/dna/dna-strand.lisp"
                                   "/home/dd/quicklisp/local-projects/small/src/dna/packages.lisp"))))




(defun all-toplevel-forms (path)
  "Takes a path, returns all top level forms"
    (do* ((start 0 (recursive-regex::end match))
      (s (uiop:read-file-string path)
         (subseq s start))
      (match (recursive-regex::regex-recursive-groups "(?<parens>)" s)
        (recursive-regex::regex-recursive-groups "(?<parens>)" s))
      (all nil (progn (format t "AWE")
                      (if match
                          (push (recursive-regex::full-match match) all)
                          all))))
     ((null match) (nreverse all))))


(defun get-definitions (dw)
  "Take a doc-writer and returns a nested list list like
(('section-name' 'file-name' ('(defun ...)' '(defclass ...)'))"
  (mapcar #'(lambda (sec files)
                (mapcar #'(lambda (file-path)
                            (list (keyword->string sec)
                                  (file-namestring file-path)
                                  (all-toplevel-forms file-path)))
                        files))
                (serapeum:plist-keys  (sections dw))
                (serapeum:plist-values (sections dw))))


(defun allowedp (allowed def)
  "allowed: list of things we want included
 def: string of the definition"
  (dolist (a allowed) nil
    (when (string-equal a
                      (subseq def 1 (1+ (length a))))
                    (return t))))


(defun filter-definitions (allowed definitions)
  "allowed: list of things we want included
 definitions: list of the all definitions in a file"
  (remove-if-not #'(lambda (def)
                     (allowedp allowed def)
                     )
                 definitions))





(filter-definitions  (track dw) (third (first (second (get-definitions dw)))))

(string-equal "a" "A")

(defun write-section (dw sec)
  "dw: dorg:doc-writer
sec: keyword"
      )


(defun definitions-from-file (path))

(mapcar #'(lambda (sec files)
                (mapcar #'(lambda (file)
                            (format nil "Section: ~A~%File: ~A~%" sec file))
                        files))
              (serapeum:plist-keys  (sections dw))
              (serapeum:plist-values (sections dw)))





(with-open-file (f "/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp" )
  ())

(ql:quickload 'split-sequenc   )

(ql:quickload ')


(uiop:read-file-string "/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp")

(all-matches-as-strings "\\(defun"
                        (uiop:read-file-string "/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp"))

(all-matches-as-strings "\\(defun(?:^\\w)*+\\)"
                        (uiop:read-file-string "/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp"))

(all-matches-as-strings "\((?:[^)(]*(?:R)?)* +\)"
                        (uiop:read-file-string "/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp"))

(defun make-sexps (strs parens-seen)
  (let (s "")
    )
  )

(subseq "acd" 0 1)

(cond ((and (subseq ))))
(describe 'result-node)

(full-match (recursive-regex::regex-recursive-groups "(?<parens>)"
                                    (uiop:read-file-string
                                     "/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp")))
)



(all-toplevel-forms "/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp")



(regex-recursive-groups "(?<parens>)" nil)

(cdr (assoc "dna" docs :test #'string-equal))

(progn
  (defparameter docs
    '(("core" ("/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp"
               "/home/dd/quicklisp/local-projects/small/src/core/ht-helpers.lisp"
               "/home/dd/quicklisp/local-projects/small/src/core/linear-algebra.lisp"
               "/home/dd/quicklisp/local-projects/small/src/core/utils.lisp"))
      ("dna"  ("/home/dd/quicklisp/local-projects/small/src/dna/dna.lisp"
               "/home/dd/quicklisp/local-projects/small/src/dna/dna-helix-strand.lisp"
               "/home/dd/quicklisp/local-projects/small/src/dna/dna-nt.lisp"
               "/home/dd/quicklisp/local-projects/small/src/dna/dna-origami.lisp"
               "/home/dd/quicklisp/local-projects/small/src/dna/dna-single-strand.lisp"
               "/home/dd/quicklisp/local-projects/small/src/dna/dna-strand.lisp"
               "/home/dd/quicklisp/local-projects/small/src/dna/packages.lisp"))))


  (cdr (car docs))

  (mapcar #'(lambda (section)
              (mapcar #'(lambda (file)
                          (format t "section: ~A  File ~A\n" section file))
                      (cdr section))
              docs))

  (with-open-file (f "/home/dd/tmp/wtf.txt" :direction :output)
    (mapcar #'(lambda (section)
                (mapcar #'(lambda (file)
                            (format f "section: ~A  File ~A\n" (car section) file))
                        (cdr section)))
            docs)
    )
  (cdr (car docs))
  (type-of :a) (k)
)
