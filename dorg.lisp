(in-package :defclass-std)
(ql:quickload :defclass-std :serapeum :alexandria)

(defun key->str (k)
  (subseq (write-to-string k) 1))

(defun section-files (dw sec)
  "dw: dorg:doc-writer
sec: keyword"
  (getf (sections dw) sec))




(defclass/std doc-writer ()
  ((sections :doc "A p-list with keys being sections names and values being an order list of files to generate docs for that section")
   (hugo-base-dir :doc "The root folder of the hugo site, docs will go to /content/:sec/filename ")
))


(setf d (make-instance 'doc-writer
               :sections '(:core ("/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp"
                                  "/home/dd/quicklisp/local-projects/small/src/core/ht-helpers.lisp"
                                  "/home/dd/quicklisp/local-projects/small/src/core/linear-algebra.lisp"
                                  "/home/dd/quicklisp/local-projects/small/src/core/utils.lisp")
                           (:dna  ("/home/dd/quicklisp/local-projects/small/src/dna/dna.lisp"
                                   "/home/dd/quicklisp/local-projects/small/src/dna/dna-helix-strand.lisp"
                                   "/home/dd/quicklisp/local-projects/small/src/dna/dna-nt.lisp"
                                   "/home/dd/quicklisp/local-projects/small/src/dna/dna-origami.lisp"
                                   "/home/dd/quicklisp/local-projects/small/src/dna/dna-single-strand.lisp"
                                   "/home/dd/quicklisp/local-projects/small/src/dna/dna-strand.lisp"
                                   "/home/dd/quicklisp/local-projects/small/src/dna/packages.lisp")))))

(section-files d :core)

(defun write-section (dw sec)
  "dw: dorg:doc-writer
sec: keyword"
  (let ((sec-str (key->str sec))
        ()


  )









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

(full-match (regex-recursive-groups "(?<parens>)"
                                    (uiop:read-file-string
                                     "/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp")))

(do* ((start 0 (end match))
      (s (uiop:read-file-string
          "/home/dd/quicklisp/local-projects/small/src/core/chem-obj.lisp")
         (subseq s start))
      (match (regex-recursive-groups "(?<parens>)" s)
        (regex-recursive-groups "(?<parens>)" s))
      (all nil (progn (format t "AWE")
                      (if match
                          (push (full-match match) all)
                          all))))
     ((null match) (nreverse all)))




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
