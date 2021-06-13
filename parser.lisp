(in-package :dorg)
(setf str (nth 1 (third (caar (get-definitions dwo)))))

(defclass/std form-parser ()
  ((name :doc "The name of the form")
   (args :doc "The args of the form")
   (doc :doc "The documentation of the form")
   (body :doc "The body of the form")
   (form-type :doc "The type of form this is")
   (template :doc "A template for the string that the parser will produce")
   ))



(defclass/std defun-parser (form-parser)
  ((form-type :std "defun")
   (template :doc "A template for the string that the parser will produce"
             :std "Function ~A ~A~% ~~~A~~~%  {{< expand \"Source\" \"...\" >}}~A{{< /expand >}}")))

;;(get-doc (parse (make-instance 'defun-parser) str))

(defun get-doc (parser)
  "Returns a string formatted ready for org"
  (with-slots ((name name) (args args) (doc doc) (body body) (template template)) parser
    (format nil template name args doc body)))






(defun remove-leading-char (s &key (num 1))
  "Normally used to remove a leading paren"
  (subseq s num))





(defun get-form-args (form)
  (let ((match (recursive-regex::regex-recursive-groups
                "(?<parens>)"
                (remove-leading-char form))))
    (values (recursive-regex::full-match match)
            (recursive-regex::end match))))


(defun get-form-name (form)
 (second (uiop::split-string form)))

(defun get-form-type (form)
  (first (uiop::split-string (remove-leading-char (car dwds)))))

(defun get-form-body (form)
  (multiple-value-bind (args end) (get-form-args form)d
    (recursive-regex::full-match
     (recursive-regex::regex-recursive-groups
                "(?<parens>)"
                (remove-leading-char form :num end)))))

;;(get-form-body str)


(defun get-form-docs (form)
"Gets the documentation for forms"
 (second (uiop::split-string form :separator "\"")))

(defmethod parse ((fp form-parser) form)
  "form: toplevel definition form"
  (with-slots ((name name) (args args) (doc doc) (body body)) fp
    (setf args (get-form-args form)
          name (get-form-name form)
          doc (get-form-docs form)
          body (get-form-body form)))
  fp)


;;(get-doc (parse (make-instance 'defun-parser) str))
;; (get-form-args str)
;; (get-form-name str)
;; (get-form-docs str)
;; (get-form-docs str)
