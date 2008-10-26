(in-package #:cl-containers-documentation)

(defmethod additional-markdown-extensions-for-system 
    append ((system (eql (asdf:find-system 'lift-documentation))))
  '(clcl))

(defsimple-extension clcl
  "*CL-Containers*")

(defextension (list-classes
	       :arguments ((root :required)
			   (filter :required :whole)))
  (ecase phase
    (:parse
     ;; could syntax check here
     )
    (:render
     (format *output-stream* "~s ~s" root filter))))

#|

(mopu:subclasses 'cl-containers::basic-generator)

(mopu:subclasses 'cl-containers::abstract-container)

(mopu:subclasses 'cl-containers::concrete-container)

(defun build-documentation-report 
    (&optional (document (or *multi-document*
			     (first *documents*))))
  (cl-markdown::build-documentation-report
   (asdf:system-relative-pathname
    'db.agraph.documentation "resources/documentation-report.md")
   (asdf:system-relative-pathname
    'db.agraph.documentation "output/documentation-report.html")
    document
    (list :db.agraph
	  :net.cluster
	  :db.agraph.parser
	  :db.agraph.serializer
	  :db.agraph.sparql) 
   :format :html 
   :excluded-symbols (symbols-explicitly-undocumented-for-agraph)
   :docs-package (find-package :db.agraph)
   :search-locations (agraph-search-locations))) 

(defun build-summary-file (doc-root)
  (cl-markdown:markdown 
   (let ((docs 
	  (sort
	   (loop for info in *documents* 
	      when (typep info 'cons) collect
	      (destructuring-bind (in out doc) info
		(list :in in :out out :doc doc 
		      :title 
		      (let ((cl-markdown:*current-document* doc))
			(cl-markdown:document-property :title)))))
	   #'string-lessp
	   :key (lambda (plist) (getf plist :title)))))
     (db.agraph:string+ 
      "{include resources/header.md}
{set-property html yes}
{set-property author \"Franz Incorporated\"}
{set-property title \"Generated Documents | AllegroGraph {property agraph-version}\"}
{set-property style-sheet styles}

<div id=\"main-content\">

# AllegroGraph Documentation 
"
      (apply #'db.agraph:string+
	     (loop for info in docs 
		for index from 1 collect
		(db.agraph:string+
		 "* [" (getf info :title) "][" index "]
")))
      "
" 
      (apply #'db.agraph:string+
	     (loop for info in docs 
		for index from 1 collect
		(db.agraph:string+  
		 " [" index "]: " 
		 (enough-namestring (getf info :out) doc-root) "
")))
      "</div>

{comment include resources/footer.md}
"
))
   :format :html
   :stream (make-pathname 
	    :name "summary" :type "html" 
	    :defaults doc-root)))

(defun agraph-search-locations ()
  (list 
   (asdf:system-relative-pathname 'db.agraph.documentation
				  "")
   (asdf:system-relative-pathname 'db.agraph.documentation
				  "resources/")))

(defun markdown-extensions-for-agraph ()
  '(cl-markdown::docs cl-markdown::docs-index
    cl-markdown::glossary cl-markdown::links-list
    cl-markdown::comment cl-markdown::ifdef
    cl-markdown::abbrev cl-markdown-user:docs-group
   
    cl-markdown::ag-eval-freetext-stop-words
    cl-markdown::ag-eval-freetext-example-phrase
    cl-markdown::ag-eval-java-agraph-cfg-file))
  
(defun cl-markdown-for-agraph (filter in out 
			       &rest args &key &allow-other-keys)
  (remf args :root)
  (push (cons :|Content-Type| "text/html; charset=UTF-8")
	(getf args :properties))
  (push (list filter t)
	(getf args :properties))
  (push (cons :search-locations 
	      (agraph-search-locations))
	(getf args :properties))
  (let* ((md (apply #'markdown:markdown 
		    in
		    :stream out
		    :format :html
		    :additional-extensions (markdown-extensions-for-agraph)
		    args)))
    (push (list in out md) *documents*)
    md))

(defun cl-markdown-many-for-agraph (filter docs &rest args)
  (remf args :root)
  (push (cons :|Content-Type| "text/html; charset=UTF-8")
	(getf args :properties))
  (push (list filter t) (getf args :properties))
  (push (cons :search-locations 
	      (agraph-search-locations))
	(getf args :properties))
  (let ((*package* (find-package :cl-markdown)))
    (multiple-value-bind (main outputs)
	(apply #'markdown:markdown-many
	       docs
	       :format :html
	       :additional-extensions (markdown-extensions-for-agraph)
	       args)
      (loop for (in nil) in docs
	  for (md dest) in outputs do (push (list in dest md) *documents*))
      main)))

(cl-markdown:add-documentation-strategy 
 'thing-names-property-p 'property 'property)

(defun thing-names-property-p (thing)
  (and (symbolp thing)
       (db.agraph::ag-property-definition thing)))

(defmethod cl-markdown:find-documentation (thing (strategy (eql :property)))
   (db.agraph::property-documentation 
    (db.agraph::ag-property-definition 
     (intern (symbol-name thing) :db.agraph))))


|#