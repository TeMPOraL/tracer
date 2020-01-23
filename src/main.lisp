(in-package #:tracer)

;;; Trace operations:
;;; 1. Reset
;;; 2. Trace
;;; 2.5 snapshot tracing?
;;; 3. Stop tracing
;;; 4. Save report

#-sbcl (error "This system currently works only on SBCL.")

(defvar *tracing-p* nil "Is currently tracing activity happening?")

;;; Trace info entry type, for function call
;;; - Timestamp
;;; - Function name
;;; - Function args maybe? (trace-with-args), on enter
;;; - Function return value, on exit
;;; - Beginning or ending
;;; - Thread ID



;;; This prints a representation of the return values delivered.
;;; First, this checks to see that cookie is at the top of
;;; *TRACED-ENTRIES*; if it is not, then we need to adjust this list
;;; to determine the correct indentation for output. We then check to
;;; see whether the function is still traced and that the condition
;;; succeeded before printing anything.

(defmacro with-tracing ((&rest specs) &body body)
  `(unwind-protect
        (progn
          (reset-tracing)
          (start-tracing ,@specs)
          (progn
            ,@body))
     (stop-tracing)))



;;; FIXME: this still has an SBCL dependency -- Jacek Złydach, 2019-10-18
(defun function-name->name-and-category (function-name)
  (etypecase function-name
    (symbol
     (values (symbol-name function-name) (package-name (symbol-package function-name))))
    (cons
     (ecase (first function-name)
       (setf
        (values (format nil "~S" function-name) (package-name (symbol-package (second function-name)))))
       ((method sb-pcl::combined-method)
        (values (remove #\Newline (format nil "~S" function-name))
                (if (consp (second function-name))
                    (package-name (symbol-package (second (second function-name))))
                    (package-name (symbol-package (second function-name))))))))))

(defgeneric post-process-arg (arg)
  (:method ((arg t))
    "Passthrough method."
    (or (ignore-errors
          (prin1-to-string arg))
        "!!Error printing argument!!"))
  (:documentation "A hook useful for changing the printed representation of input and return values."))

(defmethod post-process-arg ((arg sequence))
  (if (every (lambda (el)  (typep el 'number)) arg)
      (format nil "[~{~F~^, ~}]" (coerce arg 'list))
      (call-next-method)))

;;; FIXME: Something breaks if not collecting args, and :skip-args is NIL. Probably the getf in printing. -- Jacek Złydach, 2019-11-05
(defun trace-event->json (trace-event &key (skip-args nil))
  (flet ((sanitize-and-format-args-list (argslist)
           (if skip-args "\"skipped\""
               (substitute #\Space #\Newline (format nil "[~{~S~^, ~}]" (mapcar #'post-process-arg argslist))))))
    (ecase (trace-event-phase trace-event)
      (:enter
       (multiple-value-bind (name category)
           (function-name->name-and-category (trace-event-name trace-event))
         (format nil
                 "{ \"name\" : ~S, \"cat\" : ~S, \"ph\" : \"B\", \"pid\" : 1, \"tid\" : ~D, \"ts\" : ~D, \"args\" : { \"in\" : ~A }}"
                 name
                 category
                 (sb-impl::get-lisp-obj-address (trace-event-thread trace-event))
                 (trace-event-timestamp trace-event)
                 (sanitize-and-format-args-list (trace-event-args trace-event)))))
      (:exit
       (multiple-value-bind (name category)
           (function-name->name-and-category (trace-event-name trace-event))
         (format nil
                 "{ \"name\" : ~S, \"cat\" : ~S, \"ph\" : \"E\", \"pid\" : 1, \"tid\" : ~D, \"ts\" : ~D, \"args\" : { \"out\" : ~A }}"
                 name
                 category
                 (sb-impl::get-lisp-obj-address (trace-event-thread trace-event))
                 (trace-event-timestamp trace-event)
                 (sanitize-and-format-args-list (trace-event-args trace-event)))))
      (:complete
       (multiple-value-bind (name category)
           (function-name->name-and-category (trace-event-name trace-event))
         (format nil
                 "{ \"name\" : ~S, \"cat\" : ~S, \"ph\" : \"X\", \"pid\" : 1, \"tid\" : ~D, \"ts\" : ~D, \"dur\" : ~D,  \"args\" : { \"in\" : ~A, \"out\" : ~A }}"
                 name
                 category
                 (sb-impl::get-lisp-obj-address (trace-event-thread trace-event))
                 (trace-event-timestamp trace-event)
                 (trace-event-duration trace-event)
                 (sanitize-and-format-args-list (getf (trace-event-args trace-event) :in))
                 (sanitize-and-format-args-list (getf (trace-event-args trace-event) :out))))))))

(defun thread->json (thread)
  (format nil
          "{ \"name\" : \"thread_name\", \"ph\" : \"M\", \"pid\" : 1, \"tid\" : ~D, \"args\" : { \"name\" : ~S }}"
          (sb-impl::get-lisp-obj-address thread)
          (bt:thread-name thread)))

(defun extract-threads (events)
  (loop
     with uniques-ht = (make-hash-table :test #'eq)
     for event in events
     do
       (setf (gethash (trace-event-thread event) uniques-ht) t)
     finally
       (return (alexandria:hash-table-keys uniques-ht))))

;;; FIXME: save with streams instead? -- Jacek Złydach, 2019-10-14
(defun save-report (output-file-name &key (skip-args t))
  (with-open-file (stream output-file-name :direction :output :if-exists :supersede)
    ;; TODO: preamble -- Jacek Złydach, 2019-10-14
    (format stream "{~%\"traceEvents\" :  [~%")
    (loop
       for (entry . restp) on *trace-events*
       do
         (write-string (trace-event->json entry :skip-args skip-args) stream)
         (when restp
           (write-string "," stream)
           (terpri stream)))
    (loop
       for (thread . restp) on (extract-threads *trace-events*)
       initially
         (write-string "," stream)
         (terpri stream)
       do
         (write-string (thread->json thread) stream)
         (when restp
           (write-string "," stream)
           (terpri stream)))

    (format stream "~&],
\"displayTimeUnit\" : \"ms\",
\"application\" : \"FIXME\",
\"version\" : \"FIXME\",
\"traceTime\" : ~S
}"
            " TODO local-time independent time"
            ;;(local-time:format-timestring nil (local-time:now))
            ))
  (values))



;;; Helper function for blacklisting symbols when tracing whole packages.
(defun package-symbols-except (name &rest exceptions)
  (let (symbols
        (package (sb-impl::find-undeleted-package-or-lose name)))
    (do-all-symbols (symbol (find-package name))
      (when (eql package (symbol-package symbol))
        (when (and (fboundp symbol)
                   (not (macro-function symbol))
                   (not (special-operator-p symbol)))
          (push symbol symbols))
        (let ((setf-name `(setf ,symbol)))
          (when (fboundp setf-name)
            (push setf-name symbols)))))
    (set-difference symbols exceptions :key (lambda (x)
                                              (if (consp x)
                                                  (string (second x))
                                                  (string x))) :test #'string-equal)))
