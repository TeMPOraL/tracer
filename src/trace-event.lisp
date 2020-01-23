;;;; Trace Event - the base data unit that's stored by the tracer.
;;;; Trace event format mimicks the Google Chrome's Tracing convention with PID assumed to be `*TRACE-EVENT-DEFAULT-PID*' by default,
;;;; and category being inferred when dumping the trace to file.

(in-package #:tracer)



(defvar *trace-event-default-pid* 1 "The default value for PID for the trace events. This library is currently intended for use within a single process only.")

(defvar +arg-converter-ignore-all+ (constantly 'skipped) "A converter that rejects all parameters.")
(defvar +arg-converter-passthrough+ (lambda (&rest args) args) "A converter that remembers all args as is, without modifying them.")
(defvar +arg-converter-store-only-simple-objects+ (lambda (&rest args)
                                                    (mapcar (lambda (arg)
                                                              (typecase arg
                                                                ((or boolean character number symbol)
                                                                 arg)
                                                                (t
                                                                 (type-of arg))))
                                                            args))
  "A converter that remembers directly only objects of simple types, that cannot or are very unlikely to be destructively modified.")
(defvar +arg-converter-store-only-simple-objects-and-strings+ (lambda (&rest args)
                                                                (mapcar (lambda (arg)
                                                                          (typecase arg
                                                                            ((or boolean character number symbol string)
                                                                             arg)
                                                                            (t
                                                                             (type-of arg))))
                                                                        args))
  "Like `+ARG-CONVERTER-STORE-ONLY-SIMPLE-OBJECTS+', but also records strings as-is, hoping they don't get destructively modified too much.")

(defvar *default-arg-converter* +arg-converter-ignore-all+)
(defvar *tracing-arg-converters* (make-hash-table :test 'equal))



;;; The format of trace event; created primarily for reference, though later on we might upgrade to vector storage, and then it'll come helpful.
(defstruct (trace-event (:type list))
  "A single event being traced. "
  (phase :undefined :type keyword)
  (name nil :type (or symbol cons))
  (thread 0 :type t)
  (timestamp 0 :type alexandria:non-negative-fixnum)
  (args nil :type t)
  (duration 0 :type (or null alexandria:non-negative-fixnum))
  (id nil :type t))

;;; TODO: define accessors manually, to save performance? or somehow optimize it.  -- Jacek Złydach, 2019-11-04

(declaim (inline convert-args))
(defun convert-args (traced-fn-name args)
  "Depending on the function being traced, named `TRACED-FN-NAME', and the value of `*DEFAULT-ARG-CONVERTER*'
convert the list of arguments `ARGS' to a form suitable for storing with the trace event, using a converter
registered under `*TRACING-ARG-CONVERTERS*'.
Returns the representation of `ARGS' to store."
  (declare (optimize (speed 3)))
  (apply (the function (gethash traced-fn-name *tracing-arg-converters* *default-arg-converter*))
         args))

(declaim (inline make-trace-event-fast))
(defun make-trace-event-fast (phase name thread timestamp args duration id)
  "Like `MAKE-TRACE-EVENT', but inlined, unsafe and without typechecking."
  (declare (optimize (speed 3)))
  (list phase name thread timestamp (convert-args name args) duration id))
