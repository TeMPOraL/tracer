;;;; SBCL-specific implementation of the Tracer.

(in-package #:tracer)

(defvar *trace-events* nil "A list of trace entries, pushed onto from the beginning.")

(defvar *original-trace-start-breakpoint-fun* #'sb-debug::trace-start-breakpoint-fun "Original SBCL function being overwritten by the tracer.")
(defvar *original-trace-end-breakpoint-fun* #'sb-debug::trace-end-breakpoint-fun "Original SBCL function being overwritten by the tracer.")

(defvar *clock-reset-fun* nil)
(defvar *clock-get-time-fun* nil)


;;; Timer
;;; TODO: make it so that user can plug a high-resolution timer here. -- Jacek Złydach, 2019-10-18

(sb-ext:defglobal *hack-clock-jitter* 0 "A crude hack because our clock has too little resolution.")
(declaim (type fixnum *hack-clock-jitter*))

(unless (>= internal-time-units-per-second 1000)
  (warn "Tracer clock may not havve enough precision to be useful for profiling use."))

;;; TODO: this needs to be a function that can be changed between invocations of tracing!
;;; I want to allow for injecting higher resolution clocks if available.
;;; -- Jacek Złydach, 2019-11-01

(defun get-current-time-usec ()
  "Get current time with microsecond resolution."
  (sb-ext:atomic-incf *hack-clock-jitter*)
  (+ (* (get-internal-real-time) 1000)
     *hack-clock-jitter*))

(declaim (ftype (function () alexandria:non-negative-fixnum) get-current-time-usec)
         (inline get-current-time-usec))
(defun get-current-time-usec-nojitter ()
  "Get current time with microsecond resolution. No extra jitter involved."
  (declare (optimize (speed 3)))
  (the alexandria:non-negative-fixnum (* (get-internal-real-time) 1000)))

;;; XXX: below is our new, usec clock -- Jacek Złydach, 2019-11-02
(let ((clock-offset 0))
  (declare (type alexandria:non-negative-fixnum clock-offset))
  (defun %%start-clock ()
    (setf clock-offset (sb-kernel::get-time-of-day)))
  (defun %%get-time-usec ()
    (multiple-value-bind (sec usec)
        (sb-kernel::get-time-of-day)
      (+ (* (- sec clock-offset) 1000000) usec)))
  (defun %%time (thunk)
    (let ((start (%%get-time-usec)))
      (funcall thunk)
      (- (%%get-time-usec)  start)))
  (setf *clock-reset-fun* #'%%start-clock
        *clock-get-time-fun* #'%%get-time-usec))

(declaim (ftype (function () alexandria:non-negative-fixnum) get-current-time)
         (inline get-current-time))
(defun get-current-time ()
  (funcall *clock-get-time-fun*))



(defun post-process-entries (entries &key correct-zero-duration)
  "Destructively modify `ENTRIES', making it more compact and, if `CORRECT-ZERO-DURATION' is T,
changing zero-length events to have 1us length, also modifying other times to offset for that.
`ENTRIES' is expected to be in order entries were added. The function maintain separate offsets per (process, thread) pair.
Returns a processed list, to replace old value `ENTRIES'. As additional values, returns the total accumulated clock offset,
and the stacks containing unclosed duration entries, keyed by thread."
  (let ((offset 0)
        (stacks (make-hash-table :test 'equal)))
    (dolist (entry entries entries)
      ;; Always update event time to account for clock offset.
      (incf (trace-event-timestamp entry) offset)

      ;; Match starting and ending events to offset clock in case of zero-length events, and to convert
      ;; matching pairs of Duration events into Complete events.
      (let ((entry-ht-id (cons 1 (trace-event-thread entry)))) ;1 is the currently supported PID
        (ecase (trace-event-phase entry)
          (:enter
           ;; Push the :enter entry to stack.
           (push entry (gethash entry-ht-id stacks)))
          (:exit
           (let ((begin-event (first (gethash entry-ht-id stacks))))
             (if (equalp (trace-event-name begin-event)
                         (trace-event-name entry))
                 (progn
                   ;; Actual post-processing happens here.
                   ;; If zero-length and correct-zero-duration is on, update close time and offset.
                   (when (and correct-zero-duration
                              (= (trace-event-timestamp begin-event)
                                 (trace-event-timestamp entry)))
                     (incf (trace-event-timestamp entry))
                     (incf offset))

                   ;; Convert task into complete task + counter
                   (setf (trace-event-phase begin-event) :complete
                         (trace-event-phase entry) :skip ;TODO: counters, if any, go here -- Jacek Złydach, 2019-11-04
                         (trace-event-duration begin-event) (- (trace-event-timestamp entry) (trace-event-timestamp begin-event))
                         (trace-event-args begin-event) `(:in ,(trace-event-args begin-event) :out ,(trace-event-args entry)))

                   ;; Pop the updated entry from stack.
                   (pop (gethash entry-ht-id stacks)))
                 (warn "Recorded entries misordered; expected ~A, got ~A." (trace-event-name begin-event) (trace-event-name entry))))))))
    ;; Go over the list again, and remove "skip" entries.
    (alexandria:deletef entries :skip :key #'trace-event-phase)
    (values entries
            offset
            stacks)))


;;; Tracing process

(defun my-trace-start-breakpoint-fun (info)
  (let (conditionp)
    (values
     (lambda (frame bpt &rest args)
       (declare (ignore bpt))
       (sb-debug::discard-invalid-entries frame)
       (let ((condition (sb-debug::trace-info-condition info))
             (wherein (sb-debug::trace-info-wherein info)))
         (setq conditionp
               (and (not sb-debug::*in-trace*)
                    (or (not condition)
                        (apply (cdr condition) frame args))
                    (or (not wherein)
                        (sb-debug::trace-wherein-p frame wherein)))))
       (when conditionp
         (when (sb-debug::trace-info-encapsulated info)
           (sb-ext:atomic-push (make-trace-event-fast :enter
                                                      (sb-debug::trace-info-what info)
                                                      (bt:current-thread)
                                                      (get-current-time)
                                                      args
                                                      nil
                                                      nil)
                               *trace-events*))
         ;; TODO: perhaps remove this, it seems unneeded -- Jacek Złydach, 2019-11-05
         (with-standard-io-syntax
           (apply #'sb-debug::trace-maybe-break info (sb-debug::trace-info-break info) "before"
                  frame args))))
     (lambda (frame cookie)
       (declare (ignore frame))
       (push (cons cookie conditionp) sb-debug::*traced-entries*)))))

(declaim (ftype (function (sb-debug::trace-info) function) my-trace-end-breakpoint-fun))
(defun my-trace-end-breakpoint-fun (info)
  (lambda (frame bpt values cookie)
    (declare (ignore bpt))
    (unless (eq cookie (caar sb-debug::*traced-entries*))
      (setf sb-debug::*traced-entries*
            (member cookie sb-debug::*traced-entries* :key #'car)))

    (let ((entry (pop sb-debug::*traced-entries*)))
      (when (and (not (sb-debug::trace-info-untraced info))
                 (or (cdr entry)
                     (let ((cond (sb-debug::trace-info-condition-after info)))
                       (and cond (apply #'funcall (cdr cond) frame values)))))
        (sb-ext:atomic-push (make-trace-event-fast :exit
                                                   (sb-debug::trace-info-what info)
                                                   (bt:current-thread)
                                                   (get-current-time)
                                                   values
                                                   nil
                                                   nil)
                            *trace-events*)

        (apply #'sb-debug::trace-maybe-break info (sb-debug::trace-info-break-after info) "after"
               frame values)))))

(defun install-tracing-overrides ()
  (sb-ext:unlock-package (find-package 'sb-debug))
  (setf (symbol-function 'sb-debug::trace-start-breakpoint-fun) #'my-trace-start-breakpoint-fun
        (symbol-function 'sb-debug::trace-end-breakpoint-fun) #'my-trace-end-breakpoint-fun)
  (sb-ext:lock-package (find-package 'sb-debug)))

(defun uninstall-tracing-overrides ()
  (sb-ext:unlock-package (find-package 'sb-debug))
  (setf (symbol-function 'sb-debug::trace-start-breakpoint-fun) *original-trace-start-breakpoint-fun*
        (symbol-function 'sb-debug::trace-end-breakpoint-fun) *original-trace-end-breakpoint-fun*)
  (sb-ext:lock-package (find-package 'sb-debug)))

;;; FIXME: This should not be a macro. -- Jacek Złydach, 2019-10-18
(defmacro start-tracing (&rest specs)
  `(progn
     (install-tracing-overrides)
     (trace :encapsulate t :methods t ,@specs)))

(defun stop-tracing ()
  (untrace)
  (uninstall-tracing-overrides)
  #+nil(setf *trace-events* (nreverse *trace-events*))
  (multiple-value-bind (events offset stacks)
      (post-process-entries (nreverse *trace-events*))
    (declare (ignore offset stacks))
    (setf *trace-events* events))
  ;; TODO: report offsets and stacks -- Jacek Złydach, 2019-11-05
  (values))

(defun reset-tracing ()
  (setf *trace-events* nil
        *hack-clock-jitter* 0))

(defun get-tracing-report-data ()
  *trace-events*)
