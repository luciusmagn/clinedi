;;;; -- Scrollback-safe live regions --

(in-package #:clinedi)

(defclass live-region ()
  ((write-function
    :initarg :write-function
    :reader live-region--write-function
    :type function
    :documentation "Function accepting one trusted terminal output string.")
   (flush-function
    :initarg :flush-function
    :reader live-region--flush-function
    :type function
    :documentation "Function making pending terminal output visible.")
   (columns
    :initarg :columns
    :accessor live-region-columns
    :type integer
    :documentation "Positive terminal width used for live-region geometry.")
   (text
    :initform ""
    :accessor live-region--text
    :type string
    :documentation "Plain visible content of the most recent presentation.")
   (display
    :initform ""
    :accessor live-region--display
    :type string
    :documentation "Trusted styled content of the most recent presentation.")
   (cursor
    :initform 0
    :accessor live-region--cursor
    :type integer
    :documentation "Character index of the desired cursor within TEXT.")
   (presented-p
    :initform nil
    :accessor live-region--presented-p
    :type boolean
    :documentation "Whether the region retains content that can be repainted.")
   (visible-p
    :initform nil
    :accessor live-region-visible-p
    :type boolean
    :documentation "Whether the retained presentation is currently painted.")
   (row-count
    :initform 0
    :accessor live-region-row-count
    :type integer
    :documentation "Number of currently painted physical terminal rows.")
   (cursor-row
    :initform 0
    :accessor live-region-cursor-row
    :type integer
    :documentation "Zero-based live row holding the painted cursor.")
   (cursor-column
    :initform 0
    :accessor live-region-cursor-column
    :type integer
    :documentation "Zero-based terminal column holding the painted cursor."))
  (:documentation
   "Transient terminal content that remains beneath ordinary scrollback output."))


;;;; -- Construction and Validation --

(defun live-region--standard-write (text)
  "Write trusted terminal TEXT to the current standard output."
  (write-string text *standard-output*)
  (values))

(defun live-region--standard-flush ()
  "Flush the current standard output."
  (force-output *standard-output*)
  (values))

(defun make-live-region (&key (columns 80)
                              (write-function #'live-region--standard-write)
                              (flush-function #'live-region--standard-flush))
  "Create a live region using COLUMNS and terminal output callbacks."
  (unless (and (integerp columns) (plusp columns))
    (error 'type-error :datum columns :expected-type '(integer 1 *)))
  (check-type write-function function)
  (check-type flush-function function)
  (make-instance 'live-region
                 :columns columns
                 :write-function write-function
                 :flush-function flush-function))

(defun live-region--geometry-text-p (text)
  "True when TEXT contains only printable characters and modeled line breaks."
  (loop for character across text
        for code = (char-code character)
        always (or (char= character #\newline)
                   (char= character #\return)
                   (and (>= code 32)
                        (not (<= 127 code 159))))))

(defun live-region--validate-display (text display)
  "Require TEXT and styled DISPLAY to have identical safe visible content."
  (check-type text string)
  (check-type display string)
  (unless (live-region--geometry-text-p text)
    (error "Live-region geometry text contains an unmodeled terminal control."))
  (unless (string= text (ansi-strip display))
    (error "Live-region display text does not preserve its visible content."))
  (values))


;;;; -- Terminal Output --

(defun live-region--write (region text)
  "Write trusted TEXT through REGION's terminal callback."
  (funcall (live-region--write-function region) text)
  (values))

(defun live-region--flush (region)
  "Make REGION's pending terminal output visible."
  (funcall (live-region--flush-function region))
  (values))

(defun live-region--terminal-text (text)
  "Return TEXT with explicit carriage returns after every newline."
  (with-output-to-string (stream)
    (write-display text :stream stream)))

(defun live-region--write-newline (region)
  "Advance REGION's terminal to column zero on a fresh row."
  (live-region--write region (format nil "~c~c" #\linefeed #\return))
  (values))

(defun live-region--paint (region)
  "Paint REGION's retained presentation and restore its logical cursor."
  (unless (live-region--presented-p region)
    (return-from live-region--paint region))
  (let* ((text    (live-region--text region))
         (display (live-region--display region))
         (columns (live-region-columns region))
         (cursor  (live-region--cursor region)))
    (multiple-value-bind (cursor-row cursor-column cursor-wrap)
        (screen-position text :columns columns :end cursor)
      (declare (ignore cursor-wrap))
      (multiple-value-bind (end-row end-column pending-wrap)
          (screen-position text :columns columns)
        (declare (ignore end-column))
        (live-region--write region (ansi-cursor-hide))
        (unwind-protect
             (progn
               (live-region--write region
                                   (live-region--terminal-text display))
               (when pending-wrap
                 (live-region--write-newline region))
               (live-region--write region
                                   (ansi-cursor-up (- end-row cursor-row)))
               (live-region--write region
                                   (ansi-cursor-column cursor-column)))
          (live-region--write region (ansi-cursor-show)))
        (setf (live-region-row-count region) (1+ end-row)
              (live-region-cursor-row region) cursor-row
              (live-region-cursor-column region) cursor-column
              (live-region-visible-p region) t)
        (live-region--flush region))))
  region)


;;;; -- Public Lifecycle --

(defun live-region-suspend (region)
  "Retract REGION without forgetting the presentation that should resume."
  (check-type region live-region)
  (when (live-region-visible-p region)
    (let ((total (live-region-row-count region)))
      (live-region--write
       region
       (ansi-cursor-down
        (- total 1 (live-region-cursor-row region))))
      (loop for row downfrom (1- total) to 0
            do (live-region--write region (string #\return))
               (live-region--write region (ansi-clear-line-right))
               (when (plusp row)
                 (live-region--write region (ansi-cursor-up 1))))
      (setf (live-region-visible-p region) nil
            (live-region-row-count region) 0
            (live-region-cursor-row region) 0
            (live-region-cursor-column region) 0)
      (live-region--flush region)))
  region)

(defun live-region-resume (region)
  "Repaint REGION's retained presentation when it is currently retracted."
  (check-type region live-region)
  (when (and (live-region--presented-p region)
             (not (live-region-visible-p region)))
    (live-region--paint region))
  region)

(defun call-with-live-region-suspended (region function)
  "Call FUNCTION while REGION is retracted, then restore its presentation."
  (check-type region live-region)
  (check-type function function)
  (let ((visible-p (live-region-visible-p region)))
    (when visible-p
      (live-region-suspend region))
    (unwind-protect
         (funcall function)
      (when visible-p
        (live-region-resume region)))))

(defmacro with-live-region-suspended ((region) &body body)
  "Evaluate BODY with REGION retracted, restoring it after every exit."
  `(call-with-live-region-suspended ,region (lambda () ,@body)))

(defun live-region-present (region text &key (cursor (length text))
                                             (display text))
  "Replace REGION with TEXT and trusted styled DISPLAY, placing its cursor.

DISPLAY may contain trusted ANSI styling but must have TEXT as its exact visible
content. CURSOR is normalized to a complete grapheme boundary within TEXT."
  (check-type region live-region)
  (live-region--validate-display text display)
  (check-type cursor integer)
  (let ((safe-cursor
          (grapheme-boundary-at-or-after
           text
           (min (length text) (max 0 cursor)))))
    (live-region-suspend region)
    (setf (live-region--text region) (copy-seq text)
          (live-region--display region) (copy-seq display)
          (live-region--cursor region) safe-cursor
          (live-region--presented-p region) t)
    (live-region--paint region)))

(defun live-region-append (region text &key (display text))
  "Append TEXT to ordinary scrollback and repaint REGION beneath it.

DISPLAY may add trusted ANSI styling while preserving TEXT's visible content.
A missing final newline is supplied so the repainted region never shares the
last output row."
  (check-type region live-region)
  (live-region--validate-display text display)
  (when (plusp (length text))
    (call-with-live-region-suspended
     region
     (lambda ()
       (live-region--write region (live-region--terminal-text display))
       (unless (char= (char text (1- (length text))) #\newline)
         (live-region--write-newline region))
       (live-region--flush region))))
  region)

(defun live-region-dismiss (region)
  "Retract REGION and forget its retained presentation."
  (check-type region live-region)
  (live-region-suspend region)
  (setf (live-region--text region) ""
        (live-region--display region) ""
        (live-region--cursor region) 0
        (live-region--presented-p region) nil)
  region)

(defun live-region-resize (region columns)
  "Reflow REGION for positive COLUMNS without replaying scrollback output."
  (check-type region live-region)
  (unless (and (integerp columns) (plusp columns))
    (error 'type-error :datum columns :expected-type '(integer 1 *)))
  (unless (= columns (live-region-columns region))
    (let ((visible-p (live-region-visible-p region)))
      (when visible-p
        (live-region-suspend region))
      (setf (live-region-columns region) columns)
      (when visible-p
        (live-region-resume region))))
  region)
