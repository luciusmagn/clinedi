;;;; -- Terminal rendering --

(in-package #:clinedi)

(defun screen-position (text &key (prompt-width 0) (columns 80)
                                  (end (length text)))
  "Return the screen position after TEXT through END.

PROMPT-WIDTH is the number of cells preceding TEXT and COLUMNS is the terminal
width. The values are row, zero-based column and whether an exact-width wrap
still needs to be materialized."
  (setf columns (max 1 columns)
        end (min end (length text)))
  (let ((row (floor prompt-width columns))
        (column (mod prompt-width columns))
        (pending-wrap (and (plusp prompt-width)
                           (zerop (mod prompt-width columns))))
        (index 0))
    (loop while (< index end)
          do (let ((character (char text index)))
               (cond ((char= character #\newline)
                      (if pending-wrap
                          (setf pending-wrap nil)
                          (incf row))
                      (setf column 0)
                      (incf index))
                     ((char= character #\return)
                      (setf column 0
                            pending-wrap nil)
                      (incf index))
                     (t
                      (let* ((next (grapheme-next-boundary text index end))
                             (width (min columns
                                         (grapheme-cell-width
                                          text index next))))
                        (when (plusp width)
                          (when pending-wrap
                            (setf pending-wrap nil))
                          ;; Wide glyphs wrap as units instead of splitting at
                          ;; the terminal's last column.
                          (when (and (plusp column)
                                     (> (+ column width) columns))
                            (incf row)
                            (setf column 0))
                          (incf column width)
                          (when (= column columns)
                            (incf row)
                            (setf column 0
                                  pending-wrap t)))
                        (setf index next)))))
          finally (return (values row column pending-wrap)))))

(defun write-display (text &key (stream *standard-output*))
  "Write TEXT to STREAM, following newlines with explicit returns."
  (loop for character across text
        do (write-char character stream)
        when (char= character #\newline)
          do (write-char #\return stream))
  (values))

(defun render--write-newline (stream)
  "Write a terminal newline without relying on output post-processing."
  (write-char #\linefeed stream)
  (write-char #\return stream)
  (values))

(defun render--write-prompt (prompt prompt-width columns stream)
  "Write the static editable PROMPT and return its ending row."
  (write-string prompt stream)
  (multiple-value-bind (row column pending-wrap)
      (screen-position "" :prompt-width prompt-width :columns columns)
    (declare (ignore column))
    (when pending-wrap
      (render--write-newline stream))
    (force-output stream)
    row))

(defun render-line (text &key (cursor (length text))
                              (prompt-width 0)
                              (columns 80)
                              (previous-row 0)
                              suggestion
                              (highlight-function #'identity)
                              (stream *standard-output*))
  "Redraw dynamic line-editor TEXT and return the cursor's resulting row.

The static prompt is described by PROMPT-WIDTH but is not repainted. CURSOR is
a character index. PREVIOUS-ROW is the row where the prior redraw left it.
SUGGESTION is unaccepted suffix text rendered after TEXT. HIGHLIGHT-FUNCTION
may add ANSI styling, but must preserve the visible content of TEXT."
  (let* ((suffix (or suggestion ""))
         (display (concatenate 'string text suffix)))
    (multiple-value-bind (prompt-row prompt-column prompt-wrap)
        (screen-position "" :prompt-width prompt-width :columns columns)
      (declare (ignore prompt-wrap))
      (multiple-value-bind (target-row target-column target-wrap)
          (screen-position text :prompt-width prompt-width :columns columns
                                :end cursor)
        (declare (ignore target-wrap))
        (multiple-value-bind (end-row end-column exact-wrap)
            (screen-position display :prompt-width prompt-width
                                     :columns columns)
          (declare (ignore end-column))
          (write-string (ansi-cursor-hide) stream)
          (unwind-protect
               (progn
                 (write-string (ansi-cursor-up (- previous-row prompt-row))
                               stream)
                 (write-string (ansi-cursor-column prompt-column) stream)
                 (write-string (ansi-clear-below) stream)
                 (write-display (funcall highlight-function text)
                                :stream stream)
                 (when (plusp (length suffix))
                   (write-display (ansi-colorize suffix :bright-black)
                                  :stream stream))
                 (when exact-wrap
                   (render--write-newline stream))
                 (write-string (ansi-cursor-up (- end-row target-row)) stream)
                 (write-string (ansi-cursor-column target-column) stream))
            (write-string (ansi-cursor-show) stream)
            (force-output stream))
          target-row)))))

(defun render--finish-line
    (text &key (prompt-width 0) (columns 80) (previous-row 0) marker
               (highlight-function #'identity) (stream *standard-output*))
  "Park after TEXT, optionally print MARKER, and advance to a fresh line."
  (render-line text
               :cursor (length text)
               :prompt-width prompt-width
               :columns columns
               :previous-row previous-row
               :highlight-function highlight-function
               :stream stream)
  (when marker
    (write-string (ansi-colorize marker :bright-black) stream))
  (render--write-newline stream)
  (force-output stream)
  (values))

(defun print-candidates (displays &key (columns 80)
                                       (stream *standard-output*))
  "Print completion DISPLAYS in terminal-cell-aligned columns."
  (let ((count (length displays)))
    (cond ((zerop count)
           nil)
          ((> count 120)
           (format stream "(~d possibilities)" count)
           (render--write-newline stream))
          (t
           (let* ((width (+ 2 (loop for display in displays
                                    maximize (text-cell-width display))))
                  (per-row (max 1 (floor (max 1 columns) (max 1 width)))))
             (loop for index from 1
                   for display in displays
                   for display-width = (text-cell-width display)
                   do (write-string display stream)
                      (loop repeat (- width display-width)
                            do (write-char #\space stream))
                   when (zerop (mod index per-row))
                     do (render--write-newline stream)
                   finally
                      (unless (zerop (mod count per-row))
                        (render--write-newline stream)))))))
  (force-output stream)
  (values))

(defun split-prompt (prompt)
  "Split PROMPT into preamble and final editable line values."
  (let ((break (position #\newline prompt :from-end t)))
    (if break
        (values (subseq prompt 0 (1+ break)) (subseq prompt (1+ break)))
        (values "" prompt))))
