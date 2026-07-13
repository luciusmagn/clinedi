;;;; -- ANSI presentation --

(in-package #:clinedi)

(defconstant +escape-character+ (code-char 27)
  "The ASCII escape character used in terminal control sequences.")

(defvar *presentation-enabled* t
  "Whether ANSI presentation helpers may emit terminal control sequences.")

(defparameter *ansi-color-codes*
  '((:black          . 30)
    (:red            . 31)
    (:green          . 32)
    (:yellow         . 33)
    (:blue           . 34)
    (:magenta        . 35)
    (:cyan           . 36)
    (:white          . 37)
    (:bright-black   . 90)
    (:bright-red     . 91)
    (:bright-green   . 92)
    (:bright-yellow  . 93)
    (:bright-blue    . 94)
    (:bright-magenta . 95)
    (:bright-cyan    . 96)
    (:bright-white   . 97))
  "Mapping from color keywords to standard SGR color codes.")

(defun ansi-color-code (color)
  "Return the SGR code for COLOR, defaulting to white."
  (or (cdr (assoc color *ansi-color-codes*)) 37))

(defun ansi-colorize (text color &key bold)
  "Wrap TEXT in the SGR sequence for COLOR, optionally BOLD.
Return TEXT unchanged when presentation is disabled."
  (if *presentation-enabled*
      (format nil "~c[~:[~;1;~]~dm~a~c[0m"
              +escape-character+ bold (ansi-color-code color) text
              +escape-character+)
      text))

(defun ansi-reverse-video (text)
  "Wrap TEXT in reverse video, unless presentation is disabled."
  (if *presentation-enabled*
      (format nil "~c[7m~a~c[0m" +escape-character+ text +escape-character+)
      text))

(defun ansi-cursor-up (lines)
  "Return the sequence moving the cursor LINES up, or an empty string."
  (if (and *presentation-enabled* (plusp lines))
      (format nil "~c[~dA" +escape-character+ lines)
      ""))

(defun ansi-cursor-down (lines)
  "Return the sequence moving the cursor LINES down, or an empty string."
  (if (and *presentation-enabled* (plusp lines))
      (format nil "~c[~dB" +escape-character+ lines)
      ""))

(defun ansi-cursor-column (column)
  "Return the sequence moving the cursor to zero-based COLUMN."
  (if *presentation-enabled*
      (format nil "~c[~dG" +escape-character+ (1+ column))
      ""))

(defun ansi-cursor-hide ()
  "Return the sequence that hides the terminal cursor."
  (if *presentation-enabled*
      (format nil "~c[?25l" +escape-character+)
      ""))

(defun ansi-cursor-show ()
  "Return the sequence that makes the terminal cursor visible."
  (if *presentation-enabled*
      (format nil "~c[?25h" +escape-character+)
      ""))

(defun ansi-clear-below ()
  "Return the sequence clearing from the cursor to the screen end."
  (if *presentation-enabled*
      (format nil "~c[J" +escape-character+)
      ""))

(defun ansi-clear-line-right ()
  "Return the sequence clearing from the cursor to the line end."
  (if *presentation-enabled*
      (format nil "~c[K" +escape-character+)
      ""))

(defun ansi-clear-screen ()
  "Return the sequence clearing the whole screen and homing the cursor."
  (if *presentation-enabled*
      (format nil "~c[H~c[2J" +escape-character+ +escape-character+)
      ""))

(defun ansi--skip-csi (string start)
  "Return the index just past a CSI sequence body starting at START."
  (loop for index from start below (length string)
        for code = (char-code (char string index))
        when (<= #x40 code #x7e)
          return (1+ index)
        finally (return (length string))))

(defun ansi--skip-osc (string start)
  "Return the index just past an OSC sequence body starting at START."
  (loop for index from start below (length string)
        for character = (char string index)
        when (char= character (code-char 7))
          return (1+ index)
        when (and (char= character +escape-character+)
                  (< (1+ index) (length string))
                  (char= (char string (1+ index)) #\\))
          return (+ index 2)
        finally (return (length string))))

(defun ansi--escape-end (string index)
  "Return the index just after STRING's escape sequence at INDEX."
  (if (< (1+ index) (length string))
      (case (char string (1+ index))
        (#\[ (ansi--skip-csi string (+ index 2)))
        (#\] (ansi--skip-osc string (+ index 2)))
        (t (+ index 2)))
      (1+ index)))

(defun ansi-strip (string)
  "Remove ANSI CSI, OSC and two-byte escape sequences from STRING."
  (with-output-to-string (clean)
    (let ((index 0)
          (length (length string)))
      (loop while (< index length)
            do (let ((character (char string index)))
                 (cond ((and (char= character +escape-character+)
                             (< (1+ index) length))
                        (setf index (ansi--escape-end string index)))
                       ((char= character +escape-character+)
                        (incf index))
                       (t
                        (write-char character clean)
                        (incf index))))))))

(defun ansi--visible-slice (string start end)
  "Return STRING controls and visible characters between START and END.

All trusted ANSI controls are retained so the slice enters and leaves the same
presentation state as STRING. START and END index ANSI-stripped characters."
  (with-output-to-string (slice)
    (let ((index 0)
          (visible-index 0))
      (loop while (< index (length string))
            do (if (char= (char string index) +escape-character+)
                   (let ((sequence-end (ansi--escape-end string index)))
                     (write-string string slice
                                          :start index
                                          :end sequence-end)
                     (setf index sequence-end))
                   (progn
                     (when (<= start visible-index (1- end))
                       (write-char (char string index) slice))
                     (incf visible-index)
                     (incf index)))))))

(defun ansi-display-width (string)
  "Return the number of visible terminal cells STRING occupies."
  (text-cell-width (ansi-strip string)))
