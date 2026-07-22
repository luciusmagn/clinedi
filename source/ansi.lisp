;;;; -- ANSI presentation --

(in-package #:clinedi)

(defconstant +escape-character+ (code-char 27)
  "The ASCII escape character used in terminal control sequences.")

(defvar *presentation-enabled* t
  "Whether ANSI presentation helpers may emit terminal control sequences.")

(defun ansi--color-designator (color)
  "Return COLOR when Colorist recognizes it, otherwise basic white."
  (if (or (cl-colorist:color-p color)
          (member color (cl-colorist:basic-color-names)))
      color
      :white))

(defun ansi-colorize (text color &key bold)
  "Wrap TEXT in the SGR sequence for COLOR, optionally BOLD.
Return TEXT unchanged when presentation is disabled."
  (if *presentation-enabled*
      (cl-colorist:paint text
                         :foreground (ansi--color-designator color)
                         :bold bold
                         :level :indexed)
      text))

(defun ansi-reverse-video (text)
  "Wrap TEXT in reverse video, unless presentation is disabled."
  (if *presentation-enabled*
      (cl-colorist:paint text :reverse t :level :basic)
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

(defun ansi-strip (string)
  "Remove ANSI control sequences from STRING."
  (cl-colorist:strip-ansi string))

(defun ansi--visible-slice (string start end)
  "Return STRING controls and visible characters between START and END.

All trusted ANSI controls are retained so the slice enters and leaves the same
presentation state as STRING. START and END index ANSI-stripped characters."
  (with-output-to-string (slice)
    (let ((index 0)
          (visible-index 0))
      (loop while (< index (length string))
            for control-end = (cl-colorist:ansi-control-end string index)
            do (if control-end
                   (progn
                     (write-string string slice :start index :end control-end)
                     (setf index control-end))
                   (progn
                     (when (<= start visible-index (1- end))
                       (write-char (char string index) slice))
                     (incf visible-index)
                     (incf index)))))))

(defun ansi-display-width (string)
  "Return the number of visible terminal cells STRING occupies."
  (text-cell-width (ansi-strip string)))
