;;;; -- Test package --

(defpackage #:clinedi/tests
  (:use #:cl)
  (:import-from #:clinedi
                #:grapheme-next-boundary
                #:grapheme-previous-boundary
                #:grapheme-boundary-at-or-after
                #:grapheme-cell-width
                #:text-cell-width
                #:text-cell-prefix
                #:text-cell-window
                #:wrap-text
                #:sanitize-text
                #:*presentation-enabled*
                #:ansi-colorize
                #:ansi-cursor-hide
                #:ansi-cursor-show
                #:ansi-strip
                #:ansi-display-width
                #:make-line-editor
                #:line-editor-create
                #:line-editor-text
                #:line-editor-cursor
                #:line-editor-history
                #:line-editor-set-text
                #:line-editor-clear
                #:line-editor-add-history
                #:line-editor-handle-event
                #:line-editor-render
                #:read-event
                #:screen-position
                #:write-display
                #:render-line
                #:print-candidates
                #:split-prompt
                #:edit-line)
  (:export #:run-tests))

(in-package #:clinedi/tests)

(defvar *test-failures* nil
  "Descriptions of failures from the current test run.")

(defun check-equal (name expected actual)
  "Record a failure under NAME unless EXPECTED and ACTUAL are EQUAL."
  (unless (equal expected actual)
    (push (format nil "~a: expected ~s, got ~s" name expected actual)
          *test-failures*))
  (values))

(defun check-true (name value)
  "Record a failure under NAME unless VALUE is true."
  (unless value
    (push (format nil "~a: expected a true value" name) *test-failures*))
  (values))
