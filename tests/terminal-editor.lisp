;;;; -- Blocking editor tests --

(in-package #:clinedi/tests)

(defun terminal-editor-test--size ()
  "Return fixed test terminal dimensions."
  (values 24 80))

(defun terminal-editor-test--read (input &rest arguments)
  "Call EDIT-LINE with INPUT and return its values, output and restore count."
  (let ((restores 0))
    (with-input-from-string (input-stream input)
      (let ((output-stream (make-string-output-stream)))
        (multiple-value-bind (line kind)
            (apply #'edit-line
                   "preamble
> "
                   :input-stream input-stream
                   :output-stream output-stream
                   :terminal-size-function #'terminal-editor-test--size
                   :restore-function (lambda () (incf restores))
                   arguments)
          (values line kind
                  (get-output-stream-string output-stream)
                  restores))))))

(defun run-terminal-editor-tests ()
  "Run blocking frontend and callback regression tests."
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (format nil "plain~%")
       :raw-mode-function (lambda () nil))
    (check-equal "plain fallback line" "plain" line)
    (check-equal "plain fallback kind" :line kind)
    (check-equal "plain fallback prompt"
                 (format nil "preamble~%~c> " #\return)
                 output)
    (check-equal "plain fallback restores terminal" 1 restores))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (format nil "abc~%")
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil)
    (check-equal "raw editor preserves submit payload" "abc" line)
    (check-equal "raw editor submit kind" :line kind)
    (check-true "raw editor renders entered text" (search "abc" output))
    (check-equal "raw editor restores terminal" 1 restores))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       "partial"
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil)
    (declare (ignore output restores))
    (check-equal "physical EOF discards partial raw input" nil line)
    (check-equal "physical EOF result kind" :eof kind))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (format nil "~c~%" #\tab)
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil
       :completion-function
       (lambda (text cursor)
         (declare (ignore text cursor))
         (values 0 '("print") '("print")))
       :completion-accept-function
       (lambda (candidate)
         (concatenate 'string candidate " ")))
    (declare (ignore output restores))
    (check-equal "completion callback replacement" "print " line)
    (check-equal "completion callback submit kind" :line kind))
  (multiple-value-bind (line kind output restores)
      (terminal-editor-test--read
       (format nil "a~c[C~%" (code-char 27))
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil
       :suggestion-function
       (lambda (text history)
         (declare (ignore history))
         (and (string= text "a") "abc")))
    (declare (ignore output restores))
    (check-equal "right accepts suggestion" "abc" line)
    (check-equal "suggestion submit kind" :line kind))
  (values))
