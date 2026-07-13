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
       (format nil "first~c~csecond~%" (code-char 27) #\return)
       :raw-mode-function (lambda () t)
       :bracketed-paste-p nil)
    (declare (ignore output restores))
    (check-equal "modified enter inserts a logical line"
                 (format nil "first~%second")
                 line)
    (check-equal "multiline input still submits with enter" :line kind))
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
  (flet ((complete (text cursor)
           (declare (ignore text))
           (values (- cursor 3)
                   '("print" "printf" "private")
                   '("PRINT" "PRINTF" "PRIVATE")))

         (accept (candidate)
           (concatenate 'string candidate " ")))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "pri~c~c!~%" #\tab #\tab)
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :completion-function #'complete
         :completion-accept-function #'accept)
      (declare (ignore restores))
      (check-equal "Tab cycles live completion candidates"
                   "printf !"
                   line)
      (check-equal "typed input submits after retaining selection" :line kind)
      (check-true "live completion renders supplied labels"
                  (search "PRINTF" output)))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "pri~c~c[Z!~%" #\tab (code-char 27))
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :completion-function #'complete
         :completion-accept-function #'accept)
      (declare (ignore output restores))
      (check-equal "Shift-Tab cycles live completions backward"
                   "private !"
                   line)
      (check-equal "backward completion remains submittable" :line kind))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "pri~c~c[C~%" #\tab (code-char 27))
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :completion-function #'complete
         :completion-accept-function #'accept)
      (declare (ignore output restores))
      (check-equal "right arrow navigates completion grid" "printf " line)
      (check-equal "Enter submits the selected grid completion" :line kind))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "pri~c~c[C~%" #\tab (code-char 27))
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :completion-function #'complete
         :completion-accept-function #'accept
         :completion-arrangement :vertical)
      (declare (ignore output restores))
      (check-equal "vertical completion ignores horizontal navigation"
                   "print "
                   line)
      (check-equal "vertical completion submits selected candidate"
                   :line
                   kind))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "pri~c~cX~%" #\tab (code-char 27))
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :completion-function #'complete
         :completion-accept-function #'accept)
      (declare (ignore output restores))
      (check-equal "Escape restores text preceding completion" "pri" line)
      (check-equal "restored completion text remains submittable" :line kind)))
  (let ((source-history (vector "older"))
        (callback-histories nil))
    (multiple-value-bind (line kind output restores)
        (terminal-editor-test--read
         (format nil "a~c[C~%" (code-char 27))
         :history source-history
         :raw-mode-function (lambda () t)
         :bracketed-paste-p nil
         :suggestion-function
         (lambda (text history)
           (push history callback-histories)
           (and (string= text "a") "abc")))
      (declare (ignore output restores))
      (check-equal "right accepts suggestion" "abc" line)
      (check-equal "suggestion submit kind" :line kind))
    (check-true "suggestion callback reuses editor-owned history"
                (and callback-histories
                     (every (lambda (history)
                              (eq history (first callback-histories)))
                            callback-histories)))
    (check-true "suggestion callback cannot mutate caller history"
                (not (eq source-history (first callback-histories)))))
  (values))
