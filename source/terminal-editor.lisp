;;;; -- Blocking terminal editor --

(in-package #:clinedi)

(defun terminal-editor--default-size ()
  "Return conservative terminal dimensions for callers without a backend."
  (values 24 80))

(defun terminal-editor--false ()
  "Return false for the default unavailable raw-mode backend."
  nil)

(defun terminal-editor--no-op ()
  "Perform no operation."
  (values))

(defun terminal-editor--common-prefix (strings)
  "Return the longest common prefix of nonempty STRINGS."
  (when strings
    (reduce (lambda (first second)
              (subseq first 0 (or (mismatch first second) (length first))))
            (rest strings)
            :initial-value (first strings))))

(defun terminal-editor--replace-range (editor start end replacement)
  "Replace EDITOR text from START through END with REPLACEMENT."
  (let ((text (line-editor-text editor)))
    (line-editor-set-text
     editor
     (concatenate 'string
                  (subseq text 0 start)
                  replacement
                  (subseq text end))
     :cursor (+ start (length replacement)))))

(defun terminal-editor--suggestion (editor suggestion-function)
  "Return a valid full-text suggestion for EDITOR, or NIL."
  (when (and suggestion-function
             (= (line-editor-cursor editor)
                (length (line-editor-text editor))))
    (let* ((text (line-editor-text editor))
           (suggestion
             ;; The editor already owns this copied history vector. Passing it
             ;; directly avoids duplicating every entry on each keystroke.
             (funcall suggestion-function
                      text
                      (slot-value editor 'history))))
      (when (and (stringp suggestion)
                 (> (length suggestion) (length text))
                 (string= text suggestion :end2 (length text)))
        suggestion))))

(defun terminal-editor--complete
    (editor &key completion-function common-prefix-function
                 completion-accept-function prompt prompt-width columns
                 previous-row highlight-function stream)
  "Apply or display completion and return the resulting previous row."
  (unless completion-function
    (write-char (code-char 7) stream)
    (force-output stream)
    (return-from terminal-editor--complete previous-row))
  (let ((text (line-editor-text editor))
        (cursor (line-editor-cursor editor)))
    (multiple-value-bind (start candidates displays)
        (funcall completion-function text cursor)
      (cond ((or (null candidates)
                 (not (integerp start))
                 (minusp start)
                 (> start cursor))
             (write-char (code-char 7) stream)
             (force-output stream)
             previous-row)
            ((null (rest candidates))
             (let* ((candidate (first candidates))
                    (replacement
                      (funcall completion-accept-function candidate)))
               (terminal-editor--replace-range
                editor start cursor replacement)
               previous-row))
            (t
             (let* ((common (funcall common-prefix-function candidates))
                    (prefix-length (- cursor start)))
               (if (> (length common) prefix-length)
                   (progn
                     (terminal-editor--replace-range editor start cursor common)
                     previous-row)
                   (progn
                     (render-line text
                                  :cursor (length text)
                                  :prompt-width prompt-width
                                  :columns columns
                                  :previous-row previous-row
                                  :highlight-function highlight-function
                                  :stream stream)
                     (render--write-newline stream)
                     (print-candidates (or displays candidates)
                                       :columns columns
                                       :stream stream)
                     (render--write-prompt
                      prompt prompt-width columns stream)))))))))

(defun terminal-editor--finish
    (editor kind &key payload prompt-width columns previous-row
                      highlight-function stream)
  "Finish EDITOR input of KIND and return the blocking API values."
  (ecase kind
    (:submit
     (render--finish-line
      payload
      :prompt-width prompt-width
      :columns columns
      :previous-row previous-row
      :highlight-function highlight-function
      :stream stream)
     (values payload :line))
    (:interrupt
     (render--finish-line
      (line-editor-text editor)
      :prompt-width prompt-width
      :columns columns
      :previous-row previous-row
      :marker "^C"
      :highlight-function highlight-function
      :stream stream)
     (values nil :abort))
    (:end-of-input
     (render--finish-line
      (line-editor-text editor)
      :prompt-width prompt-width
      :columns columns
      :previous-row previous-row
      :highlight-function highlight-function
      :stream stream)
     (values nil :eof))))

(defun terminal-editor--fallback (prompt input-stream output-stream)
  "Read a plain line after PROMPT when raw mode is unavailable."
  (write-string prompt output-stream)
  (force-output output-stream)
  (let ((line (cl:read-line input-stream nil nil)))
    (if line
        (values line :line)
        (values nil :eof))))

(defun edit-line
    (prompt &key (history #())
                 (input-stream *standard-input*)
                 (output-stream *standard-output*)
                 (terminal-size-function #'terminal-editor--default-size)
                 (raw-mode-function #'terminal-editor--false)
                 (restore-function #'terminal-editor--no-op)
                 (highlight-function #'identity)
                 completion-function
                 (common-prefix-function #'terminal-editor--common-prefix)
                 (completion-accept-function #'identity)
                 suggestion-function
                 (bracketed-paste-p t))
  "Edit one line under PROMPT and return line and result kind.

The result kind is :LINE, :ABORT or :EOF. HISTORY is copied into an incremental
LINE-EDITOR. Terminal ownership remains with the caller through size, raw-mode
and restore callbacks. Highlighting, completion and suggestion callbacks add
application policy without coupling Clinedi to a parser or history store.

When raw mode is unavailable, this function prints the final prompt line and
uses ordinary READ-LINE."
  (multiple-value-bind (preamble editable-prompt)
      (split-prompt prompt)
    (multiple-value-bind (rows columns)
        (funcall terminal-size-function)
      (declare (ignore rows))
      (setf columns (max 1 columns))
      (let ((editor (make-line-editor :history history))
            (prompt-width (ansi-display-width editable-prompt))
            (previous-row 0)
            (raw-p nil))
        (write-display preamble :stream output-stream)
        (unwind-protect
             (progn
               (setf raw-p (funcall raw-mode-function))
               (unless raw-p
                 (return-from edit-line
                   (terminal-editor--fallback
                    editable-prompt input-stream output-stream)))
               (when bracketed-paste-p
                 (format output-stream "~c[?2004h" +escape-character+))
               (setf previous-row
                     (render--write-prompt editable-prompt prompt-width columns
                                           output-stream))
               (loop
                 (let* ((suggestion
                          (terminal-editor--suggestion
                           editor suggestion-function))
                        (suffix
                          (and suggestion
                               (subseq suggestion
                                       (line-editor-cursor editor)))))
                   (setf previous-row
                         (render-line
                          (line-editor-text editor)
                          :cursor (line-editor-cursor editor)
                          :prompt-width prompt-width
                          :columns columns
                          :previous-row previous-row
                          :suggestion suffix
                          :highlight-function highlight-function
                          :stream output-stream))
                   (let ((event (read-event :stream input-stream)))
                     (cond ((and (eq event :right) suggestion
                                 (= (line-editor-cursor editor)
                                    (length (line-editor-text editor))))
                            (line-editor-set-text
                             editor suggestion :cursor (length suggestion)))
                           ((eq event :ignore)
                            nil)
                           (t
                            (multiple-value-bind (action payload)
                                (line-editor-handle-event editor event)
                              (case action
                                ((:submit :interrupt :end-of-input)
                                 (return
                                   (terminal-editor--finish
                                    editor action
                                    :payload payload
                                    :prompt-width prompt-width
                                    :columns columns
                                    :previous-row previous-row
                                    :highlight-function highlight-function
                                    :stream output-stream)))
                                (:complete
                                 (setf previous-row
                                       (terminal-editor--complete
                                        editor
                                        :completion-function completion-function
                                        :common-prefix-function
                                        common-prefix-function
                                        :completion-accept-function
                                        completion-accept-function
                                        :prompt editable-prompt
                                        :prompt-width prompt-width
                                        :columns columns
                                        :previous-row previous-row
                                        :highlight-function highlight-function
                                        :stream output-stream)))
                                (:clear-screen
                                 (write-string (ansi-clear-screen)
                                               output-stream)
                                 (write-display preamble :stream output-stream)
                                 (setf previous-row
                                       (render--write-prompt
                                        editable-prompt prompt-width columns
                                        output-stream)))
                                ((:continue :escape)
                                 nil)))))))))
          (when raw-p
            (when bracketed-paste-p
              (format output-stream "~c[?2004l" +escape-character+)
              (force-output output-stream)))
          (funcall restore-function))))))
