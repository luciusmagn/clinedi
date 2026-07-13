;;;; -- Rendering tests --

(in-package #:clinedi/tests)

(defun run-render-tests ()
  "Run ANSI and wrapped-rendering regression tests."
  (let ((combined (format nil "e~c" (code-char #x301))))
    (check-equal "wide glyph ends at edge"
                 '(1 0 t)
                 (multiple-value-list
                  (screen-position "aa猫" :columns 4)))
    (check-equal "wide glyph wraps intact"
                 '(1 2 nil)
                 (multiple-value-list
                  (screen-position "aaa猫" :columns 4)))
    (check-equal "combining glyph advances one cell"
                 '(0 1 nil)
                 (multiple-value-list
                  (screen-position combined :columns 4))))
  (let ((cases `((""                    (0 2 nil))
                 ("a"                   (0 3 nil))
                 ("ab"                  (1 0 t))
                 (,(format nil "ab~%")   (1 0 nil))
                 (,(format nil "ab~%~%") (2 0 nil))
                 (,(format nil "abc~%")  (2 0 nil)))))
    (dolist (case cases)
      (destructuring-bind (text expected) case
        (check-equal (format nil "screen position for ~s" text)
                     expected
                     (multiple-value-list
                      (screen-position text
                                       :prompt-width 2
                                       :columns 4))))))
  (let ((text (format nil "a~%b")))
    (check-equal "cursor before newline"
                 '(0 1 nil)
                 (multiple-value-list
                  (screen-position text :columns 4 :end 1)))
    (check-equal "cursor after newline"
                 '(1 0 nil)
                 (multiple-value-list
                  (screen-position text :columns 4 :end 2)))
    (check-equal "display emits explicit carriage return"
                 (format nil "a~%~cb" #\return)
                 (with-output-to-string (stream)
                   (write-display text :stream stream))))
  (check-equal "completion columns use cell widths"
               (format nil "猫  a   ~%~c" #\return)
               (with-output-to-string (stream)
                 (print-candidates '("猫" "a")
                                   :columns 8
                                   :stream stream)))
  (multiple-value-bind (preamble prompt)
      (split-prompt (format nil "first~%second> "))
    (check-equal "prompt preamble" (format nil "first~%") preamble)
    (check-equal "editable prompt" "second> " prompt))
  (let ((rendered
          (with-output-to-string (stream)
            (render-line "pri"
                         :cursor 3
                         :prompt-width 7
                         :columns 80
                         :previous-row 0
                         :suggestion "ntf example"
                         :stream stream))))
    (check-true "redraw starts by hiding cursor"
                (let ((hide (ansi-cursor-hide)))
                  (and (<= (length hide) (length rendered))
                       (string= hide rendered :end2 (length hide)))))
    (check-true "redraw restores cursor"
                (let ((show (ansi-cursor-show)))
                  (and (<= (length show) (length rendered))
                       (string= show rendered
                                :start2 (- (length rendered)
                                           (length show)))))))
  (let ((*presentation-enabled* nil))
    (check-equal "disabled presentation leaves text plain"
                 "plain"
                 (ansi-colorize "plain" :red)))
  (check-equal "ANSI display width ignores styling"
               2
               (ansi-display-width (ansi-colorize "猫" :green)))
  (check-equal "ANSI strip removes OSC"
               "safe"
               (ansi-strip
                (format nil "~c]0;title~csafe" (code-char 27) (code-char 7))))
  (values))
