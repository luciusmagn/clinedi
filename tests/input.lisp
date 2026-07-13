;;;; -- Input-decoder tests --

(in-package #:clinedi/tests)

(defun input-test--event (text)
  "Decode one event from TEXT."
  (with-input-from-string (stream text)
    (read-event :stream stream :escape-delay 0)))

(defun input-test--escape-sequence (body)
  "Return an escape-prefixed terminal sequence containing BODY."
  (concatenate 'string (string (code-char 27)) body))

(defun run-input-tests ()
  "Run semantic input-decoder regression tests."
  (check-equal "printable input event"
               '(:insert "猫")
               (input-test--event "猫"))
  (check-equal "physical stream end is distinct from control-D"
               :stream-end
               (input-test--event ""))
  (check-equal "control-D editing event"
               :end-of-input
               (input-test--event (string (code-char 4))))
  (check-equal "control-left event"
               :left
               (input-test--event (string (code-char 2))))
  (check-equal "raw control-backspace event"
               :kill-word
               (input-test--event (string (code-char 8))))
  (check-equal "arrow-up event"
               :history-previous
               (input-test--event (input-test--escape-sequence "[A")))
  (check-equal "delete event"
               :delete
               (input-test--event (input-test--escape-sequence "[3~")))
  (check-equal "legacy alt-enter event"
               :insert-newline
               (input-test--event
                (concatenate 'string
                             (string (code-char 27))
                             (string #\return))))
  (check-equal "CSI-u shift-enter event"
               :insert-newline
               (input-test--event
                (input-test--escape-sequence "[13;2u")))
  (check-equal "modify-other-keys alt-enter event"
               :insert-newline
               (input-test--event
                (input-test--escape-sequence "[27;3;13~")))
  (dolist (case '(("CSI-u control-backspace with BS" "[8;5u")
                  ("CSI-u control-backspace with DEL" "[127;5u")
                  ("modify-other-keys control-backspace with BS" "[27;5;8~")
                  ("modify-other-keys control-backspace with DEL" "[27;5;127~")))
    (check-equal (first case)
                 :kill-word
                 (input-test--event
                  (input-test--escape-sequence (second case)))))
  (check-equal "lone escape event"
               :escape
               (input-test--event (string (code-char 27))))
  (check-equal "unknown CSI event"
               :ignore
               (input-test--event (input-test--escape-sequence "[9~")))
  (check-equal "bracketed paste is one event"
               (list :paste (format nil "one~%猫"))
               (input-test--event
                (concatenate 'string
                             (input-test--escape-sequence "[200~")
                             (format nil "one~%猫")
                             (input-test--escape-sequence "[201~"))))
  (values))
