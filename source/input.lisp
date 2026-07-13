;;;; -- Terminal input decoding --

(in-package #:clinedi)

(defparameter *bracketed-paste-end*
  (concatenate 'string (string +escape-character+) "[201~")
  "Terminal sequence ending a bracketed paste payload.")

(defun input--terminal-control-p (character)
  "True when CHARACTER is a C0, DEL or C1 terminal control."
  (let ((code (char-code character)))
    (or (< code 32) (<= 127 code 159))))

(defun input--read-bracketed-paste (stream)
  "Read a complete bracketed-paste payload from STREAM."
  (let ((payload (make-string-output-stream))
        (matched 0)
        (marker *bracketed-paste-end*))
    (loop for character = (read-char stream nil nil)
          do (cond ((null character)
                    (return))
                   ((char= character (char marker matched))
                    (incf matched)
                    (when (= matched (length marker))
                      (setf matched 0)
                      (return)))
                   (t
                    (when (plusp matched)
                      (write-string marker payload :end matched)
                      (setf matched 0))
                    (if (char= character (char marker 0))
                        (setf matched 1)
                        (write-char character payload)))))
    (when (plusp matched)
      (write-string marker payload :end matched))
    (sanitize-text (get-output-stream-string payload))))

(defun input--csi-event (body stream)
  "Decode a CSI sequence BODY, reading paste payloads from STREAM."
  (cond ((string= body "A") :history-previous)
        ((string= body "B") :history-next)
        ((string= body "C") :right)
        ((string= body "D") :left)
        ((string= body "H") :home)
        ((string= body "F") :end)
        ((member body '("1~" "7~") :test #'string=) :home)
        ((string= body "3~") :delete)
        ((member body '("4~" "8~") :test #'string=) :end)
        ((string= body "200~")
         (list :paste (input--read-bracketed-paste stream)))
        (t :ignore)))

(defun input--read-csi (stream)
  "Read and decode the body of one CSI sequence from STREAM."
  (let ((body (make-string-output-stream)))
    (loop for character = (read-char stream nil nil)
          do (cond ((null character)
                    (return :ignore))
                   (t
                    (write-char character body)
                    (let ((code (char-code character)))
                      (when (<= #x40 code #x7e)
                        (return
                          (input--csi-event
                           (get-output-stream-string body) stream)))))))))

(defun input--read-escape (stream escape-delay)
  "Read an escape sequence from STREAM, waiting ESCAPE-DELAY seconds."
  (let ((first (or (read-char-no-hang stream nil nil)
                   (progn
                     (when (plusp escape-delay)
                       (sleep escape-delay))
                     (read-char-no-hang stream nil nil)))))
    (case first
      ((nil) :escape)
      (#\[ (input--read-csi stream))
      (#\O
       (case (read-char stream nil nil)
         (#\H :home)
         (#\F :end)
         (t :ignore)))
      (t :escape))))

(defun read-event (&key (stream *standard-input*) (escape-delay 0.002))
  "Read one semantic editing event from STREAM.

Printable input becomes (:INSERT text). Control and escape sequences become
editing keywords. Bracketed paste becomes one (:PASTE text) event, with terminal
controls sanitized before the text reaches an editor. Ctrl-D becomes
:END-OF-INPUT; physical stream EOF becomes :STREAM-END."
  (let ((character (read-char stream nil nil)))
    (cond ((null character)
           :stream-end)
          ((char= character +escape-character+)
           (input--read-escape stream escape-delay))
          (t
           (case (char-code character)
             (1 :home)                 ; C-a
             (2 :left)                 ; C-b
             (3 :interrupt)            ; C-c
             (4 :end-of-input)         ; C-d
             (5 :end)                  ; C-e
             (6 :right)                ; C-f
             (8 :backspace)            ; C-h
             (9 :complete)             ; Tab
             ((10 13) :submit)
             (11 :kill-to-end)         ; C-k
             (12 :clear-screen)        ; C-l
             (14 :history-next)        ; C-n
             (16 :history-previous)    ; C-p
             (21 :kill-line)           ; C-u
             (23 :kill-word)           ; C-w
             (127 :backspace)
             (t
              (if (input--terminal-control-p character)
                  :ignore
                  (list :insert (string character)))))))))
