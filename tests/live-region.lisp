;;;; -- Live-region tests --

(in-package #:clinedi/tests)

(defun live-region-tests--count (needle text)
  "Return the number of non-overlapping NEEDLE occurrences in TEXT."
  (loop with start = 0
        for position = (search needle text :start2 start)
        while position
        count 1
        do (setf start (+ position (length needle)))))

(defun live-region-tests--signals-error-p (function)
  "True when calling FUNCTION signals an error."
  (handler-case
      (progn
        (funcall function)
        nil)
    (error ()
      t)))

(defun run-live-region-tests ()
  "Run scrollback-safe live-region lifecycle regression tests."
  (let ((stream (make-string-output-stream))
        (flushes 0))
    (let* ((write-function
             (lambda (text)
               (write-string text stream)))
           (region
             (make-live-region
              :columns 6
              :write-function write-function
              :flush-function (lambda () (incf flushes))))
           (text (format nil "> abcdef~%x~%")))
      (live-region-present region text :cursor 5)
      (check-true "presented live region is visible"
                  (live-region-visible-p region))
      (check-equal "wrapped live region row count"
                   4
                   (live-region-row-count region))
      (check-equal "wrapped live cursor row"
                   0
                   (live-region-cursor-row region))
      (check-equal "wrapped live cursor column"
                   5
                   (live-region-cursor-column region))
      (live-region-append region (format nil "FINAL~%~%"))
      (let ((output (get-output-stream-string stream)))
        (check-equal "scrollback output is appended once"
                     1
                     (live-region-tests--count "FINAL" output))
        (check-true "live repaint never clears below"
                    (not (search (format nil "~c[J" (code-char 27))
                                 output))))
      (live-region-resize region 3)
      (check-equal "resize updates live-region width"
                   3
                   (live-region-columns region))
      (check-equal "resize reflows retained presentation"
                   5
                   (live-region-row-count region))
      (live-region-suspend region)
      (check-true "suspended live region is hidden"
                  (not (live-region-visible-p region)))
      (check-equal "suspended live region has no painted rows"
                   0
                   (live-region-row-count region))
      (live-region-resume region)
      (check-true "resumed live region is visible"
                  (live-region-visible-p region))
      (call-with-live-region-suspended
       region
       (lambda ()
         (check-true "callback runs with region hidden"
                     (not (live-region-visible-p region)))))
      (check-true "callback restores live region"
                  (live-region-visible-p region))
      (let ((styled (ansi-colorize text :green)))
        (live-region-present region text :cursor 0 :display styled))
      (check-true "visible-content mismatch is rejected"
                  (live-region-tests--signals-error-p
                   (lambda ()
                     (live-region-present region "plain"
                                          :display "different"))))
      (check-true "unmodeled geometry controls are rejected"
                  (live-region-tests--signals-error-p
                   (lambda ()
                     (live-region-present region
                                          (format nil "bad~c" #\tab)))))
      (live-region-dismiss region)
      (check-true "dismissed live region is hidden"
                  (not (live-region-visible-p region)))
      (check-equal "dismissed live region has no rows"
                   0
                   (live-region-row-count region))
      (check-true "live-region operations flush terminal output"
                  (plusp flushes))))
  (values))
