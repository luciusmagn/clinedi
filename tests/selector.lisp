;;;; -- Candidate Selector Tests --

(in-package #:clinedi/tests)

(defun run-selector-tests ()
  "Run candidate selection, navigation and viewport regression tests."
  (let ((selector (make-selector :items '(alpha beta gamma delta)
                                 :visible-count 2)))
    (check-equal "selector owns its candidate container"
                 '(alpha beta gamma delta)
                 (selector-items selector))
    (check-equal "selector begins on the first candidate"
                 'alpha
                 (selector-selected-item selector))
    (multiple-value-bind (items start selection)
        (selector-window selector)
      (check-equal "initial selector window" '(alpha beta) items)
      (check-equal "initial selector window starts at zero" 0 start)
      (check-equal "initial selector selection is local" 0 selection))
    (selector-move selector 2)
    (multiple-value-bind (items start selection)
        (selector-window selector)
      (check-equal "selector window follows forward movement"
                   '(beta gamma)
                   items)
      (check-equal "forward selector window start" 1 start)
      (check-equal "forward local selector index" 1 selection))
    (selector-move selector 2)
    (check-equal "selector movement wraps forward"
                 'alpha
                 (selector-selected-item selector))
    (selector-move selector -1)
    (check-equal "selector movement wraps backward"
                 'delta
                 (selector-selected-item selector))
    (selector-set-items selector '(alpha beta gamma delta))
    (check-equal "equal candidates preserve selection"
                 'delta
                 (selector-selected-item selector))
    (selector-set-items selector '(newer newest))
    (check-equal "changed candidates reset selection"
                 'newer
                 (selector-selected-item selector))
    (multiple-value-bind (action value)
        (selector-handle-event selector :history-next)
      (check-equal "down event changes selector" :changed action)
      (check-equal "movement event has no payload" nil value))
    (multiple-value-bind (action value)
        (selector-handle-event selector :complete)
      (check-equal "completion accepts selection" :accept action)
      (check-equal "completion returns opaque value" 'newest value))
    (multiple-value-bind (action value)
        (selector-handle-event selector :escape)
      (check-equal "escape cancels selector" :cancel action)
      (check-equal "cancel has no payload" nil value))
    (multiple-value-bind (action value)
        (selector-handle-event selector '(:insert "x"))
      (check-equal "text input is not selector policy" :unhandled action)
      (check-equal "unhandled event has no payload" nil value)))
  (let ((selector (make-selector :items #() :visible-count 3)))
    (multiple-value-bind (items start selection)
        (selector-window selector)
      (check-equal "empty selector window" nil items)
      (check-equal "empty selector window start" 0 start)
      (check-equal "empty selector has no local selection" nil selection))
    (multiple-value-bind (action value)
        (selector-handle-event selector :submit)
      (check-equal "empty selector cannot accept" :unhandled action)
      (check-equal "empty selector has no accepted value" nil value)))
  (let ((selector (make-selector :items '(nil))))
    (multiple-value-bind (action value)
        (selector-handle-event selector :submit)
      (check-equal "nil is a valid opaque candidate" :accept action)
      (check-equal "nil candidate remains nil" nil value)))
  (values))
