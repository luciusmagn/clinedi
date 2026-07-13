;;;; -- Candidate Selection --

(in-package #:clinedi)

(defclass selector ()
  ((items
    :initarg :items
    :reader selector-items
    :type list
    :documentation "Candidate values in presentation order.")
   (selection
    :initform 0
    :reader selector-selection
    :type integer
    :documentation "Zero-based selected candidate index.")
   (visible-count
    :initarg :visible-count
    :reader selector-visible-count
    :type integer
    :documentation "Maximum number of candidates in the visible window."))
  (:documentation
   "Application-neutral candidate selection and viewport state.

Candidate values remain opaque to Clinedi. Applications own their labels,
descriptions, filtering, styling and accepted-value semantics."))

(defun selector--copy-items (items)
  "Return a fresh list containing the candidate values from ITEMS."
  (unless (typep items 'sequence)
    (error 'type-error :datum items :expected-type 'sequence))
  (loop for item across (coerce items 'vector)
        collect item))

(defun make-selector (&key (items nil) (visible-count 6))
  "Create candidate selection state for ITEMS.

VISIBLE-COUNT must be positive. The sequence container is copied, while its
opaque candidate values are retained. An empty selector has no selected item."
  (unless (and (integerp visible-count) (plusp visible-count))
    (error 'type-error
           :datum visible-count
           :expected-type '(integer 1 *)))
  (make-instance 'selector
                 :items (selector--copy-items items)
                 :visible-count visible-count))

(defun selector-set-items (selector items &key (test #'equal))
  "Replace SELECTOR candidates with ITEMS and return SELECTOR.

Selection returns to the first candidate when the candidate sequence changes
according to TEST. Reinstalling an equal sequence preserves navigation state."
  (check-type selector selector)
  (let ((new-items (selector--copy-items items)))
    (unless (and (= (length new-items) (length (selector-items selector)))
                 (every test new-items (selector-items selector)))
      (setf (slot-value selector 'selection) 0))
    (setf (slot-value selector 'items) new-items))
  selector)

(defun selector-selected-item (selector)
  "Return SELECTOR's selected candidate, or NIL when it is empty."
  (check-type selector selector)
  (let ((items (selector-items selector)))
    (and items
         (nth (min (selector-selection selector)
                   (1- (length items)))
              items))))

(defun selector-move (selector offset)
  "Move SELECTOR by signed OFFSET with wraparound and return SELECTOR."
  (check-type selector selector)
  (check-type offset integer)
  (let ((count (length (selector-items selector))))
    (when (plusp count)
      (setf (slot-value selector 'selection)
            (mod (+ (selector-selection selector) offset) count))))
  selector)

(defun selector-window (selector)
  "Return SELECTOR's visible candidates, start index and local selection.

The first value is a fresh candidate list. The second is its start index in
SELECTOR-ITEMS. The third is the selected index relative to that list, or NIL
when the selector is empty. The window follows the selection toward either
end and never exceeds SELECTOR-VISIBLE-COUNT."
  (check-type selector selector)
  (let* ((items (selector-items selector))
         (count (length items)))
    (if (zerop count)
        (values nil 0 nil)
        (let* ((selection (min (selector-selection selector) (1- count)))
               (visible-count (min (selector-visible-count selector) count))
               (start (min (max 0 (- (1+ selection) visible-count))
                           (- count visible-count))))
          (values (subseq items start (+ start visible-count))
                  start
                  (- selection start))))))

(defun selector-handle-event (selector event)
  "Apply navigation EVENT and return an action and optional selected value.

Up and Down, including Clinedi's history event names, move with wraparound.
Completion and submission accept the selected value. Escape, interruption and
end-of-input cancel. Other events return :UNHANDLED."
  (check-type selector selector)
  (cond
    ((member event '(:up :history-previous))
     (selector-move selector -1)
     (values :changed nil))
    ((member event '(:down :history-next))
     (selector-move selector 1)
     (values :changed nil))
    ((member event '(:complete :submit))
     (if (selector-items selector)
         (values :accept (selector-selected-item selector))
         (values :unhandled nil)))
    ((member event '(:escape :interrupt :end-of-input :stream-end))
     (values :cancel nil))
    (t
     (values :unhandled nil))))
