(in-package #:clinedi)

;;;; -- Incremental Line Editor --

(defconstant +default-history-limit+ 10000
  "Default number of entries retained by a line editor.")

(defclass line-editor ()
  ((text
    :initarg :text
    :reader line-editor-text
    :type string
    :documentation "Editable text owned by this editor.")
   (cursor
    :initarg :cursor
    :reader line-editor-cursor
    :type integer
    :documentation "Grapheme boundary in TEXT at which editing occurs.")
   (history
    :initarg :history
    :type vector
    :documentation
    "Adjustable vector of entries ordered from oldest to newest.")
   (history-limit
    :initarg :history-limit
    :reader line-editor-history-limit
    :type integer
    :documentation "Maximum number of retained history entries.")
   (history-index
    :initform nil
    :documentation
    "Current HISTORY index, or NIL when the editor is not navigating history.")
   (history-stash
    :initform nil
    :documentation
    "Draft saved when history navigation begins, or NIL otherwise."))
  (:documentation
   "Portable editable-line state driven by semantic input events."))


;;;; -- State Helpers --

(defun line-editor--normalize-cursor (text cursor)
  "Clamp CURSOR to TEXT and return a complete grapheme boundary."
  (check-type text string)
  (check-type cursor integer)
  (grapheme-boundary-at-or-after
   text
   (min (length text) (max 0 cursor))))

(defun line-editor--make-history (history history-limit)
  "Copy the newest HISTORY entries into an adjustable vector."
  (unless (typep history 'sequence)
    (error 'type-error :datum history :expected-type 'sequence))
  (let* ((length (length history))
         (start  (max 0 (- length history-limit)))
         (copy   (make-array (max 1 (- length start))
                             :adjustable t
                             :fill-pointer 0)))
    (loop for index from start below length
          for entry = (elt history index)
          do (unless (stringp entry)
               (error 'type-error :datum entry :expected-type 'string))
             (vector-push-extend (copy-seq entry) copy))
    copy))

(defun line-editor--leave-history (editor)
  "Leave EDITOR's history traversal and discard its saved draft."
  (setf (slot-value editor 'history-index) nil
        (slot-value editor 'history-stash) nil)
  nil)

(defun line-editor--set-state (editor text cursor &key leave-history-p)
  "Replace EDITOR's TEXT and CURSOR, optionally leaving history traversal."
  (check-type editor line-editor)
  (check-type text string)
  (when leave-history-p
    (line-editor--leave-history editor))
  (let ((owned-text (copy-seq text)))
    (setf (slot-value editor 'text) owned-text
          (slot-value editor 'cursor)
          (line-editor--normalize-cursor owned-text cursor)))
  editor)

(defun line-editor--insert (editor inserted-text)
  "Insert INSERTED-TEXT at EDITOR's cursor."
  (check-type inserted-text string)
  (let* ((text        (line-editor-text editor))
         (cursor      (line-editor-cursor editor))
         (new-text    (concatenate 'string
                                   (subseq text 0 cursor)
                                   inserted-text
                                   (subseq text cursor)))
         (new-cursor  (grapheme-boundary-at-or-after
                       new-text
                       (+ cursor (length inserted-text)))))
    (line-editor--set-state editor new-text new-cursor
                            :leave-history-p t))
  nil)

(defun line-editor--delete-backward (editor)
  "Delete the complete grapheme before EDITOR's cursor."
  (let ((text   (line-editor-text editor))
        (cursor (line-editor-cursor editor)))
    (if (zerop cursor)
        (line-editor--leave-history editor)
        (let ((start (grapheme-previous-boundary text cursor)))
          (line-editor--set-state
           editor
           (concatenate 'string
                        (subseq text 0 start)
                        (subseq text cursor))
           start
           :leave-history-p t))))
  nil)

(defun line-editor--delete-forward (editor)
  "Delete the complete grapheme at EDITOR's cursor."
  (let ((text   (line-editor-text editor))
        (cursor (line-editor-cursor editor)))
    (if (>= cursor (length text))
        (line-editor--leave-history editor)
        (let ((end (grapheme-next-boundary text cursor)))
          (line-editor--set-state
           editor
           (concatenate 'string
                        (subseq text 0 cursor)
                        (subseq text end))
           cursor
           :leave-history-p t))))
  nil)

(defun line-editor--whitespace-character-p (character)
  "Return true when CHARACTER is standard line-editor whitespace."
  (case character
    ((#\Space #\Tab #\Newline #\Return #\Page) t)
    (otherwise nil)))

(defun line-editor--word-start (text cursor)
  "Return the grapheme boundary at the start of the word before CURSOR."
  (labels ((previous-boundary ()
             (grapheme-previous-boundary text cursor)))
    (loop while (plusp cursor)
          for previous = (previous-boundary)
          while (line-editor--whitespace-character-p
                 (char text previous))
          do (setf cursor previous))
    (loop while (plusp cursor)
          for previous = (previous-boundary)
          while (not (line-editor--whitespace-character-p
                      (char text previous)))
          do (setf cursor previous))
    cursor))

(defun line-editor--kill-to-end (editor)
  "Delete EDITOR's text after its cursor."
  (line-editor--set-state editor
                          (subseq (line-editor-text editor)
                                  0
                                  (line-editor-cursor editor))
                          (line-editor-cursor editor)
                          :leave-history-p t)
  nil)

(defun line-editor--kill-word (editor)
  "Delete whitespace and the word immediately before EDITOR's cursor."
  (let* ((text   (line-editor-text editor))
         (cursor (line-editor-cursor editor))
         (start  (line-editor--word-start text cursor)))
    (line-editor--set-state
     editor
     (concatenate 'string
                  (subseq text 0 start)
                  (subseq text cursor))
     start
     :leave-history-p t))
  nil)

(defun line-editor--history-previous (editor)
  "Recall the next older history entry in EDITOR."
  (let* ((history (slot-value editor 'history))
         (index   (slot-value editor 'history-index)))
    (when (plusp (length history))
      (cond
        ((null index)
         (setf (slot-value editor 'history-stash)
               (copy-seq (line-editor-text editor))
               index
               (1- (length history))))
        ((plusp index)
         (decf index)))
      (setf (slot-value editor 'history-index) index)
      (line-editor--set-state editor
                              (aref history index)
                              (length (aref history index)))))
  nil)

(defun line-editor--history-next (editor)
  "Recall the next newer history entry or restore EDITOR's saved draft."
  (let* ((history (slot-value editor 'history))
         (index   (slot-value editor 'history-index)))
    (when index
      (if (< index (1- (length history)))
          (let* ((next-index (1+ index))
                 (entry      (aref history next-index)))
            (setf (slot-value editor 'history-index) next-index)
            (line-editor--set-state editor entry (length entry)))
          (let ((draft (or (slot-value editor 'history-stash) "")))
            (line-editor--set-state editor draft (length draft))
            (line-editor--leave-history editor)))))
  nil)


;;;; -- Public Editor Operations --

(defun make-line-editor (&key
                           (text "")
                           (cursor (length text))
                           (history #())
                           (history-limit +default-history-limit+))
  "Create an editor initialized with TEXT, CURSOR, and copied HISTORY.

HISTORY is ordered from oldest to newest.  CURSOR is clamped to TEXT and
advanced when necessary so that it never divides a grapheme."
  (check-type text string)
  (unless (and (integerp history-limit) (plusp history-limit))
    (error 'type-error
           :datum history-limit
           :expected-type '(integer 1 *)))
  (let* ((owned-text (copy-seq text))
         (safe-cursor (line-editor--normalize-cursor owned-text cursor)))
    (make-instance 'line-editor
                   :text owned-text
                   :cursor safe-cursor
                   :history (line-editor--make-history history history-limit)
                   :history-limit history-limit)))

(defun line-editor-create (&key
                             (text "")
                             (cursor (length text))
                             (history #())
                             (history-limit +default-history-limit+))
  "Create a line editor through the application-oriented named constructor."
  (make-line-editor :text text
                    :cursor cursor
                    :history history
                    :history-limit history-limit))

(defun line-editor-history (editor)
  "Return a detached oldest-to-newest snapshot of EDITOR's history."
  (check-type editor line-editor)
  (line-editor--make-history (slot-value editor 'history)
                             (line-editor-history-limit editor)))

(defun line-editor-set-text (editor text &key (cursor (length text)))
  "Replace EDITOR's text, place its cursor, and leave history traversal."
  (line-editor--set-state editor text cursor :leave-history-p t))

(defun line-editor-clear (editor)
  "Clear EDITOR's text and leave history traversal."
  (line-editor--set-state editor "" 0 :leave-history-p t))

(defun line-editor-add-history (editor text)
  "Append non-empty TEXT to EDITOR's history unless it repeats the newest entry."
  (check-type editor line-editor)
  (check-type text string)
  (let ((history (slot-value editor 'history)))
    (when (and (plusp (length text))
               (or (zerop (length history))
                   (not (string= text
                                 (aref history (1- (length history)))))))
      (vector-push-extend (copy-seq text) history)
      (when (> (length history) (line-editor-history-limit editor))
        (replace history history :start2 1)
        (decf (fill-pointer history)))))
  (line-editor--leave-history editor)
  editor)

(defun line-editor-handle-event (editor event)
  "Apply semantic EVENT to EDITOR and return an action and optional payload.

Editing events return (VALUES :CONTINUE NIL).  Submission returns the full
line as its payload and clears the buffer.  UI actions such as completion are
returned to the caller without modifying the buffer. :END-OF-INPUT applies
Ctrl-D semantics. :STREAM-END returns the :END-OF-INPUT action without
clearing buffered text."
  (check-type editor line-editor)
  (labels ((continue-action ()
             (values :continue nil))

           (text-event-p (kind)
             (and (consp event)
                  (eq (first event) kind)
                  (consp (rest event))
                  (null (rest (rest event)))
                  (stringp (second event)))))
    (cond
      ((text-event-p :insert)
       (line-editor--insert editor (second event))
       (continue-action))
      ((text-event-p :paste)
       (line-editor--insert editor (second event))
       (continue-action))
      ((text-event-p :line)
       (line-editor-set-text editor (second event))
       (line-editor-handle-event editor :submit))
      ((eq event :left)
       (when (plusp (line-editor-cursor editor))
         (setf (slot-value editor 'cursor)
               (grapheme-previous-boundary
                (line-editor-text editor)
                (line-editor-cursor editor))))
       (continue-action))
      ((eq event :right)
       (when (< (line-editor-cursor editor)
                (length (line-editor-text editor)))
         (setf (slot-value editor 'cursor)
               (grapheme-next-boundary
                (line-editor-text editor)
                (line-editor-cursor editor))))
       (continue-action))
      ((eq event :home)
       (setf (slot-value editor 'cursor) 0)
       (continue-action))
      ((eq event :end)
       (setf (slot-value editor 'cursor)
             (length (line-editor-text editor)))
       (continue-action))
      ((eq event :backspace)
       (line-editor--delete-backward editor)
       (continue-action))
      ((eq event :delete)
       (line-editor--delete-forward editor)
       (continue-action))
      ((eq event :history-previous)
       (line-editor--history-previous editor)
       (continue-action))
      ((eq event :history-next)
       (line-editor--history-next editor)
       (continue-action))
      ((eq event :kill-to-end)
       (line-editor--kill-to-end editor)
       (continue-action))
      ((eq event :kill-line)
       (line-editor-clear editor)
       (continue-action))
      ((eq event :kill-word)
       (line-editor--kill-word editor)
       (continue-action))
      ((eq event :complete)
       (values :complete nil))
      ((eq event :submit)
       (let ((submitted (copy-seq (line-editor-text editor))))
         (line-editor-add-history editor submitted)
         (line-editor-clear editor)
         (values :submit submitted)))
      ((eq event :interrupt)
       (values :interrupt nil))
      ((eq event :end-of-input)
       (cond
         ((zerop (length (line-editor-text editor)))
          (values :end-of-input nil))
         ((< (line-editor-cursor editor)
             (length (line-editor-text editor)))
          (line-editor--delete-forward editor)
          (continue-action))
         (t
          (continue-action))))
      ((eq event :stream-end)
       (values :end-of-input nil))
      ((eq event :escape)
       (values :escape nil))
      ((eq event :clear-screen)
       (values :clear-screen nil))
      (t
       (values :ignored nil)))))

(defun line-editor-render (editor &key suggestion)
  "Return EDITOR's display text and its cursor's terminal cell offset.

When SUGGESTION is a string and the cursor is at the end, append it as an
unaccepted display suffix.  The suggestion does not affect editor state or
the returned cursor offset."
  (check-type editor line-editor)
  (when suggestion
    (check-type suggestion string))
  (let* ((text       (line-editor-text editor))
         (cursor     (line-editor-cursor editor))
         (display    (if (and suggestion (= cursor (length text)))
                         (concatenate 'string text suggestion)
                         (copy-seq text))))
    (values display (text-cell-width text :end cursor))))
