;;;; -- Package definition --

(defpackage #:clinedi
  (:use #:cl)
  (:export
   ;; Identity
   #:*clinedi-version*

   ;; Unicode text geometry
   #:grapheme-next-boundary
   #:grapheme-previous-boundary
   #:grapheme-boundary-at-or-after
   #:grapheme-cell-width
   #:text-cell-width
   #:text-cell-prefix
   #:text-cell-window
   #:wrap-text
   #:sanitize-text

   ;; ANSI presentation
   #:*presentation-enabled*
   #:ansi-colorize
   #:ansi-reverse-video
   #:ansi-cursor-up
   #:ansi-cursor-column
   #:ansi-cursor-hide
   #:ansi-cursor-show
   #:ansi-clear-below
   #:ansi-clear-line-right
   #:ansi-clear-screen
   #:ansi-strip
   #:ansi-display-width

   ;; Incremental editing
   #:line-editor
   #:make-line-editor
   #:line-editor-create
   #:line-editor-text
   #:line-editor-cursor
   #:line-editor-history
   #:line-editor-history-limit
   #:line-editor-set-text
   #:line-editor-clear
   #:line-editor-add-history
   #:line-editor-handle-event
   #:line-editor-render

   ;; Input, layout and blocking frontend
   #:read-event
   #:screen-position
   #:write-display
   #:render-line
   #:print-candidates
   #:split-prompt
   #:edit-line)
  (:documentation
   "Portable editing state, Unicode terminal geometry and line input."))

(in-package #:clinedi)

(defparameter *clinedi-version* "0.1.0"
  "The version of the loaded Clinedi system.")
