# Clinedi

Clinedi is a portable Common Lisp line editor for terminal applications. It
separates a Unicode-aware incremental editor from its blocking terminal
frontend, so applications can either feed it semantic events or use it as a
complete interactive input loop.

The editor handles extended grapheme clusters, terminal-cell layout, wrapped
multiline input, history navigation, bracketed paste, completion presentation,
syntax-highlighting callbacks and ghost-text suggestions. Shell parsing,
completion policy and history persistence remain the application's concern.

## Loading

Clinedi is an ASDF system and has no third-party dependencies.

```lisp
(ql:quickload :clinedi)
```

For local Quicklisp development, either place the checkout directly below
`~/quicklisp/local-projects/`, or add its parent directory before registering
local projects:

```lisp
(pushnew #P"/root/common-lisp/"
         ql:*local-project-directories*
         :test #'equal)
(ql:register-local-projects)
```

Quicklisp does not reliably discover a project through a directory symlink in
`local-projects`.

## Incremental editor

```lisp
(let ((editor (clinedi:make-line-editor :history '("git status"))))
  (clinedi:line-editor-handle-event editor '(:insert "echo 猫"))
  (clinedi:line-editor-handle-event editor :left)
  (clinedi:line-editor-text editor))
```

`line-editor-handle-event` accepts semantic editing events and returns an
action plus an optional payload. This API is suitable for event-driven terminal
UIs that own their repaint loop. `:end-of-input` represents Ctrl-D and follows
the usual delete-or-EOF behavior. `:stream-end` represents physical stream EOF;
handling it returns the `:end-of-input` action without clearing partial text.
`:insert-newline` adds an explicit newline without submitting the editor.

## Candidate selection

`clinedi:selector` is application-neutral navigation and viewport state for
pickers and interactive completions. Candidate values are opaque, so an
application can use strings for file completion, model records for a picker,
or any other values while retaining control of filtering, labels, styling and
acceptance policy. A selector can arrange candidates vertically or in a
row-major grid that measures candidate cell widths against the available
terminal width. Arrow keys navigate that geometry, Tab cycles candidates,
Enter accepts, and ordinary editing input dismisses the chooser while returning
the selected value.

```lisp
(let ((selector (clinedi:make-selector
                 :items '("source/" "source/main.lisp")
                 :arrangement :grid)))
  (clinedi:selector-arrange selector 80
                           :width-function #'clinedi:text-cell-width)
  (clinedi:selector-handle-event selector :history-next)
  (clinedi:selector-selected-item selector))
```

## Blocking frontend

`clinedi:edit-line` owns key decoding and repainting while delegating terminal
raw mode, terminal size, completion, highlighting and suggestions to callbacks.
This keeps terminal policy and application semantics outside the library.

Enter submits input. Alt-Enter inserts a newline on terminals that encode Alt
as an Escape prefix. Shift-Enter and Alt-Enter also work when the terminal emits
CSI-u or modifyOtherKeys sequences for modified Enter. Ctrl-Backspace and
Ctrl-W delete the whitespace and word before the cursor; Ctrl-Backspace works
with its raw control byte and its CSI-u or modifyOtherKeys encodings. Ctrl-Left
and Ctrl-Right move across words when the terminal emits the usual modified
arrow sequences.

## Live application region

Event-driven applications can keep editable or transient content below their
ordinary terminal output with `clinedi:live-region`. The region tracks its
physical rows and cursor position across wrapping and explicit newlines. It is
retracted before scrollback output, then repainted beneath that output without
entering an alternate screen or clearing earlier terminal contents. Compound
scrollback updates keep cursor motion hidden until the input cursor is back in
place.

Applications may call `live-region-set-cursor-visible` to keep cursor movement
hidden across repeated updates. Dismissing the region always restores cursor
visibility.

```lisp
(let ((region (clinedi:make-live-region :columns 80 :maximum-rows 20)))
  (clinedi:live-region-present region "> draft" :cursor 7)
  (clinedi:live-region-append region "tool completed\n")
  (clinedi:live-region-resize region 120 :maximum-rows 30)
  (clinedi:live-region-dismiss region))
```

`live-region-present` accepts separate plain geometry text and trusted ANSI
display text when an application owns styling. Their visible contents must be
identical. `live-region-append` always leaves the cursor on a fresh line before
repainting the region, so appended output remains in normal scrollback. An
optional `maximum-rows` keeps long multiline content inside a cursor-following
viewport while retaining the complete presentation for later repainting.

Applications that manage their own presentation can use `clinedi:screen-window`
to obtain grapheme-safe start, end, and cursor indexes for the same bounded
multiline viewport behavior.

## Tests

Run the regression suite on every supported Lisp:

```sh
./check
```
