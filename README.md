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

## Blocking frontend

`clinedi:edit-line` owns key decoding and repainting while delegating terminal
raw mode, terminal size, completion, highlighting and suggestions to callbacks.
This keeps terminal policy and application semantics outside the library.

## Tests

Run the regression suite on every supported Lisp:

```sh
./check
```
