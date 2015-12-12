;;

;;;### (autoloads nil "e-other-window" "e-other-window.el" (22119
;;;;;;  37869 0 0))
;;; Generated autoloads from e-other-window.el

(autoload 'e-other-window-blink "e-other-window" "\
Blink the currently selected window so that it is obvious what is going on.

\(fn)" t nil)

(autoload 'e-other-window-deactivate "e-other-window" "\
Deactive the current overlay

\(fn)" nil nil)

(autoload 'e-other-window "e-other-window" "\
Go to the other window and blink it.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads nil "mediawiki" "mediawiki.el" (22117 37741 0 0))
;;; Generated autoloads from mediawiki.el

(autoload 'mediawiki-draft "mediawiki" "\
Open a temporary buffer in wikipedia mode for editing an wikipedia
 draft, which an arbitrary piece of data. After finishing the editing
 either use C-c C-k \\[mediawiki-draft-buffer] to send the data into
 the mediawiki-draft-data-file, or send  the buffer using C-x C-s
\\[mediawiki-save]  and insert it later into a wikipedia article.

\(fn)" t nil)

(autoload 'mediawiki-draft-page "mediawiki" "\


\(fn)" t nil)

(autoload 'mediawiki-draft-buffer "mediawiki" "\
Mediawiki-draft-buffer sends the contents of the current (temporary)
buffer to the mediawiki-draft-buffer, see the variable
mediawiki-draft-data-file.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "mmix-mode" "mmix-mode.el" (22054 50227 0 0))
;;; Generated autoloads from mmix-mode.el

(autoload 'mmix-mode "mmix-mode" "\
Major mode for editing MMIX code.
Features a private abbrev table and the following bindings:

\\[mmix-colon]	outdent a preceding label, tab to next tab stop.
\\[tab-to-tab-stop]	tab to next tab stop.
\\[mmix-newline]	newline, then tab to next tab stop.
\\[mmix-comment]	smart placement of assembler comments.

The character used for making comments is set by the variable
`mmix-comment-char' (which defaults to `; *').

Alternatively, you may set this variable in `mms-mode-set-comment-hook',
which is called near the beginning of mode initialization.

Turning on MMIX mode runs the hook `mmix-mode-hook' at the end of initialization.

Special commands:
\\{mmix-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "moccur-wrappers" "moccur-wrappers.el" (22121
;;;;;;  36491 0 0))
;;; Generated autoloads from moccur-wrappers.el

(autoload 'qap-locate-windows "moccur-wrappers" "\


\(fn TERMS)" t nil)

(autoload 'qap-locate-windows-code-like-and-moccur "moccur-wrappers" "\
Does an moccur regexp search among files with names like the
provided term according to Windows search

\(fn TEXTMATCH)" t nil)

(autoload 'qap-locate-windows-code-contains-and-moccur "moccur-wrappers" "\
Does an moccur regexp search among files containign the
provided term according to Windows search

\(fn TEXTMATCH)" t nil)

(autoload 'qap-p4-grep-list-dirs "moccur-wrappers" "\
Show a list of directories that would be searched by
qap-p4-moccur-grep

\(fn)" t nil)

(autoload 'qap-p4-grep-moccur "moccur-wrappers" "\
Perform a p4 grep of the current directory and all
descendants, honouring the qap-p4-dirs-to-split and
qap-p4-dirs-to-exclude lists. You can check the results of the
split and exclude lists by using M-x qap-p4-grep-list-dirs.

\(fn REGEX)" t nil)

(autoload 'qap-p4-grep-count-matches "moccur-wrappers" "\
Count matches for regex

\(fn REGEX)" t nil)

(autoload 'moccur-wrappers-test "moccur-wrappers" "\


\(fn REGEX)" t nil)

;;;***

;;;### (autoloads nil "once-only-header" "once-only-header.el" (22054
;;;;;;  50227 0 0))
;;; Generated autoloads from once-only-header.el

(autoload 'ooh-maybe-insert-cpp-guard "once-only-header" "\
Inserts a CPP once-only-include guard.

For example:

	#ifndef _SYS_TYPES_H_
	#define _SYS_TYPES_H_
	... the file's text ...
	#endif /* _SYS_TYPES_H_ */

This function is normally just added to the C/C++ mode hooks, but it
can also be called interactively.

Since the C and C++ mode hooks are run for other files besides
headers, this function needs to decide when inserting a guard is
appropriate.  It considers it appropriate when:

	a) optional FORCE argument is non-nil (\\[universal-argument]
	   when interactive)
	b) `ooh-user-approve-function' returns non-nil.
	c) the buffer is new (i.e. empty) and is visiting a filename
	   that looks like a header (see `ooh-header-file-regexp'.)
	d) it is called interactively and the buffer is visiting a
	   filename that looks like a header and that file doesn't
	   already have a guard, or the user says OK

Choosing the guard symbol is done interactively by prompting the user
with a reasonable default symbol (e.g., _TYPES_H_).  If a scoped
symbol is desired the user can type M-p when prompted to add the name
of the current directory to the symbol (e.g., _SYS_TYPES_H_).  If M-p
is typed again then the parent directory name is added (e.g.,
_INCLUDE_SYS_TYPES_H_), and so on.  Analogously, M-n removes the first
directory component (if any) from the default symbol.  If the user wishes
to provide an entirely different guard symbol, the minibuffer may be
edited accordingly.  If no CPP guarding is desired, the user can simply
erase the minibuffer contents and hit RET.

The format of the guard symbol and the CPP directives used can be
customized via `ooh-guard-sym-maker' and `ooh-guard-template-maker',
respectively.

Empty buffers are normally left unmodified after the guard is inserted
\(see `ooh-new-file-unmodify').

\(fn &optional FORCE)" t nil)

;;;***

;;;### (autoloads nil "org-helpers" "org-helpers.el" (22119 35493
;;;;;;  0 0))
;;; Generated autoloads from org-helpers.el

(autoload 'org-check-running-clock-any-buffer "org-helpers" "\
Check if any Org buffer contains a running clock.
If yes, offer to stop it and to save the buffer with the changes.

\(fn)" t nil)

(autoload 'org-clock-in-and-goto "org-helpers" "\


\(fn)" t nil)

(autoload 'org-show-contents-or-move-to-previous-table-field "org-helpers" "\
If not at a table, switch to an outline view to the arg'th level of logical headings with `org-content'.
If at a table, move the point to the previous table field with `org-table-previous-field'

\(fn &optional ARG)" t nil)

(autoload 'org-update-mode-line-thoroughly "org-helpers" "\
Force the org clock total time to be recalculated for the current item and 
then update the mode line.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "pretty-column" "pretty-column.el" (22054 50227
;;;;;;  0 0))
;;; Generated autoloads from pretty-column.el

(autoload 'pretty-column "pretty-column" "\
Prettify all columns in a text region.

START and END delimits the text region.

\(fn START END)" t nil)

(autoload 'pretty-rectangle "pretty-column" "\
Prettify all columns in a text rectangle.

START and END delimits the corners of text rectangle.

\(fn START END)" t nil)

;;;***

;;;### (autoloads nil "python-helpers" "python-helpers.el" (22119
;;;;;;  31307 0 0))
;;; Generated autoloads from python-helpers.el

(autoload 'activate-venv-if-python "python-helpers" "\
For a `python-mode' buffer with an associated file, activates
the virtual environment for the file defined by `venv-for'

\(fn)" nil nil)

(autoload 'pyvenv-virtualenv-list-with-second-level "python-helpers" "\
If NOERROR is set, do not raise an error if WORKON_HOME is not
configured.

\(fn &optional NOERROR)" nil nil)

(autoload 'pyvenv-use-venv "python-helpers" "\
Basically my version of pyvenv workon, but taking venvs from
up to two directories down.

\(fn)" t nil)

(autoload 'my-python-shell-get-process-name "python-helpers" "\
If `pyvenv-virtual-env-name' is set then return a buffer name
based on this and `python-shell-buffer-name', otherwise call
`python-shell-get-process-name'

\(fn ORIG-FUN &rest ARGS)" nil nil)

;;;***

;;;### (autoloads nil "scroll-in-place" "scroll-in-place.el" (22119
;;;;;;  32514 0 0))
;;; Generated autoloads from scroll-in-place.el

(autoload 'scroll-down-in-place "scroll-in-place" "\
Scroll the text of the current window downward by LINES lines, leaving point
as close as possible to its current window position (window line and column).
In other words, point is left \"in place\" within the window.  As a special
case, when the current window is a minibuffer window, this command scrolls the
`minibuffer-scroll-window' (which is usually the list of completions) if it
exists, or otherwise the next window in the canonical ordering of windows.

If the optional argument LINES is `nil', scroll the window by the same amount
it was moved by the immediately previous \"in place\" scrolling command, or by
the value of the variable `scroll-default-lines' (usually almost a windowful)
if the previous command was not an \"in place\" scrolling command (or when that
previous command scrolled some other window, or when other circumstances
prevent the previous scrolling distance from being used).  If LINES is the
symbol `-', then the scrolling distance is determined as if LINES had been
`nil' and then that distance is multiplied by -1.

If the window cannot be scrolled by the full distance, point is allowed to
stray from its initial position so that it can move the full number of lines.
If point cannot move the full number of lines, point is moved to the buffer
boundary.  Any immediately subsequent \"in place\" scrolling commands will try
to restore point to its initial window position.

\(fn &optional LINES)" t nil)

(autoload 'scroll-up-in-place "scroll-in-place" "\
Scroll the text of the current window upward by LINES lines, leaving point
as close as possible to its current window position (window line and column).
In other words, point is left \"in place\" within the window.  As a special
case, when the current window is a minibuffer window, this command scrolls the
`minibuffer-scroll-window' (which is usually the list of completions) if it
exists, or otherwise the next window in the canonical ordering of windows.

If the optional argument LINES is `nil', scroll the window by the same amount
it was moved by the immediately previous \"in place\" scrolling command, or by
the value of the variable `scroll-default-lines' (usually almost a windowful)
if the previous command was not an \"in place\" scrolling command (or when that
previous command scrolled some other window, or when other circumstances
prevent the previous scrolling distance from being used).  If LINES is the
symbol `-', then the scrolling distance is determined as if LINES had been
`nil' and then that distance is multiplied by -1.

If the window cannot be scrolled by the full distance, point is allowed to
stray from its initial position so that it can move the full number of lines.
If point cannot move the full number of lines, point is moved to the buffer
boundary.  Any immediately subsequent \"in place\" scrolling commands will try
to restore point to its initial window position.

\(fn &optional LINES)" t nil)

(autoload 'scroll-other-window-down-in-place "scroll-in-place" "\
Scroll the text of the next window downward by LINES lines, leaving point in
that window as close as possible to its current window position (window line
and column).  In other words, point is left \"in place\" within the window.
The next window is generally the one below the current one, or the one at the
top of the screen if the current window is at the bottom of the screen.  In
special circumstances this command will scroll a window other than the next
window.  Read the documentation for the function `scroll-choose-other-window'
for details.

If the optional argument LINES is `nil', scroll the window by the same amount
it was moved by the immediately previous \"in place\" scrolling command, or by
the value of the variable `scroll-default-lines' (usually almost a windowful)
if the previous command was not an \"in place\" scrolling command (or when that
previous command scrolled some other window, or when other circumstances
prevent the previous scrolling distance from being used).  If LINES is the
symbol `-', then the scrolling distance is determined as if LINES had been
`nil' and then that distance is multiplied by -1.

If the window cannot be scrolled by the full distance, point is allowed to
stray from its initial position so that it can move the full number of lines.
If point cannot move the full number of lines, point is moved to the buffer
boundary.  Any immediately subsequent \"in place\" scrolling commands will try
to restore point to its initial window position.

If it is impossible to scroll the text of the window at all (because a buffer
boundary is already visible), this command signals a buffer boundary error.
The error is signalled even if point could otherwise move the full number of
lines.

\(fn &optional LINES)" t nil)

(autoload 'scroll-other-window-in-place "scroll-in-place" "\
Scroll the text of the next window upward by LINES lines, leaving point in
that window as close as possible to its current window position (window line
and column).  In other words, point is left \"in place\" within the window.
The next window is generally the one below the current one, or the one at the
top of the screen if the current window is at the bottom of the screen.  In
special circumstances this command will scroll a window other than the next
window.  Read the documentation for the function `scroll-choose-other-window'
for details.

If the optional argument LINES is `nil', scroll the window by the same amount
it was moved by the immediately previous \"in place\" scrolling command, or by
the value of the variable `scroll-default-lines' (usually almost a windowful)
if the previous command was not an \"in place\" scrolling command (or when that
previous command scrolled some other window, or when other circumstances
prevent the previous scrolling distance from being used).  If LINES is the
symbol `-', then the scrolling distance is determined as if LINES had been
`nil' and then that distance is multiplied by -1.

If the window cannot be scrolled by the full distance, point is allowed to
stray from its initial position so that it can move the full number of lines.
If point cannot move the full number of lines, point is moved to the buffer
boundary.  Any immediately subsequent \"in place\" scrolling commands will try
to restore point to its initial window position.

If it is impossible to scroll the text of the window at all (because a buffer
boundary is already visible), this command signals a buffer boundary error.
The error is signalled even if point could otherwise move the full number of
lines.

\(fn &optional LINES)" t nil)

(autoload 'scroll-down "scroll-in-place" "\
Scroll the text of the current window downward by LINES lines.  As a special
case, when the current window is a minibuffer window, this command scrolls the
`minibuffer-scroll-window' (which is usually the list of completions) if it
exists, or otherwise the next window in the canonical ordering of windows.

The argument LINES is optional.  Its meaning depends on the current value of
the variable `scroll-in-place'.

When the variable `scroll-in-place' is true, this command works just like the
command `scroll-down-in-place', scrolling the current window and leaving point
\"in place\" within the window.  See the documentation for the command
`scroll-down-in-place' for more information.

When the variable `scroll-in-place' is `nil' this command invokes the standard
GNU Emacs version of `scroll-down'.  In that case, when LINES is `nil' the
current window is scrolled by nearly a complete windowful of text.

Note that this command correctly handles cases in which `scroll-in-place' has a
buffer-local value in the window to be scrolled.  That value is honored.

\(fn &optional LINES)" t nil)

(autoload 'scroll-up "scroll-in-place" "\
Scroll the text of the current window upward by LINES lines.  As a special
case, when the current window is a minibuffer window, this command scrolls the
`minibuffer-scroll-window' (which is usually the list of completions) if it
exists, or otherwise the next window in the canonical ordering of windows.

The argument LINES is optional.  Its meaning depends on the current value of
the variable `scroll-in-place'.

When the variable `scroll-in-place' is true, this command works just like the
command `scroll-up-in-place', scrolling the current window and leaving point
\"in place\" within the window.  See the documentation for the command
`scroll-up-in-place' for more information.

When the variable `scroll-in-place' is `nil' this command invokes the standard
GNU Emacs version of `scroll-up'.  In that case, when LINES is `nil' the
current window is scrolled by nearly a complete windowful of text.

Note that this command correctly handles cases in which `scroll-in-place' has a
buffer-local value in the window to be scrolled.  That value is honored.

\(fn &optional LINES)" t nil)

(autoload 'scroll-other-window-down "scroll-in-place" "\
Scroll the text of the next window downward by LINES lines.  The next window
is generally the one below the current one, or the one at the top of the screen
if the current window is at the bottom of the screen.  In special circumstances
this command will scroll a window other than the next window.  Read the
documentation for the function `scroll-choose-other-window' for details.

The argument LINES is optional.  Its meaning depends on the current value of
the variable `scroll-in-place'.

When the variable `scroll-in-place' is true, this command works just like the
command `scroll-other-window-down-in-place', scrolling the next window and
leaving point \"in place\" within that window.  See the documentation for the
command `scroll-other-window-down-in-place' for more information.

When the variable `scroll-in-place' is `nil' this command invokes the standard
GNU Emacs version of `scroll-other-window-down'.  In that case, when LINES is
`nil' the next window is scrolled by nearly a complete windowful of text.
\(Note that `scroll-other-window-down' first appeared as a standard command in
the FSF's GNU Emacs 19.26.  If the builtin version of that command is not
available in the current Emacs system, an equivalent action is invoked
instead.)

Note that this command correctly handles cases in which `scroll-in-place' has a
buffer-local value in the window to be scrolled.  That value is honored.

\(fn &optional LINES)" t nil)

(autoload 'scroll-other-window "scroll-in-place" "\
Scroll the text of the next window upward by LINES lines.  The next window
is generally the one below the current one, or the one at the top of the screen
if the current window is at the bottom of the screen.  In special circumstances
this command will scroll a window other than the next window.  Read the
documentation for the function `scroll-choose-other-window' for details.

The argument LINES is optional.  Its meaning depends on the current value of
the variable `scroll-in-place'.

When the variable `scroll-in-place' is true, this command works just like the
command `scroll-other-window-in-place', scrolling the next window and leaving
point \"in place\" within that window.  See the documentation for the command
`scroll-other-window-in-place' for more information.

When the variable `scroll-in-place' is `nil' this command invokes the standard
GNU Emacs version of `scroll-other-window'.  In that case, when LINES is `nil'
the next window is scrolled by nearly a complete windowful of text.

Note that this command correctly handles cases in which `scroll-in-place' has a
buffer-local value in the window to be scrolled.  That value is honored.

\(fn &optional LINES)" t nil)

(autoload 'scroll-window-in-place "scroll-in-place" "\
Scroll WINDOW vertically by the given number of window LINES in the given
DIRECTION, leaving the window's point as close as possible to its original
window position (window line and column).  In other words, the window's point
is left \"in place\" within the window.

Note that the window to be scrolled does not have to be the selected window,
and that this function does not change which window is selected.

LINES specifies the number of window lines to scroll and is interpreted as if
it were a raw prefix argument.  If LINES is `nil', the window is scrolled by
the amount it was moved by the immediately previous \"in place\" scrolling
command, or by the value of the variable `scroll-default-lines' (by default,
almost a windowful) if the previous command was not an \"in place\" scrolling
command (or when WINDOW is not the previously scrolled window, or when the
value of `this-command' is not in the same group as the previous scrolling
command (see the documentation for the variable `scroll-command-groups'), or
when other circumstances prevent the previous scrolling distance from being
used).  If LINES is the symbol `-', then the scrolling distance is determined
as if LINES had been `nil' and then that distance is multiplied by -1.

DIRECTION determines the direction of the scrolling motion.  The values -1 and
`down' indicate downward motion; the values 1 and `up' indicate upward motion.
Any other value causes an error.

If the window cannot be scrolled by the full distance (because the window hits
the boundary of its buffer), the window's point is allowed to stray from its
initial position so that it can move the full number of lines.  If point cannot
move the full number of lines, point is moved to the buffer boundary (unless it
was already there, in which case a buffer boundary error is signalled instead).
Any immediately subsequent \"in place\" scrolling commands will try to restore
point to its initial window position.

Unless the variable `scroll-allow-blank-lines-past-eob' is true, this function
avoids displaying blank lines past the end of the buffer except as necessary to
make a previous \"in place\" scrolling action reversible.  Effectively, this
means that this function will not display any more past-end-of-buffer blank
lines than were visible when the current sequence of \"in place\" scrolling
commands started.  When the variable `scroll-allow-blank-lines-past-eob' is
true, this function will display as many blank lines as is necessary to keep
point \"in place\" in the window.

Note that if WINDOW is not the selected window and it is impossible to scroll
the text of WINDOW at all (because a buffer boundary is already visible), then
this function signals a buffer boundary error.  The error is signalled even if
point could otherwise move the full number of lines.

\(fn WINDOW LINES DIRECTION)" nil nil)

(autoload 'scroll-window "scroll-in-place" "\
Scroll WINDOW vertically by the given number of window LINES in the given
DIRECTION.  Note that the window to be scrolled does not have to be the
selected window, and that this function does not change which window is
selected.

When the variable `scroll-in-place' is true, this function simply invokes the
function `scroll-window-in-place' to scroll the window and leave point \"in
place\" within that window.  See the documentation for `scroll-window-in-place'
for more information.

When the variable `scroll-in-place' is `nil' this function invokes the original
version of the standard GNU Emacs command `scroll-down' or `scroll-up', as
determined by DIRECTION, to scroll the window.  If DIRECTION is -1 or `down',
the original `scroll-down' is called; if DIRECTION is 1 or `up', the original
`scroll-up' is called.  Any other DIRECTION is an error.  LINES is interpreted
as if it were a raw prefix argument.  If LINES is `nil', the window is scrolled
by almost a complete windowful.  If LINES is the symbol `-', the window is
scrolled by almost a complete windowful in the opposite direction.

Note that this function correctly handles cases in which `scroll-in-place' has
a buffer-local value in the WINDOW's buffer.  That value is honored.

\(fn WINDOW LINES DIRECTION)" nil nil)

(autoload 'scroll-window-in-place-continue-sequence "scroll-in-place" "\
If the previous command was a \"scroll in place\" command, set the variable
`this-command' to the name of that previous command.  This ensures that any
running sequence of \"in place\" scrolling commands will not be broken by the
current command.  See the documentation for the commands `scroll-down-in-place'
and `scroll-up-in-place' for more information about \"in place\" scrolling.

NOTE that you don't need to call this function if the current command scrolls
in place!  You only need to call this function when the current command is not
a \"scroll in place\" command but you still want to preserve any running
sequence of \"in place\" commands.  Such situations are rare.

NOTE that this function sets `this-command' in order to trick the \"in place\"
scrolling commands.  If something else subsequently sets `this-command', any
running sequence of scrolling commands will probably be broken anyway.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "snippets" "snippets.el" (22119 43619 0 0))
;;; Generated autoloads from snippets.el

(autoload 'recenter-top "snippets" "\


\(fn)" t nil)

(autoload 'recenter-bottom "snippets" "\


\(fn)" t nil)

(autoload 'insert-time "snippets" "\
Insert a string describing the current time into the buffer.

\(fn)" t nil)

(autoload 'isearch-joccur "snippets" "\
Invoke `joccur' from within isearch.

\(fn)" t nil)

(autoload 'pretty-print-keymap-recurse "snippets" "\
Engine for pretty-print-keymap

\(fn KMAP PREFIX)" nil nil)

(autoload 'pretty-print-keymap "snippets" "\
Function for doing pretty printing on very simple keymaps.  Not at all general

\(fn KMAP)" nil nil)

(autoload 'fill-and-indent-current-paragraph "snippets" "\
Fill and indent the current paragraph.  This is an alternative
to fill-paragraph when using indent gives a different result from
using fill-paragraph. 

\(fn)" t nil)

(autoload 'delete-blank-lines-around-point-or-in-region "snippets" "\
Delete blank lines in the region.
If the mark is not set or inactive, act like `delete-blank-lines'.

\(fn)" t nil)

(autoload 'move-file-and-buffer "snippets" "\
Moves both current buffer and file it's visiting to DIR.

\(fn DIR)" t nil)

(autoload 'rename-file-and-buffer "snippets" "\
Renames both current buffer and file it's visiting to NEW-NAME.

\(fn NEW-NAME)" t nil)

(autoload 'buffer-file-names-in-selected-frame "snippets" "\
Lists the name of the buffers in the same frame as the current
buffer, as determined by get-buffer-window, searching all frames.

\(fn)" nil nil)

(autoload 'swap-win-contents "snippets" "\
Swap the buffers displayed in windows WIN1 and WIN2.  Does not
affect which window is selected.

\(fn WIN1 WIN2)" t nil)

(autoload 'swap-buffers-previous-window "snippets" "\
Swap the buffers displayed in the selected window and the previous window.

\(fn)" t nil)

(autoload 'swap-buffers-previous-window-and-select "snippets" "\
Swap the buffers displayed in the selected window and the previous window and select the previous window.

\(fn)" t nil)

(autoload 'swap-buffers-next-window "snippets" "\
Swap the buffers displayed in the selected window and the next window.

\(fn)" t nil)

(autoload 'swap-buffers-next-window-and-select "snippets" "\
Swap the buffers displayed in the selected window and the next window and select the next window.

\(fn)" t nil)

(autoload 'rotate-buffer-to-previous-window "snippets" "\
Rotate the buffers displayed in the current frame's windows
maintaining window order so that the current buffer is displayed
in the previous window.

\(fn)" t nil)

(autoload 'rotate-buffer-to-previous-window-and-select "snippets" "\
Call rotate-buffer-to-previous-window, then select the
previous window.

\(fn)" t nil)

(autoload 'rotate-buffer-to-next-window "snippets" "\
Rotate the buffers displayed in the current frame's windows
maintaining window order so that the current buffer is displayed
in the next window.

\(fn)" t nil)

(autoload 'rotate-buffer-to-next-window-and-select "snippets" "\
Call rotate-buffer-to-next-window, then select the next
window.

\(fn)" t nil)

(autoload 'select-next-buffer "snippets" "\
Selects the buffer after the current buffer in the buffer
list, and burys the current buffer. When used with
`select-last-buffer', allows navigating the buffer list as if it were
a ring.  Filters out `cycle-buffer-exclusion-regexp'

\(fn)" t nil)

(autoload 'select-last-buffer "snippets" "\
Selects the buffer at the back of the buffer list. When used
with `select-next-buffer', allows navigating the buffer list as if it
were a ring.  Filters out `cycle-buffer-exclusion-regexp'

\(fn)" t nil)

(autoload 'kill-buffer-other-window "snippets" "\
Kill the buffer in the other window, and make the current buffer full size. If no other window, kills current buffer.

\(fn ARG)" t nil)

(autoload 'delete-unselected-frames "snippets" "\
Delete unselected frames. Useful as emacs may think they are invisible 
even when they are not

\(fn)" nil nil)

(autoload 'cycle-frame-maximized "snippets" "\
Cycle current frame state through maximized and normal.

\(fn)" t nil)

(autoload 'dired-do-ps-print "snippets" "\
Print the marked (or next ARG) files with ps-print.el.

If `dired-ps-print-buffer-with-faces' is non-nil, use
`ps-print-buffer-with-faces; otherwise, use `ps-print-buffer'.

\(fn &optional ARG)" t nil)

(autoload 'dired-execute-file "snippets" "\


\(fn &optional ARG)" t nil)

(autoload 'shell-in-default-directory "snippets" "\
Run an inferior shell in the current directory.
  set name to that of this directory.
  If buffer exists but shell process is not running, make new shell.
  (Type \\[describe-mode] in the shell buffer for a list of commands.)

\(fn)" t nil)

(autoload 'c++-convert-to-method-body "snippets" "\
Take a function prototype from the class definition and convert it
to the implementation body

\(fn)" t nil)

(autoload 'query-replace-in-open-core "snippets" "\


\(fn ARG1 ARG2 F)" nil nil)

(autoload 'query-replace-in-open-buffers "snippets" "\
query-replace in open buffers

\(fn ARG1 ARG2)" t nil)

(autoload 'query-replace-regexp-in-open-buffers "snippets" "\
query-replace-regexp in open buffers

\(fn ARG1 ARG2)" t nil)

(autoload 'uniquify-region "snippets" "\
remove duplicate adjacent lines in the given region

\(fn)" t nil)

(autoload 'uniquify-buffer "snippets" "\
remove duplicate adjacent lines in the current buffer

\(fn)" t nil)

(autoload 'assoc-regexp-exact "snippets" "\
Return non-nil if KEY is `equal' to the car of an element of LIST.
The value is actually the first element of list whose car equals key.

\(fn KEY LIST)" nil nil)

(autoload 'char-syntax-to-string "snippets" "\
Return the syntax class of CHARACTER as a string.  See also
`describe-syntax'

\(fn CHARACTER)" nil nil)

(autoload 'msg "snippets" "\


\(fn Y X)" nil nil)

(autoload 'fake-stdin-slurp "snippets" "\
Emulate stdin slurp using emacsclient hack

\(fn FILENAME)" nil nil)

;;;***

;;;### (autoloads nil "update-personal-autoloads" "update-personal-autoloads.el"
;;;;;;  (22119 27501 0 0))
;;; Generated autoloads from update-personal-autoloads.el

(autoload 'update-personal-autoloads "update-personal-autoloads" "\
Update personal autoloads for FILE. If FILE is nil, update
autoloads for all files in the directory in which this function
is defined.  The autoloads are saved to this directory.

\(fn &optional FILE FORCE)" t nil)

;;;***

;;;### (autoloads nil "wikipedia-mode" "wikipedia-mode.el" (22054
;;;;;;  50227 0 0))
;;; Generated autoloads from wikipedia-mode.el

(autoload 'wikipedia-mode "wikipedia-mode" "\
Major mode for editing articles written in the markup language used by
Wikipedia, the free on-line encyclopedia (http://www.wikipedia.org).

There are several ways to use wikipedia-mode. One is to copy articles
between Emacs and your web browser's text box. However for GNU emacs,
that does not work always smoothly, since copying marked regions into
other X applications is somehow buggy for GNU emacs. Another way is to
use MozEx, a Mozilla/Firefox web browser extension that allows you to
call Emacs from a text box (http://mozex.mozdev.org/). Another way is
to use the PERL script ee-helper, which allows you to up and download
wiki texts.

Wikipedia articles are usually unfilled: newline characters are not
used for breaking paragraphs into lines. Unfortunately, Emacs does not
handle word wrapping yet. As a workaround, wikipedia-mode turns on
longlines-mode automatically. In case something goes wrong, the
following commands may come in handy:

\\[wikipedia-fill-article] fills the buffer.
\\[wikipedia-unfill-article] unfills the buffer.
Be warned that function can be dead  slow, better use wikipedia-unfill-paragraph-or-region.
\\[wikipedia-unfill-paragraph-or-region] unfills the paragraph
\\[wikipedia-unfill-paragraph-simple] doehe same but simpler.



The following commands put in markup structures.

\\[wikipedia-insert-strong-emphasis] inserts italics
\\[wikipedia-insert-bold] inserts bold text
\\[wikipedia-insert-italics] italics
\\[wikipedia-insert-header] header
\\[wikipedia-insert-link] inserts a link

The following commands are also defined:
\\[wikipedia-insert-user] inserts user name
\\[wikipedia-insert-signature] inserts ~~~~
\\[wikipedia-insert-enumerate] inserts enumerate type structures
\\[wikipedia-insert-itemize] inserts itemize type structures
\\[wikipedia-insert-hline] inserts a hline

The draft functionality
\\[wikipedia-draft]
\\[wikipedia-draft-region]
\\[wikipedia-draft-view-draft]
\\[wikipedia-draft-page]
\\[wikipedia-draft-buffer]

Replying and sending functionality
\\[wikipedia-reply-at-point-simple]
\\[wikipedia-draft-reply]
\\[wikipedia-send-draft-to-mozex]


The register functionality
\\[wikipedia-copy-page-to-register]
\\[defun wikipedia-insert-page-to-register]


Some simple editing commands.
\\[wikipedia-enhance-indent]
\\[wikipedia-yank-prefix]
\\[wikipedia-unfill-paragraph-or-region]



\\[wikipedia-terminate-paragraph]     starts a new list item or paragraph in a context-aware manner.
\\[wikipedia-next-header]     moves to the next (sub)section header.
\\[wikipedia-prev-header]     moves to the previous (sub)section header.

\(fn)" t nil)

(autoload 'wikipedia-draft "wikipedia-mode" "\
Open a temporary buffer in wikipedia mode for editing an wikipedia
 draft, which an arbitrary piece of data. After finishing the editing
 either use C-c C-k \\[wikipedia-draft-buffer] to send the data into
 the wikipedia-draft-data-file, or send  the buffer using C-c C-c
\\[wikipedia-draft-send-to-mozex]  and insert it later into a wikipedia article.

\(fn)" t nil)

(autoload 'wikipedia-draft-page "wikipedia-mode" "\


\(fn)" t nil)

(autoload 'wikipedia-draft-buffer "wikipedia-mode" "\
Wikipedia-draft-buffer sends the contents of the current (temporary)
buffer to the wikipedia-draft-buffer, see the variable
wikipedia-draft-data-file.

\(fn)" t nil)

(defvar wikipedia-draft-send-archive t "\
*Archive the reply.")

;;;***

;;;### (autoloads nil "word-perfect-theme" "word-perfect-theme.el"
;;;;;;  (22117 37741 0 0))
;;; Generated autoloads from word-perfect-theme.el

(when load-file-name (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

;;;***

;;;### (autoloads nil nil ("dired-column-widths.el" "ediff-trees.el"
;;;;;;  "moccur-edit.el" "nexus-extensions.el" "org-planning.el"
;;;;;;  "org-wp-link.el" "sumatra-forward.el" "tempo-c-cpp.el") (22123
;;;;;;  34602 944000 0))

;;;***

;;;### (autoloads nil "highlight-sexps" "highlight-sexps.el" (22117
;;;;;;  37741 0 0))
;;; Generated autoloads from highlight-sexps.el

(autoload 'highlight-sexps-mode "highlight-sexps" "\
Minor mode to highlight an expanding set of surrounding s-expressions.

\(fn &optional ARG)" t nil)

;;;***
