
;;; completion-ui.el --- in-buffer completion user interface


;; Copyright (C) 2006-2009 Toby Cubitt

;; Author: Toby Cubitt <toby-predictive@dr-qubit.org>
;; Version: 0.10.2
;; Keywords: completion, ui, user interface
;; URL: http://www.dr-qubit.org/emacs.php


;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.


;;; Commentary:
;;
;; This package is primarily intended for Elisp package writers. It is NOT
;; intended for Emacs users. Emacs users with no Elisp knowledge who want to
;; try out the completion-UI features are referred instead to the accompanying
;; "completion-ui-examples.el" package. You have been warned!
;;
;; This package provides a user-interface for in-buffer text completion. It
;; doesn't find completions itself. Instead, a completion package can simply
;; set the `completion-function' variable to a function that takes two
;; arguments, a string PREFIX and an integer MAXNUM, and returns a list of at
;; most MAXNUM completion candidates for PREFIX. Completion-UI does the rest.
;;
;; That's it! Completion-UI, the `auto-completion-mode' minor mode, and user
;; customizations take care of the rest. (Avoid the temptation to set
;; completion-UI customization variables from Elisp code to alter its
;; behaviour. The user knows what they want better than you do!)
;;
;; Examples are available in the accompanying "completion-ui-examples.el"
;; package, and on the Emacs wiki, at:
;; www.emacswiki.org/cgi-bin/wiki/CompletionUI
;;
;; Why use completion-UI? Typically, a lot of code in packages providing some
;; kind of text completion deals with the user interface. This is a big waste
;; of effort, since each package reinvents the wheel by implementing its own
;; subset of in-buffer completion user-interfaces. The ultimate goal of
;; completion-UI is that all packages providing in-buffer (and possibly one
;; day also mini-buffer) completion should use this package to provide a
;; common user interface, freeing them to concentrate on the more interesting
;; task of finding the completions in the first place. The Elisp programmer
;; benfits by not having to reinvent the wheel, and the Emacs user benefits by
;; having a standard yet highly customizable user-interface that they can
;; customize once and for all to to suit their preferences, for all completion
;; mechanisms they want to use.
;;
;; Various completion user-interfaces are provided, all of which can be
;; individually enabled, disabled and extensively tweaked via
;; customization variables:
;;
;; * Dynamic completion: provisionally insert the first available
;;   completion candidate into the buffer.
;;
;; * Completion hotkeys: single-key selection of a completion
;;   candidate.
;;
;; * Cycling: cycle through completion candidates.
;;
;; * Tab-completion: "traditional" expansion to longest common
;;   substring.
;;
;; * Help-echo: display a list of completion candidates in the
;;   echo-area.
;;
;; * Tooltip: display a list of completion candidates in a tool-tip
;;   located below the point, from which completions can be selected.
;;
;; * Pop-up frame: display a list of completion candidates in a pop-up
;;   frame located below the point, which can be toggled between
;;   displaying some or all completions, and from which completions can
;;   be selected.
;;
;; * Completion menu: allow completion candidates to be selected from
;;   a drop-down menu located below the point.
;;
;; * Completion browser: browse through all possible completion
;;   candidates in a hierarchical deck-of-cards menu located below the
;;   point.
;;
;; Completion-UI also provides a new minor mode, called
;; `auto-completion-mode'. When enabled, Emacs will automatically complete
;; words as they are typed, using the `completion-function' to find completion
;; candidates. The same customization variables determine how those candidates
;; are displayed and can be selected. This works particularly well with
;; dynamic completion (see above), as long as `completion-function' is fast.
;;
;; This package will work alongside the auto-overlays package if it's
;; available, but does not require it.



;;; Change Log:
;;
;; Version 0.10.2
;; * bug-fixes to `completion-replaces-prefix' support (thanks once again to
;;   Henry Weller for reporting them)
;; * added work-around to enable tooltip keybindings when tooltip is
;;   auto-displayed
;;
;; Version 0.10.1
;; * bug-fixes to `complete-dynamic' relating to prefixes whose size has
;;   changed
;; * refactored recurring code into `completion-highlight-common-substring'
;;   and `completion-hightlight-prefix-alterations' functions
;; * always bind `completion-self-insert' in `completion-map', so that old
;;   completions get resolved
;;
;; Version 0.10
;; * removed `completion-includes-prefix' flag; completion functions now have
;;   to return entire completion, including prefix
;; * prefix is now deleted along with provisional completion when a completion
;;   accepted, and the entire completion is inserted; this allows completions
;;   to modify the prefix when they're accepted
;; * completions can now include data about length of prefix, in case it is
;;   not the same length as the original
;; * `complete-in-buffer' and `complete-word-at-point' can now take arguments
;;   that override the global values for `completion-function',
;;   `completion-prefix-function' and `completion-replaces-prefix'
;; * `completion-construct-menu', `completion-construct-browser-menu' and the
;;   other browser functions now take completion-function, prefix-function and
;;   completion-replaces-prefix arguments
;; * null values for `completion-prefix-function', `completion-menu',
;;   `completion-browser-menu-function', `completion-tooltip-function' and
;;   `completion-popup-frame-function' now mean use the default
;; * only position popup frame in `completion-popup-frame' *after* setting
;;   it's size, otherwise some window managers won't position it where we want
;;
;; Version 0.9.4
;; * modified `completion-run-if-condition' and `completion-select' to get key
;;   sequence used to invoke it via `unread-command-keys' and
;;   `read-key-sequence', to ensure key sequence translation takes place
;; * added new 'pop-up setting for `completion-use-hotkeys' which only enables
;;   hotkeys when a tooltip or pop-up frame is active (thanks to Henry Weller
;;   for the suggestion)
;; * attempted to fix bug preventing default `completion-tooltip-face' being
;;   set correctly
;; * fixed bugs in `completion-browser-sub-menu' and
;;   `completion-browser-menu-iterm'
;; * made most anonymous lambda bindings into names functions (lambdas just
;;   confuse people, and make it hard to bind other keys to the same thing)
;; * bug-fix to `completion-select' which triggered infinite recursion trap
;;
;; Version 0.9.3
;; * added 'accept-common option to `completion-resolve-behaviour'
;;   (thanks to Henry Weller for the patch)
;; * other code refactorings (thanks to Henry Weller again)
;;
;; Version 0.9.2
;; * define hotkey bindings on the fly in `completion-setup-overlay', getting
;;   rid of `completion-hotkey-map' entirely
;; * `completion-hotkey-list' can revert to being a customization option
;; * remove `completion-cancel-tooltip' from `after-change-functions', since
;;   this prevents tooltip being displayed when `flyspell-mode' is enabled
;;   (it's in `before-command-hook' anyway, which should be enough)
;; * use `run-with-timer' instead of `run-with-idle-timer' in
;;   `completion-auto-show', as it seems to make more sense
;; * move backspace and delete bindings to `completion-dynamic-map' and
;;   `auto-completion-map'
;; * added `completion-browser-recurse-on-completions' variable to control
;;   whether the browser lists completions of completions (of completions
;;   of...)
;; * replace `x-popup-menu' with newer `popup-menu' in `completion-show-menu'
;;
;; Version 0.9.1
;; * use :family attribute of `completion-tooltip-face' to set tooltip font
;;   (thanks to Andy Stewart for the patch)
;;
;; Version 0.9
;; * added `completion-includes-prefix' variable to indicate that completions
;;   returned by `completion-function' include the prefix
;; * added `completion-replaces-prefix' variable to indicate that completions
;;   should replace the prefix when they're accepted, and made changes to
;;   almost all of the user-interface functions to cope with this non-prefix
;;   completion (thanks to Henry Weller for helpful discussions about this)
;; * move point to appropriate position in parent frame when cycling through
;;   completions in pop-up frame
;; * added `completion-show-browser-menu' command
;;
;; Version 0.8.2
;; * prevented `completion-show-tooltip' from moving mouse unless absolutely
;;   necessary (thanks to Martin Pohlack for reporting this)
;; * modified `completion-show-tooltip' to ensure tooltip is cancelled before
;;   (re)displaying it, otherwise `x-show-tip' "magically" moves it to the top
;;   of the frame! (thanks to Martin Pohlack for reporting this)
;; * added option to highlight longest common prefix in a dynamic completion
;;   (thanks to Vagn Johansen for the suggestion)
;; * actually make use of `completion-popup-frame-function' variable!
;; * fixed bug in some calls to `completion-reject-functions' that only passed
;;   two arguments instead of three (thanks to Vagn Johansen for reporting
;;   this)
;;
;; Version 0.8,1
;; * fix `completion-define-word-syntax-binding' so it creates key binding in
;;   `auto-completion-dynamic-map' as it should
;; * fix `completion-setup-overlay' to assign correct keymap to 'keymap
;;   property, depending on whether `auto-completion-mode' is enabled or not
;;
;; Version 0.8
;; * give completion overlay a non-nil end-advance property, because...
;; * ...keymap property works for zero-length overlays with non-nil
;;   end-advance since Emacs 22! So disable work-around from that versions on.
;;
;; Version 0.7.5
;; * added `completion-simulate-overlay-bindings' function that can
;;   automatically create key bindings to simulate overlay keymap bindings
;;   using the `completion-run-if-within-overlay' hack
;;
;; Version 0.7.4
;; * split `completion-self-insert' into two: one function, the new
;;   `completion-self-insert', deals with completion-related stuff if
;;   auto-completion-mode is disabled, the other,
;;   `auto-completion-self-insert', does auto-completion as
;;   before. This allows individual printable characters to invoke
;;   auto-completion without auto-completion-mode being enabled.
;; * `auto-completion-self-insert' is now only directly bound to
;;   printable characters in auto-completion-map
;; * overlay keymap now binds printable characters to the new
;;   `completion-self-insert' (which hands off to
;;   `auto-completion-self-insert' if auto-completion is enabled)
;;
;; Version 0.7.3
;; * fixed bug in `completion-popup-frame-toggle-show-all'
;; * fixed bug in definition of `completion-tooltip-face'
;;
;; Version 0.7.2
;; * prevent `complete-in-buffer' from auto-displaying the tooltip/menu/pop-up
;;   frame if there are no completions (otherwise Emacs CVS seems to crash!)
;; * bug fixes to key bindings and `completion-tab-complete'
;; * bug fixes to pop-up frames
;;
;; Version 0.7.1
;; * minor key binding fixes
;; * `complete-in-buffer' can now take an optional prefix argument to override
;;   automatically determined prefix
;; * `completion-self-insert' can now take an optional argument that causes
;;   `auto-completion-syntax-override-alist' to be ignored, as can
;;   `completion-define-word-constituent-binding'
;; * bug fixes to `completion-self-insert'
;; * switched ordering of `auto-completion[-override]-syntax-alist' entries
;;   back to something closer to old ordering
;;
;; Version 0.7
;; * modified core `complete-in-buffer', `complete-word-at-point',
;;   `completion-self-insert' and `completion-backward-delete' functions
;;   to allow `completion-prefix-function' to properly take over prefix
;;   finding
;; * created default `completion-prefix' function
;; * modified same core functions so that completion behaviour is more
;;   intelligent, especially when a character is inserted within a
;;   completion overlay
;; * `completion-overwrite' option now controls whether completions
;;   over-write the remainder of the word at the point or not
;; * renamed `completion-dynamic-*syntax-alist' to
;;   `auto-completion-*syntax-alist' and modified their format somewhat;
;;   behaviour now accessed through interface macros
;; * added new pop-up frame completion method (thanks to anon. on the
;;   Emacs wiki for the suggestion)
;; * auto-show can now display one out of the tooltip, completion menu,
;;   or pop-up frame
;; * `completion-tooltip-delay' and `completion-auto-show-menu' options
;;   subsumed into `completion-auto-show' and
;;   `completion-auto-show-delay'
;; * RET binding now respects customization options
;;
;; Version 0.6.5
;; * bug-fixes to interactive definitions
;; * moved modification hook setting to end of file
;;
;; Version 0.6.4
;; * defined properties to make delete-selection-mode work correctly
;;   (thanks to Sivaram for drawing my attention to this)
;; * minor improvement to text displayed in completion browser bucket
;;   menu entries
;;
;; Version 0.6.3
;; * fixed M-<space> bindings so that prefix argument is passed to
;;   `completion-reject', and fixed C-<space> bindings
;;
;; Version 0.6.2
;; * modified the default `completion-dynamic-syntax-alist' to make
;;   parentheses behave like punctuation
;; * minor bug-fix to `completion-show-menu-if-within-overlay'
;; * fixed `completion-self-insert' again so that it works if called with
;;   an explicit char (auto-fill will not work in that case)
;; * fixed `complete-dynamic' so that the completion overlay ends up in
;;   the right place even when modification hooks cause text to be
;;   inserted in the buffer during its execution
;;
;; Version 0.6.1
;; * modified define-minor-mode usage for auto-completion-mode to work in
;;   older Emacs versions
;; * fixed `completion-self-insert' so that auto-fill works again
;; * if command remapping isn't supported, attempt to simulate it more
;;   effectively for deletion commands
;;
;; Version 0.6
;; * added `completion-prefix' and `completion-tooltip' variables to
;;   allow overriding of default methods for determining prefix at point
;;   and constructing tooltip text
;; * fixed bugs related to backwards-deletion (thanks to Maciej
;;   Katafiasz for pointing some of these out)
;; * added optional arguements to `completion-self-insert' to allow
;;   automatically determined character and syntax to be overridden, and
;;   created key bindings to insert characters as word constituents
;; * modified `completion-backward-delete', created corresponding
;;   `completion-delete' function, and defined a whole host of deletion
;;   and kill commands that are substituted for the standard ones
;; * added convenience function
;;   `completion-define-word-constituent-binding' for defining bindings
;;   to insert characters as word-constituents
;;
;;
;; Version 0.5.2
;; * fixed tooltip face issues, which included defining a new
;;   `completion-tooltip-face'
;; * implemented better method of positioning tooltip, avoiding moving
;;   the mouse (thanks to Nikolaj Schumacher for this!)
;;
;; Version 0.5.1
;; * fixed small bug in `completion-self-insert' (thanks to Nikolaj
;;   Schumacher for pointing it out)
;;
;; Version 0.5
;; Modifications arising from discussions with rms:
;; * removed `completion-define-minor-mode' macro; to use completion-UI,
;;   `completion-function' should just be set appropriately
;; * auto-completion is now a separate minor mode
;; * renamed various variables and functions
;;
;; Version 0.4.1
;; * small but important bug-fix to `completion-accept'
;;
;; Version 0.4
;; * accept and reject hooks now called with two or three arguments
;;   instead of one: the prefix, the full word (this is what was passed
;;   previously) and possibly the interactive prefix argument.
;; * moved some anonymous commands into named functions to sanitize
;;   key-bindings
;;
;; Version 0.3.13
;; * Tried to work around annoying `completion-select' bug
;;
;; Version 0.3.12
;; * added `completion-backward-delete-delay' customization option
;;
;; Version 0.3.11
;; * finally figured out how to prevent list of completions displayed in
;;   echo area from being logged
;;
;; Version 0.3.10
;; * fixed start-of-word behaviour in `completion-self-insert'
;;
;; Version 0.3.9
;; * `completion-select' now uses the `completion-trap-recursion'
;;   variable, instead of testing if 'trap-recursion is bound
;;
;; Version 0.3.8
;; * fixed `completion-run-if-within-overlay' so it doesn't error if
;;   there's no "normal" binding for the key sequence used to invoke it
;; * defined a new `completion-trap-recursion' variable in case the
;;   symbol trap-recursion is bound outside
;;   `completion-run-if-within-overlay'
;;
;; Version 0.3.7
;; * fixed M-<space> binding so it's only active within an overlay
;;
;; Version 0.3.6
;; * fixed bug in `completion-define-minor-mode'
;;
;; Version 0.3.5
;; * added eval-when-compile to prevent bogus compilation errors
;;
;; Version 0.3.4
;; * added function to `after-change-functions' to hide tooltip
;; * made self-insert behaviour alists more flexible
;; * minor fix to `completion-cycle' to leave point at end of word if
;;   dynamic completion is disabled
;; * `completion-hotkey-list' no longer a customization option, since it
;;   must be set *before* completion-ui.el is loaded
;;
;; Version 0.3.3
;; * minor bug-fix to `completion-self-insert'
;; * removed cl dependency
;;
;; Version 0.3.2
;; * bug fixes
;; * incorporated compatability code
;;
;; Version 0.3.1
;; * bug fixes
;;
;; Version 0.3
;; * incorporated a lot of code from predictive.el
;; * rewrote things so that all a package needs to do is set
;;   the `completion-function' variable
;; * `completon-overlay-at-point' is kludgy no more
;;
;; Version 0.2.1
;; * added commentary
;; * prevented any attempt to display tooltips and menus when not
;;   running X
;;
;; Version 0.2
;; * bug fixes (thanks to Mark Zonzon for patch)
;; * added `completion-min-chars' and `completion-delay' options
;;   (thanks to Jin Tong for suggestions)
;; * renamed to `completion-ui.el'
;;
;; Version 0.1
;; * initial release


;;; Code:


(eval-when-compile (require 'cl))
(require 'auto-overlay-common nil t)




;;; ============================================================
;;;                    Customization variables

(defgroup completion-ui nil
  "Completion user interface."
  :group 'convenience)


(defcustom completion-max-candidates 10
  "*Maximum number of completion candidates to offer."
  :group 'completion-ui
  :type 'integer)


(defcustom completion-resolve-behaviour 'accept
  "*What to do with unfinished completions elsewhere in the buffer:

  'leave:         leave the old completions pending
  'accept:        automatically accept the old completions
  'accept-common: automatically accept the common part of the completions
  'reject:        automatically reject the old completions
  'ask:           ask what to do with the old completions"
  :group 'completion-ui
  :type '(choice (const :tag "leave" leave)
                 (const :tag "accept" accept)
                 (const :tag "accept-common" accept-common)
                 (const :tag "reject" reject)
                 (const :tag "ask" ask)))


(defcustom completion-overwrite t
  "*When non-nil, completing in the middle of a word over-writes
the rest of the word. `completion-word-thing' determines what is
considered a word."
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-auto-show 'tooltip
  "*Display completion tooltip/menu/pop-up frame automatically.
When null, nothing is auto-displayed. When set to 'tooltip, 'menu
or 'pop-up, the corresponding completion interface is displayed
automatically, after a delay of `completion-auto-show-delay' if
one is set."
  :group 'completion-ui
  :type '(choice (const :tag "none" nil)
                 (const tooltip)
                 (const menu)
                 (const :tag "pop-up frame" pop-up)))


(defcustom completion-auto-show-delay 3
  "*Number of seconds to wait after completion is invoked
before auto-displaying tooltip/menu/pop-up frame when
`completion-auto-show' is enabled."
  :group 'completion-ui
  :type '(choice (const :tag "Off" nil)
                 (float :tag "On")))



;;; ===== Auto-completion customizations =====

(defcustom auto-completion-min-chars nil
  "*Minimum number of characters before completions are offered."
  :group 'completion-ui
  :type '(choice (const :tag "Off" nil)
                 (integer :tag "On")))


(defcustom auto-completion-delay nil
  "*Number of seconds to wait before activating completion mechanisms
in auto-completion mode."
  :group 'completion-ui
  :type '(choice (const :tag "Off" nil)
                 (float :tag "On")))


(defcustom auto-completion-backward-delete-delay 0.1
  "*Number of seconds to wait before activating completion mechanisms
after deleting backwards in auto-completion mode."
  :group 'completion-ui
  :type 'float)


(defcustom auto-completion-syntax-alist '(reject . word)
  "*Associates character syntax with completion behaviour.
Used by the `auto-completion-self-insert' function to decide what
to do based on a typed character's syntax.

When customizing this variable, predefined choices can be used to
configure two separate syntax-dependent completion behaviours:
how provisional completions are accepted, and how the prefix is
chosen when characters are typed. The first choice is between
\"type normally\" or \"punctuation accepts\", and controls how
completions are accepted. The second is between \"word\" or
\"string\", and controls how the prefix is chosen.

If \"type normally\" is selected, the provisional completions
that appear as you type are only accepted if you call
`completion-accept' manually. You are free to ignore them
entirely and type normally. If \"punctuation accepts\" is
selected, the provisional completions are automatically accepted
whenever you type any punctuation or whitespace character (as
defined by the buffers' syntax table). For example, hitting SPC
will usually accept the current provisional completion and insert
a space after it. Once your fingers get used to it, this can
allow you to type faster as you can quickly accept a completion
and move onto the next word. However, you can no longer entirely
ignore the completions and type normally, since you may
accidentally accept a completion you didn't want.

If \"word\" is selected, typing a word-constituent character (as
defined by a buffer's syntax table) will cause the part of the
word before point to be completed. That is,
`auto-completion-mode' will first find the word at or next to the
point, and then complete that part of it that comes before the
point (`completion-word-thing' determines which characters form
part of a word). If \"string\" is selected, typing a
word-constituent character will complete the string that has been
built up by typing consecutive characters. That is, the prefix
will consist of all the characters you've typed in the buffer
since the last time you did something other than typing a
word-constituent character. Although they sound quite different,
the two behaviours usually only differ if you move the point to
the middle or end of an existing word and then start typing.


Customizing the behaviour for each syntax individually gives more
fine-grained control over the syntax-dependent completion
behaviour. In this case, the value of
`auto-completion-syntax-alist' must be an alist associating
syntax descriptors (characters) with behaviours (two-element
lists).

The first element of the behaviour list must be one of symbols
'accept, 'reject or 'add. The first two have the same meaning as
the predefined behaviours \"punctuation accepts\" and \"type
normally\", though they now apply only to one syntax
descriptor. 'add causes characters with that syntax to be added
to the current completion prefix (this is the usual setting for
word-constutuent characters).

The second element of the list must be one of the symbols 'word,
'string or 'none. 'word and 'string have the same meaning as the
predefined behaviours, though they now apply only to one syntax
descriptor, whereas 'none prevents characters with that syntax
from invoking auto-completion.


When `auto-completion-syntax-alist' is set from Lisp code, in
addition to the symbol values described above the behaviour list
entries can also be functions which return one of those
symbols. The list can also have an additional third element,
which determines whether or not the typed character is inserted
into the buffer: the character is inserted if it is non-nil, not
if it is nil. (Note that, perhaps confusingly, a non-existent
third element is equivalent to setting the third element to
t). If the third element of the list is a function, its return
value again determines the insertion behaviour. This allows a
function to take-over the job of inserting characters (e.g. in
order to make sure parentheses are inserted in pairs), and is
probably the only time it makes sense to use a null third
element."
  :group 'completion-ui
  :type '(choice
          (cons :tag "Predefined"
                (choice :tag "Acceptance behaviour"
                        (const :tag "type normally" reject)
                        (const :tag "punctuation accepts" accept))
                (choice :tag "Completion behaviour"
                        (const word)
                        (const string)))
          (alist :tag "Custom"
                 :key-type character
                 :value-type (list
                              (choice (const accept)
                                      (const reject)
                                      (const add))
                              (choice (const word)
                                      (const string)
                                      (const none))))))


(defcustom auto-completion-override-syntax-alist
  '((?0 . (reject none))
    (?1 . (reject none))
    (?2 . (reject none))
    (?3 . (reject none))
    (?4 . (reject none))
    (?5 . (reject none))
    (?6 . (reject none))
    (?7 . (reject none))
    (?8 . (reject none))
    (?9 . (reject none))
    (?' . (add word)))
  "*Alist associating characters with completion behaviour.
Overrides the default behaviour defined by the character's syntax
in `auto-completion-syntax-alist'. The format is the same as for
`completion-dynamic-synax-alist', except that the alist keys are
characters rather than syntax descriptors."
  :group 'completion-ui
  :type '(alist :key-type (choice character (const :tag "default" t))
                :value-type (list (choice (const :tag "accept" accept)
                                          (const :tag "reject" reject)
                                          (const :tag "add" add))
                                  (choice (const :tag "string" string)
                                          (const :tag "word" word)
                                          (const :tag "none" none)))))



;;; ===== Dynamic completion customizations =====

(defcustom completion-use-dynamic t
  "*Enable dynamic completion.
Dynamic completion directly inserts the first completion into the
buffer without further action required by the user. It is still a
provisional completion, so until it is accepted all the usual
mechanisms for selecting completions are still available."
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-dynamic-highlight-common-substring t
  "*Highlight the longest common prefix in dynamic completions."
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-dynamic-highlight-prefix-alterations t
  "*Highlight alterations to the prefix in dynamic completions."
  :group 'completion-ui
  :type 'boolean)


(defface completion-dynamic-face
  '((((class color) (background dark))
     (:background "blue" :foreground "white"))
    (((class color) (background light))
     (:background "orange" :foreground "black")))
  "*Face used for provisional completions during dynamic completion.
Also used to highlight selected completions in tooltips and
pop-up frames."
  :group 'completion-ui)


(defface completion-dynamic-common-substring-face
  '((((class color) (background dark))
     (:background "dodger blue" :foreground "white"))
    (((class color) (background light))
     (:background "gold" :foreground "black")))
  "*Face used to highlight the common prefix in dynamic completions."
  :group 'completion-ui)


(defface completion-dynamic-prefix-alterations-face
  '((((class color) (background dark))
     (:background "slate blue" :foreground "white"))
    (((class color) (background light))
     (:background "yellow" :foreground "black")))
  "*Face used to highlight prefix alterations in dynamic completions."
  :group 'completion-ui)



;; ===== Hotkey customizations =====

(defcustom completion-use-hotkeys t
  "*Enable completion hotkeys (single-key selection of completions).

If t, enable hotkeys whenever completions are available. If nil,
disable hotkeys entirely. If set to the symbol 'pop-up, only
enable hotkeys when a tooltip or pop-up frame is active. (Note
that because the completion menu steals keyboard focus, enabling
hotkeys when the menu is active would be pointless. So don't
try to request this feature!)"
  :group 'completion-ui
  :type '(choice (const t)
                 (const pop-up)
                 (const nil)))


(defcustom completion-hotkey-list '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
  "*List of keys (vectors) to use for selecting completions
when `completion-use-hotkeys' is enabled."
  :group 'completion-ui
  :type '(repeat character))



;;; ===== Echo-text customizations =====

(defcustom completion-use-echo t
  "*Display completions in echo area."
  :group 'completion-ui
  :type 'boolean)



;;; ===== Tooltip customizations =====

(defcustom completion-tooltip-timeout 86400
  "*Number of seconds for wihch to display completion tooltip.
Unfortunately, there is no way to display a tooltip indefinitely
in Emacs. You can work around this by using a very large
number. (The completion tooltip disapears automatically as soon
as you do anything other than cycling through completions.)"
  :group 'completion-ui
  :type 'integer)


(defcustom completion-tooltip-offset '(0 . 0)
  "Pixel offset for tooltip.
This sometimes needs to be tweaked manually to get the tooltip in
the correct position under different window systems."
  :group 'completion-ui
  :type '(cons (integer :tag "x") (integer :tag "y")))


(defface completion-tooltip-face
  `((t . ,(or (and (stringp (face-attribute 'menu :background))
		   (stringp (face-attribute 'menu :foreground))
		   (list :background (face-attribute 'menu :background)
			 :foreground (face-attribute 'menu :foreground)))
	      '(:background "light yellow" :foreground "black"))))
  "*Face used in tooltip. Only :foreground, :background and :family
attributes are used."
  :group 'completion-ui)



;;; ===== Completion menu customizations =====

(defcustom completion-menu-offset '(0 . 0)
  "*Pixel offset for completion menus.
This sometimes needs to be tweaked manually to get completion
menus in the correct position under different window systems."
  :group 'completion-ui
  :type '(cons (integer :tag "x") (integer :tag "y")))


(defcustom completion-browser-max-items 25
  "*Maximum number of completions to display
in any one completion browser submenu."
  :group 'predictive
  :type 'integer)


(defcustom completion-browser-recurse-on-completions t
  "If non-nil, the completion browser will recursively list
completions of completions (of completions of completions...).
If nil, it will only display the original list of completions,
organised hierarchically.

Note that setting `completion-replaces-prefix' makes the browser
act as though this variable is set to nil, regardless of its
actual value, since recursing only makes sense for prefix
completion.")


(defcustom completion-browser-buckets 'balance
  "*Policy for choosing number of \"buckets\" in completion browser
when there are more than `completion-browser-max-items' to
display:

balance:  balance number of buckets and size of content
maximize: maximize number of buckets, minimize size of contents
mininize: minimize number of buckets, maximize size of contents"
  :group 'predictive
  :type '(choice (const :tag "balance" balance)
                 (const :tag "maximize" max)
                 (const :tag "minimize" min)))



;;; ===== Pop-up frame customizations =====

(defcustom completion-auto-popup-frame t
  "*Display completion pop-up frame automatically."
  :group 'completion-ui
  :type 'boolean)


(defcustom completion-popup-frame-max-height 20
  "*Maximum height of a popup frame"
  :group 'completion-ui
  :type 'integer)


(defcustom completion-popup-frame-offset '(0 . 0)
  "Pixel offset for pop-up frame.
This sometimes needs to be tweaked manually to get the pop-up
frame in the correct position under different window systems."
  :group 'completion-ui
  :type '(cons (integer :tag "x") (integer :tag "y")))




;;; ============================================================
;;;                 Other configuration variables

(defvar completion-function nil
  "Function that returns completions of a prefix.
It should accept two arguments, PREFIX and MAXNUM, and return a
list of at most MAXNUM completion candidates. If MAXNUM is nil,
it should return *all* completion candidates for PREFIX.

If `completion-function' doesn't actually do prefix completion at
all, e.g. it treats the \"prefix\" as a search pattern, and
returns matches that should replace that pattern in the buffer,
then `completion-replaces-prefix' should be set to non-nil to
modify the completion-UI behaviour appropriately.")
(make-variable-buffer-local 'completion-function)
(set-default 'completion-function nil)  ; ensure "global" value is nil


(defvar completion-prefix-function nil
  "Function that finds a prefix to complete at point.
It should return the prefix as a string.

Defaults to finding the `completion-word-thing' at or next to the
point.

If `completion-replaces-prefix' is non-nil, then the returned
string need not be an actual prefix to complete, but it should
still be a sequence characters located immediately before the
point in the current buffer.")
(make-variable-buffer-local 'completion-prefix-function)


(defvar completion-replaces-prefix nil
  "If non-nil, modify behaviour appropriately
for a `completion-function' that does something other than
straight prefix-completion.

Enable this if `completion-function' returns matches that should
completely replace the original \"prefix\" in the buffer, e.g. if
the \"prefix\" is used as a search pattern which should be
replaced by whatever matches.")
(make-variable-buffer-local 'completion-replaces-prefix)


(defvar completion-word-thing 'word
  "Symbol used to determine what is considered a word.

Used by `complete-word-at-point' and `completion-backward-delete'
in calls to `thing-at-point'. See `thing-at-point' for more
details. (Note that we require `forward-op' to be defined for
`completion-word-thing', which is *not* the case for all
pre-defined \"things\" in `thing-at-point'.)

More precisely, `completion-word-thing' is used by the default
`completion-prefix-function' (`completion-prefix') which is
called by the above functions to determine the prefix at
point. So it may be ignored if `completion-prefix-function' has
been changed from the default.

Note: this can be overridden by an \"overlay local\" binding (see
`auto-overlay-local-binding').")
(make-variable-buffer-local 'completion-word-thing)


(defvar completion-tooltip-function nil
  "Function to call to construct the tooltip text.

The function is called with three arguments, the prefix,
completions, and index of the currently active completion. It
should return a string containing the text to be displayed in the
tooltip.

Note: this can be overridden by an \"overlay local\" binding (see
`auto-overlay-local-binding').")


(defvar completion-menu nil
  "The completion menu keymap, or a function to call
to get a completion menu keymap (the latter is more usual).

If a function, that function is called with five arguments:
PREFIX, COMPLETIONS, CMPL-FUNCTION, CMPL-PREFIX-FUNCTION and
CMPL-REPLACES-PREFIX. It should return a menu keymap. The first
two arguments are self-explanatory. CMPL-FUNCTION,
CMPL-PREFIX-FUNCTION and CMPL-REPLACES-PREFIX and the values of
`completion-function', `completion-prefix-function', and
`completion-replaces-prefix' for the current completion.

Note: the value of `completion-menu' can be overridden by an
\"overlay local\" binding (see `auto-overlay-local-binding').")


(defvar completion-browser-menu-function nil
  "Function to call to get a browser menu keymap.

The function is called with five arguments: PREFIX, COMPLETIONS,
CMPL-FUNCTION, CMPL-PREFIX-FUNCTION and CMPL-REPLACES-PREFIX It
should return a menu keymap. The first two arguments are
self-explanatory. CMPL-FUNCTION, CMPL-PREFIX-FUNCTION and
CMPL-REPLACES-PREFIX and the values of `completion-function',
`completion-prefix-function', and `completion-replaces-prefix'
for the currenct completion.

Note: this can be overridden by an \"overlay local\" binding (see
`auto-overlay-local-binding').")


(defvar completion-popup-frame-function nil
  "Function to call to construct pop-up frame text.

The function is called with two arguments, PREFIX and
COMPLETIONS. It should return a list of strings, which are used
\(in order\) as the lines of text in the pop-up frame.

Note: this can be overridden by an \"overlay local\" binding (see
`auto-overlay-local-binding').")


(defvar completion-accept-functions nil
  "Hook run after a completion is accepted.

Completions are accepted by calling `completion-accept',
selecting one with a hotkey, or selecting one from a
menu. Functions are passed three arguments: the prefix, the
complete string that was accepted (including the prefix), and any
prefix argument supplied by the user if the accept command was
called interactively.")


(defvar completion-reject-functions nil
  "Hook run after a completion is rejected.

Completions are rejected by calling
`completion-reject'. Functions are passed three arguments: the
prefix, the complete string that was rejected \(including the of
the prefix\), and any prefix argument supplied by the user if the
rejection command was called interactively.")


;; (defvar completion-tab-complete-functions nil
;;   "Hook run after tab-completion.
;; Functions are passed two arguments: the complete string that has
;; been inserted so far \(prefix and tab-completion combined\).")


(defvar completion-map nil
  "Keymap active when a completion-function is defined.")

(defvar completion-dynamic-map nil
  "Keymap active in a dynamic completion overlay.")


(defvar auto-completion-map nil
  "Keymap active when auto-completion-mode is enabled.")


(defvar auto-completion-dynamic-map nil
  "Keymap active in a dynamic completion overlay when
  auto-completion-mode is enabled.")


(defvar completion-tooltip-map nil
  "Keymap used when a tooltip is displayed.")

(defvar completion-popup-frame-mode-map nil
  "Keymap used by completion pop-up frames.")




;;; ============================================================
;;;                     Internal variables

(defvar completion-overlay-list nil
  "List of overlays used during completion")
(make-variable-buffer-local 'completion-overlay-list)


(defvar completion-auto-timer (timer-create)
  "Timer used to postpone auto-completion or auto-display
of tooltip/menu/pop-up frame until there's a pause in typing.")


(defvar completion-tooltip-active nil
  "Used to enable `completion-tooltip-map' when a tooltip is displayed.")

(defvar completion-backward-delete-timer nil
  "Timer used to postpone completion until finished deleting.")


(defvar completion-trap-recursion nil
  "Used to trap infinite recursion errors
in calls to certain completion functions, for which infinite
recursion can result from incorrectly configured key bindings set
by the Emacs user.")




;;; =================================================================
;;;            Set properties for delete-selection-mode

(put 'auto-completion-self-insert 'delete-selection t)
(put 'completion-accept-and-newline 'delete-selection t)
(put 'completion-backward-delete-char 'delete-selection 'supersede)
(put 'completion-backward-delete-char-untabify
     'delete-selection 'supersede)
(put 'completion-delete-char 'delete-selection 'supersede)




;;; ===============================================================
;;;                  Keybinding functions

(defun completion-define-word-constituent-binding
  (key char &optional syntax no-syntax-override)
  "Setup key binding for KEY so that it inserts character CHAR as
though it's syntax were SYNTAX. SYNTAX defaults to
word-constituent, ?w, hence the name of the function, but it can
be used to set up any syntax.

If NO-SYNTAX-OVERRIDE is non-nil, this binding will cause
`auto-completion-override-syntax-alist' to be ignored when this
key binding is used, so that the behaviour is determined only by
SYNTAX."

  (when (null syntax) (setq syntax ?w))
  (let ((doc (concat "Insert \"" (string char) "\" as though it were a\
 word-constituent.")))

    ;; create `auto-completion-dynamic-map' binding
    (define-key auto-completion-dynamic-map key
      `(lambda () ,doc
         (interactive)
         (auto-completion-self-insert ,char ,syntax
                                      ,no-syntax-override)))

    ;; if emacs version doesn't support overlay keymaps properly, create
    ;; binding in `completion-map' to simulate it via
    ;; `completion-run-if-within-overlay' hack
    (when (<= emacs-major-version 21)
      (define-key completion-map key
        `(lambda () ,doc
           (interactive)
           (completion-run-if-within-overlay
            (lambda () (interactive)
              (auto-completion-self-insert ,char ,syntax ,no-syntax-override))
            'completion-function))))))



(defun completion-remap-delete-commands (map)
  "Remap deletion commands to completion-UI versions
\(or substitute existing bindings, if remapping is not supported\)."

  ;; If command remapping is supported, remap delete commands
  (if (fboundp 'command-remapping)
      (progn
        (define-key map [remap delete-char]
          'completion-delete-char)
        (define-key map [remap backward-delete-char]
          'completion-backward-delete-char)
        (define-key map [remap delete-backward-char]
          'completion-backward-delete-char)
        (define-key map [remap backward-delete-char-untabify]
          'completion-backward-delete-char-untabify)
        (define-key map [remap kill-word]
          'completion-kill-word)
        (define-key map [remap backward-kill-word]
          'completion-backward-kill-word)
        (define-key map [remap kill-sentence]
          'completion-kill-sentence)
        (define-key map [remap backward-kill-sentence]
          'completion-backward-kill-sentence)
        (define-key map [remap kill-sexp]
          'completion-kill-sexp)
        (define-key map [remap backward-kill-sexp]
          'completion-backward-kill-sexp)
        (define-key map [remap kill-line]
          'completion-kill-line)
        (define-key map [remap kill-paragraphs]
          'completion-kill-paragraph)
        (define-key map [remap backward-kill-paragraph]
          'completion-backward-kill-paragraph))

    ;; Otherwise, can't do better than define bindings for the keys
    ;; that are currently bound to them
    (dolist (key '([delete] [deletechar] [backspace] "\d"
                   [(control delete)] [(control deletechar)]
                   [(meta delete)] [(meta deletechar)]
                   [(control backspace)] [(meta backspace)] "\M-\d"))
      (catch 'rebound
        (dolist (binding '((delete-char . completion-delete-char)
                           (kill-word . completion-kill-word)
                           (kill-sentence . completion-kill-sentence)
                           (kill-sexp . completion-kill-sexp)
			   (kill-line . completion-kill-line)
                           (kill-paragraph . completion-kill-paragraph)
                           (backward-delete-char
                            . completion-backward-delete-char)
                           (delete-backward-char
                            . completion-backward-delete-char)
                           (backward-delete-char-untabify
                            . completion-backward-delete-char-untabify)
                           (backward-kill-word
                            . completion-backward-kill-word)
                           (backward-kill-sentence
                            . completion-backward-kill-sentence)
                           (backward-kill-sexp
                            . completion-backward-kill-sexp)
                           (backward-kill-paragraph
                            . completion-backward-kill-paragraph)))
          (when (eq (key-binding key) (car binding))
            (define-key map key (cdr binding))
            (throw 'rebound t)))))))



(defun completion-bind-printable-chars (map command)
  "Manually bind printable characters to COMMAND.
Command remapping is a far better way to do this, so it should only be
used if the current Emacs version lacks command remapping support."
  (define-key map "A" command)
  (define-key map "a" command)
  (define-key map "B" command)
  (define-key map "b" command)
  (define-key map "C" command)
  (define-key map "c" command)
  (define-key map "D" command)
  (define-key map "d" command)
  (define-key map "E" command)
  (define-key map "e" command)
  (define-key map "F" command)
  (define-key map "f" command)
  (define-key map "G" command)
  (define-key map "g" command)
  (define-key map "H" command)
  (define-key map "h" command)
  (define-key map "I" command)
  (define-key map "i" command)
  (define-key map "J" command)
  (define-key map "j" command)
  (define-key map "K" command)
  (define-key map "k" command)
  (define-key map "L" command)
  (define-key map "l" command)
  (define-key map "M" command)
  (define-key map "m" command)
  (define-key map "N" command)
  (define-key map "n" command)
  (define-key map "O" command)
  (define-key map "o" command)
  (define-key map "P" command)
  (define-key map "p" command)
  (define-key map "Q" command)
  (define-key map "q" command)
  (define-key map "R" command)
  (define-key map "r" command)
  (define-key map "S" command)
  (define-key map "s" command)
  (define-key map "T" command)
  (define-key map "t" command)
  (define-key map "U" command)
  (define-key map "u" command)
  (define-key map "V" command)
  (define-key map "v" command)
  (define-key map "W" command)
  (define-key map "w" command)
  (define-key map "X" command)
  (define-key map "x" command)
  (define-key map "Y" command)
  (define-key map "y" command)
  (define-key map "Z" command)
  (define-key map "z" command)
  (define-key map "'" command)
  (define-key map "-" command)
  (define-key map "<" command)
  (define-key map ">" command)
  (define-key map " " command)
  (define-key map "." command)
  (define-key map "," command)
  (define-key map ":" command)
  (define-key map ";" command)
  (define-key map "?" command)
  (define-key map "!" command)
  (define-key map "\"" command)
  (define-key map "0" command)
  (define-key map "1" command)
  (define-key map "2" command)
  (define-key map "3" command)
  (define-key map "4" command)
  (define-key map "5" command)
  (define-key map "6" command)
  (define-key map "7" command)
  (define-key map "8" command)
  (define-key map "9" command)
  (define-key map "~" command)
  (define-key map "`" command)
  (define-key map "@" command)
  (define-key map "#" command)
  (define-key map "$" command)
  (define-key map "%" command)
  (define-key map "^" command)
  (define-key map "&" command)
  (define-key map "*" command)
  (define-key map "_" command)
  (define-key map "+" command)
  (define-key map "=" command)
  (define-key map "(" command)
  (define-key map ")" command)
  (define-key map "{" command)
  (define-key map "}" command)
  (define-key map "[" command)
  (define-key map "]" command)
  (define-key map "|" command)
  (define-key map "\\" command)
  (define-key map "/" command)
)



(defun completion-simulate-overlay-bindings (source dest variable
                                                    &optional no-parent)
  ;; Simulate SOURCE overlay keybindings in DEST using the
  ;; `completion-run-if-within-overlay' hack. DEST should be a symbol whose
  ;; value is a keymap, SOURCE should be a keymap.
  ;;
  ;; VARIABLE should be a symbol that deactivates DEST when its value is
  ;; (temporarily) set to nil. Usually, DEST will be a minor-mode keymap and
  ;; VARIABLE will be the minor-mode variable with which it is associated in
  ;; `minor-mode-map-alist'.
  ;;
  ;; NO-PARENT will prevent this recursing into the parent keymap of SOURCE,
  ;; if it has one.

  ;; if NO-PARENT is specified, remove parent keymap if there is one
  (when (and no-parent (memq 'keymap (cdr source)))
    (setq source
          (completion--sublist
           source 0 (1+ (completion--position 'keymap (cdr source))))))

  ;; map over all bindings in SOURCE
  (map-keymap
   (lambda (key binding)
     ;; don't simulate remappings, and don't simulate parent keymap's bindings
     (unless (eq key 'remap)
       ;; usually need to wrap key in an array for define-key
       (unless (stringp key) (setq key (vector key)))
       ;; bind key in DEST to simulated overlay keymap binding
       (define-key dest key
         (completion-construct-simulated-overlay-binding binding variable))))
   source))



(defun completion-construct-simulated-overlay-binding (binding variable)
  ;; Return a binding that simulates assigning BINDING to KEY in an overlay
  ;; keymap, using the `completion-run-if-within-overlay' hack.
  ;;
  ;; VARIABLE should be a symbol that deactivates BINDING when its value is
  ;; (temporarily) set to nil. Typically, BINDING will be bound in a
  ;; minor-mode keymap and VARIABLE will be the minor-mode variable with which
  ;; it is associated in `minor-mode-map-alist'.
  ;;
  ;; The return value is a command if BINDING was a command, or a keymap if
  ;; BINDING was a keymap. Any other type of BINDING (e.g. a remapping)
  ;; returns nil, since there is no easy way to simulate this.

  (cond
   ;; don't simulate command remappings or keyboard macros
   ((or (eq binding 'remap) (stringp binding))
    nil)


   ;; if BINDING is a keymap, call ourselves recursively to construct a keymap
   ;; filled with bindings that simulate an overlay keymap
   ((keymapp binding)
    (let ((map (make-sparse-keymap)))
      (map-keymap
       (lambda (key bind)
         ;; usually need to wrap key in an array for define-key
         (unless (stringp key) (setq key (vector key)))
         (define-key map key
           (completion-construct-simulated-overlay-binding bind variable)))
       binding)
      map))


   ;; if BINDING is a command, construct an anonymous command that simulates
   ;; binding that command in an overlay keymap
   ((or (commandp binding) (and (symbolp binding) (symbol-function binding)))
    (let (funcdef arglist docstring interactive args)

      ;; get function definition of command
      (cond
       ((symbolp binding) (setq funcdef (symbol-function binding)))
       ((functionp binding) (setq funcdef binding)))

      ;; extract argument list
      (cond
       ;; compiled function
       ((byte-code-function-p funcdef) (setq arglist (aref funcdef 0)))
       ;; uncompiled function
       ((listp funcdef) (setq arglist (nth 1 funcdef))))

      ;; extract docstring and interactive definition
      (setq docstring (documentation binding))
      (setq interactive (interactive-form binding))

      ;; construct docstring for new binding
      (setq docstring
            (concat "Do different things depending on whether point is "
                    "within a provisional completion.\n\n"
                    "If point is within a provisional completion,\n"
                    (downcase (substring docstring 0 1))
                    (substring docstring 1)
                    "\n\n"
                    "If point is not within a provisional completion,\n"
                    "run whatever would normally be bound to "
                    "this key sequence."))

      ;; construct list of argument variable names, removing &optional and
      ;; &rest
      (setq args '(list))
      (mapc (lambda (a)
              (unless (or (eq a '&optional) (eq a '&rest))
                (setq args (append args (list a)))))
            arglist)

      ;; construct and return command to simulate overlay keymap binding
      `(lambda ,(copy-sequence arglist)
         "" ;,docstring
         ,(copy-sequence interactive)
         (completion-run-if-within-overlay
          (lambda ,(copy-sequence arglist) ,(copy-sequence interactive)
            (apply ',binding ,args))
          ',variable))
      ))

   ;; anything else is an error
   (t (error (concat "Unexpected binding in "
                     "`completion-construct-simulated-overlay-binding': %s")
             binding))))




;;; =================================================================
;;;                     Setup default keymaps

;; Set the default keymap if it hasn't been defined already (most likely
;; in an init file). This keymap is active whenever `completion-function'
;; is non-nil.
(unless completion-map
  ;; If the current Emacs version doesn't support overlay keybindings
  ;; half decently and doesn't support command remapping, we're going to
  ;; have to bind all printable characters in this keymap, so we might as
  ;; well create a full keymap
  (if (and (<= emacs-major-version 21)
           (not (fboundp 'command-remapping)))
      (setq completion-map (make-keymap))
    (setq completion-map (make-sparse-keymap)))

  ;; M-<tab> and M-/ cycle or complete word at point
  (define-key completion-map [?\M-\t] 'complete-or-cycle-word-at-point)
  (define-key completion-map "\M-/" 'complete-or-cycle-word-at-point)

  ;; M-<shift>-<tab> and M-? (usually M-<shift>-/) cycle backwards
  (define-key completion-map [(meta shift iso-lefttab)]
    'complete-or-cycle-backwards-word-at-point)
  (define-key completion-map "\M-?"
    'complete-or-cycle-backwards-word-at-point)

  ;; RET deals with any pending completion candidate, then runs
  ;; whatever is usually bound to RET.
  ;; Note: although this uses `completion-run-if-within-overlay', it is
  ;;       not a hack to work-around poor overlay keybinding
  ;;       support. Rather, we are using it to run
  ;;       `completion-resolve-current' and then run the normal RET
  ;;       keybinding. We bind it here instead of in the overlay keymap
  ;;       because it's easier to disable this keymap.
  (dolist (key '("\r" "\n" [return]))
    (define-key completion-map key 'completion-resolve-then-run-command))

  ;; remap the deletion commands
  (completion-remap-delete-commands completion-map)


  ;; ----- Simulated overlay keybindings -----

  ;; Note: could remove this and leave it up to the call to the
  ;;       `completion-simulate-overlay-keybindings' function at the very end
  ;;       of this file, if only that function could deal with remappings

  ;; If the current Emacs version doesn't support overlay keybindings
  ;; half decently, have to simulate them using the
  ;; `completion-run-if-within-overlay' hack.
;;  (when (<= emacs-major-version 21)
    ;; if we can remap commands, remap `self-insert-command' to
    ;; `completion-self-insert'
    (if (fboundp 'command-remapping)
        (define-key completion-map [remap self-insert-command]
          'completion-self-insert)
      ;; otherwise, rebind all printable characters to
      ;; `completion-self-insert' manually
      (completion-bind-printable-chars completion-map
                                       'completion-self-insert));)

  ;; <up> and <down> cycle the tooltip
  ;; Note: it shouldn't be necessary to bind them here, but the bindings in
  ;;       completion-tooltip-map don't work when the tooltip is
  ;;       auto-displayed, for mysterious reasons (note that we can't use
  ;;       `completion-run-if-condition' in `completion-dynamic-map', hence
  ;;       binding them here)
  (define-key completion-map [down]
    (lambda () (interactive)
      (completion-run-if-condition
       'completion-tooltip-cycle
       'completion-function
       completion-tooltip-active)))
  (define-key completion-map [down]
    (lambda () (interactive)
      (completion-run-if-condition
       'completion-tooltip-cycle-backwards
       'completion-function
       completion-tooltip-active)))
  )


;; make sure completion-map is associated with `completion-function' in
;; the minor-mode-keymap-alist, so that the bindings are enabled whenever
;; a completion function is defined
(let ((existing (assq 'completion-function minor-mode-map-alist)))
  (if existing
      (setcdr existing completion-map)
    (push (cons 'completion-function completion-map)
          minor-mode-map-alist)))




;; Set the default bindings for the keymap assigned to the completion
;; overlays created when dynamic completion is enabled, if it hasn't been
;; defined already (most likely in an init file).
(unless completion-dynamic-map
  ;; Note: rebinding printable characters here is redundant if
  ;;       `auto-completion-mode' is enabled, since they are also bound
  ;;       in `auto-completion-map', but we still need to ensure that the
  ;;       provisional completion is correctly dealt with even if
  ;;       `auto-completion-mode' is disabled.

  ;; if we can remap commands, remap `self-insert-command' to
  ;; `completion-self-insert'
  (if (fboundp 'command-remapping)
      (progn
        (setq completion-dynamic-map (make-sparse-keymap))
        (define-key completion-dynamic-map [remap self-insert-command]
          'completion-self-insert))
    ;; otherwise, create a great big keymap and rebind all printable
    ;; characters to `completion-self-insert' manually
    (setq completion-dynamic-map (make-keymap))
    (completion-bind-printable-chars completion-dynamic-map
                                     'completion-self-insert))

  ;; C-RET accepts, C-DEL rejects
  (define-key completion-dynamic-map [(control return)] 'completion-accept)
  (define-key completion-dynamic-map [(control backspace)] 'completion-reject)

  ;; <tab> does traditional tab-completion
  (define-key completion-dynamic-map "\t" 'completion-tab-complete)

  ;; C-<tab> scoots ahead
  (define-key completion-dynamic-map [(control tab)]
    'completion-scoot-ahead)

  ;; C-<space> abandons
  (define-key completion-dynamic-map [?\C- ] 'completion-reject)

  ;; S-<down>, M-<down> and C-<down> display the completion tooltip,
  ;; menu, or pop-up frame
  (define-key completion-dynamic-map [S-down] 'completion-show-tooltip)
  (define-key completion-dynamic-map [M-down] 'completion-show-menu)
  (define-key completion-dynamic-map [C-down] 'completion-popup-frame)

  ;; clicking on a completion displays the completion menu
  (define-key completion-dynamic-map [mouse-2] 'completion-show-menu)

  ;; remap the deletion commands
  (completion-remap-delete-commands completion-dynamic-map))


;; Set the default auto-completion-mode keymap if it hasn't been defined
;; already (most likely in an init file). This keymap is active when
;; `auto-completion-mode' is enabled.
(unless auto-completion-map
  ;; if we can remap commands, remap `self-insert-command'
  (if (fboundp 'command-remapping)
      (progn
        (setq auto-completion-map (make-sparse-keymap))
        (define-key auto-completion-map [remap self-insert-command]
          'auto-completion-self-insert))
    ;; otherwise, create a great big keymap where all printable characters run
    ;; `auto-completion-self-insert', which decides what to do based on the
    ;; character's syntax
    (setq auto-completion-map (make-keymap))
    (completion-bind-printable-chars auto-completion-map
                                     'auto-completion-self-insert))
  ;; remap the deletion commands
  (completion-remap-delete-commands auto-completion-map))



;; Set the default bindings for the keymap assigned to the completion overlays
;; created when dynamic completion and auto-completion are enabled, if it
;; hasn't been defined already (most likely in an init file).
(unless auto-completion-dynamic-map
  ;; inherit all keybindings from completion-dynamic-map, then add
  ;; auto-completion specific ones below
  (setq auto-completion-dynamic-map (make-sparse-keymap))
  (set-keymap-parent auto-completion-dynamic-map completion-dynamic-map)

  ;; M-<space> abandons and inserts a space
  (define-key auto-completion-dynamic-map "\M- "
    (lambda (&optional arg)
      "Reject any current provisional completion and insert a space."
      (interactive "P")
      (completion-reject arg)
      (insert " ")))

  ;; M-S-<space> inserts a space as a word-constituent
  (define-key auto-completion-dynamic-map [?\M-\S- ]
    (lambda ()
      "Insert a space as though it were a word-constituent."
      (interactive)
      (auto-completion-self-insert ?\  ?w t)))

  ;; M-. inserts "." as a word-constituent
  (define-key auto-completion-dynamic-map "\M-."
    (lambda ()
      "Insert \".\" as though it were a word-constituent."
      (interactive)
      (auto-completion-self-insert ?. ?w t)))

  ;; M-- inserts "-" as a word-constituent
  (define-key auto-completion-dynamic-map "\M--"
    (lambda ()
      "Insert \"-\" as though it were a word-constituent."
      (interactive)
      (auto-completion-self-insert ?- ?w t)))

;;;   ;; M-/ inserts "/" as a word-constituent
;;;   (define-key auto-completion-dynamic-map "\M-/"
;;;     (lambda ()
;;;       "Insert \"/\" as though it were a word-constituent."
;;;       (interactive)
;;;       (auto-completion-self-insert ?/ ?w t)))
)



;; Note:
;;
;; `completion-tooltip-active' is reset by `pre-command-hook' (see end of
;; file), so the keymap below is disabled before every command is
;; executed. However, the key bindings are looked up before `pre-command-hook'
;; runs, so the first key sequence after displaying a tooltip has a chance of
;; running something from here. This is exactly what we want, since Emacs
;; hides tooltips after every command and we only want this keymap to be
;; active if a tooltip is visible.
;;
;; The cycling commands bound below re-display the completion tooltip, which
;; causes `completion-tooltip-active' to be set to non-nil again. So after
;; they've run, the keymap is left active again for the next key sequence.


;; Set default key bindings for the keymap used when a completion tooltip
;; is displayed, unless it's already been set (most likely in an init
;; file). This keymap is active when `completion-tooltip-active' is
;; non-nil.
(unless completion-tooltip-map
  (let ((map (make-sparse-keymap)))
    ;; <up> and <down> cycle completions, which appears to move selection
    ;; up and down tooltip entries
    (define-key map [down] 'completion-tooltip-cycle)
    (define-key map [up] 'completion-tooltip-cycle-backwards)
    (setq completion-tooltip-map map)))


;; make sure completion-tooltip-map is in minor-mode-keymap-alist
(let ((existing (assq 'completion-tooltip-active minor-mode-map-alist)))
  (if existing
      (setcdr existing completion-tooltip-map)
    (push (cons 'completion-tooltip-active completion-tooltip-map)
          minor-mode-map-alist)))



;; Set default keybindings for the keymap used in completion pop-up
;; frames (actually, used by the completion-popup-frame major mode),
;; unless it's already been set (most likely in an init file).
(unless completion-popup-frame-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-n" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'next-line arg)))
    (define-key map "\C-n" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'next-line arg)))
    (define-key map [down] (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'next-line arg)))
    (define-key map "\M-p" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'previous-line arg)))
    (define-key map "\C-p" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'previous-line arg)))
    (define-key map [up] (lambda (&optional arg) (interactive)
                           (completion-popup-frame-motion
                            'previous-line arg)))
    (define-key map "\C-v" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'scroll-up arg)))
    (define-key map [next] (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'scroll-up arg)))
    (define-key map "\M-v" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'scroll-down arg)))
    (define-key map [prior] (lambda (&optional arg) (interactive)
                              (completion-popup-frame-motion
                               'scroll-down arg)))
    (define-key map [home] (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'beginning-of-buffer arg)))
    (define-key map "\M-<" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'beginning-of-buffer arg)))
    (define-key map [end] (lambda (&optional arg) (interactive)
                            (completion-popup-frame-motion
                             'end-of-buffer arg)))
    (define-key map "\M->" (lambda (&optional arg) (interactive)
                             (completion-popup-frame-motion
                              'end-of-buffer arg)))
    (define-key map "\C-u" 'universal-argument)
    (define-key map [?\C--] 'negative-argument)
    (define-key map [C-up] 'completion-popup-frame-dismiss)
    (define-key map [M-up] 'completion-popup-frame-dismiss)
    (define-key map [?\M-\t] 'completion-popup-frame-toggle-show-all)
    (define-key map "\M-/" 'completion-popup-frame-toggle-show-all)
    (define-key map [t] 'completion-popup-frame-unread-key)
    (setq completion-popup-frame-mode-map map)))




;;; ================================================================
;;;                Replacements for CL functions

(defun completion--sublist (list start &optional end)
  "Return the sub-list of LIST from START to END.
If END is omitted, it defaults to the length of the list
If START or END is negative, it counts from the end."
  (let (len)
    ;; sort out arguments
    (if end
        (when (< end 0) (setq end (+ end (setq len (length list)))))
      (setq end (or len (setq len (length list)))))
    (when (< start 0)
      (setq start (+ start (or len (length list)))))

    ;; construct sub-list
    (let (res)
      (while (< start end)
        (push (nth start list) res)
        (setq start (1+ start)))
      (nreverse res))))



(defun completion--position (item list)
  "Find the first occurrence of ITEM in LIST.
Return the index of the matching item, or nil of not found.
Comparison is done with 'equal."
  (let (el (i 0))
    (catch 'found
      (while (setq el (nth i list))
        (when (equal item el) (throw 'found i))
        (setq i (1+ i))
        nil))))



;;; ================================================================
;;;                Interface abstraction macros

(defun completion-lookup-behaviour (&optional char syntax)
  "Return syntax-dependent behaviour
of character CHAR and/or syntax-class SYNTAX. At least one of
these must be supplied. If both are supplied, SYNTAX overrides the
syntax-class of CHAR."

  ;; SYNTAX defaults to syntax-class of CHAR
  (when (and char (not syntax)) (setq syntax (char-syntax char)))

  ;; get syntax alists
  (let ((syntax-alist
         (if (fboundp 'auto-overlay-local-binding)
             (auto-overlay-local-binding
              'auto-completion-syntax-alist)
           auto-completion-syntax-alist))
        (override-alist
         (if (fboundp 'auto-overlay-local-binding)
             (auto-overlay-local-binding
              'auto-completion-override-syntax-alist)
           auto-completion-override-syntax-alist))
	(global-syntax-alist auto-completion-syntax-alist)
	behaviour)

    ;; if `auto-completion-syntax-alist' is a predefined behaviour (a
    ;; cons cell), convert it to an alist
    (dolist (alist '(syntax-alist global-syntax-alist))
      (unless (listp (car (eval alist)))
	(set alist
	     `(;; word constituents add to current completion and complete
	       ;; word or string, depending on VALUE
	       (?w . (add ,(cdr (eval alist))))
	       ;; symbol constituents, whitespace and punctuation characters
	       ;; either accept or reject, depending on VALUE, and don't
	       ;; complete
	       (?_ .  (,(car (eval alist)) none))
	       (?  .  (,(car (eval alist)) none))
	       (?. .  (,(car (eval alist)) none))
	       (?\( . (,(car (eval alist)) none))
	       (?\) . (,(car (eval alist)) none))
	       ;; anything else rejects and does't complete
	       (t . (reject none)))
	     )))

    ;; extract behaviours from syntax alists
    (setq behaviour (or (when char (cdr (assq char override-alist)))
                        (cdr (assq syntax syntax-alist))
                        (cdr (assq t syntax-alist))
			;; fall back to global values if not defined in
			;; overlay-local ones
			(cdr (assq char auto-completion-override-syntax-alist))
			(cdr (assq syntax global-syntax-alist))))
    (when (= (length behaviour) 2)
      (setq behaviour (append behaviour '(t))))
    ;; return behaviour
    behaviour))



(defmacro completion-get-resolve-behaviour (behaviour)
  "Extract syntax-dependent resolve behaviour from BEHAVIOUR.
BEHAVIOUR should be the return value of a call to
`completion-lookup-behaviour'."
  `(nth 0 ,behaviour))


(defmacro completion-get-completion-behaviour (behaviour)
  "Extract syntax-dependent completion behaviour from BEHAVIOUR.
BEHAVIOUR should be the return value of a call to
`completion-lookup-behaviour'."
  `(nth 1 ,behaviour))


(defmacro completion-get-insertion-behaviour (behaviour)
  "Extract syntax-dependent insertion behaviour from BEHAVIOUR.
BEHAVIOUR should be the return value of a call to
`completion-lookup-behaviour'."
  `(nth 2 ,behaviour))



;;; =======================================================
;;;         Auto-completion minor-mode definition

(define-minor-mode auto-completion-mode
  "Toggle auto-completion mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

In auto-completion-mode, Emacs will try to complete words as you
type, using whatever completion method has been set up (either by the
major mode, or by another minor mode)."
  nil                   ; init-value
  " Complete"           ; lighter
  auto-completion-map   ; keymap
  ;; refuse to enable if no `completion-function' is defined
  (when (and auto-completion-mode (null completion-function))
    (setq auto-completion-mode nil)
    (message (concat "No `completion-function' defined; "
		     "auto-completion-mode NOT enabled"))))


(defun turn-on-auto-completion-mode ()
  "Turn on auto-completion mode. Useful for adding to hooks."
  (unless auto-completion-mode (auto-completion-mode)))




;;; =======================================================
;;;              User-interface functions

(defun complete-in-buffer
  (&optional prefix cmpl-function cmpl-prefix-function cmpl-replaces-prefix
	     auto pos)
  "Complete PREFIX, or prefix at point if none specified.

CMPL-FUNCTION, CMPL-PREFIX-FUNCTION and CMPL-REPLACES-PREFIX
specify the `completion-function' and
`completion-prefix-function' to use, and whether
`completion-replaces-prefix'. If they are not specified, the
global values of these variables are used. See the global
variables for an explanation of their formats.

AUTO and POS are for internal use only. If AUTO is non-nil,
assume we're auto-completing and respect settings of
`auto-completion-min-chars' and `auto-completion-delay'. If POS
is non-nil, only complete if point is at POS."

  ;; cancel any timer so that we don't have two running at once
  (cancel-timer completion-auto-timer)

  ;; only complete if point is at POS (only used when called from timer)
  (unless (and pos (/= (point) pos))

    ;; if we're auto-completing and `auto-completion-delay' is set,
    ;; delay completing by setting a timer to call ourselves later
    (if (and auto auto-completion-delay)
        (setq completion-auto-timer
              (run-with-idle-timer auto-completion-delay nil
                                   'complete-in-buffer
                                   prefix cmpl-function cmpl-prefix-function
				   cmpl-replaces-prefix (not 'auto) (point)))

      ;; otherwise...
      (let ((overlay (completion-overlay-at-point))
            completions)
        ;; resolve any provisional completions
        (completion-resolve-old overlay)

        ;; get prefix
	(unless (or prefix
		    (and overlay (setq prefix (overlay-get overlay 'prefix))))
	  (setq cmpl-prefix-function
		(or (and (fboundp 'auto-overlay-local-binding)
			 (let ((completion-prefix-function
				cmpl-prefix-function))
			   (auto-overlay-local-binding
			    'completion-prefix-function)))
		    completion-prefix-function
		    'completion-prefix))
	  (setq prefix (or prefix (funcall cmpl-prefix-function))))

        ;; if auto-completing, only complete prefix if it has requisite
        ;; number of characters
        (unless (and auto auto-completion-min-chars
                     (< (length prefix) auto-completion-min-chars))
          ;; get completions
	  (setq cmpl-function
		(or (and (fboundp 'auto-overlay-local-binding)
			 (let ((completion-function cmpl-function))
			   (auto-overlay-local-binding 'completion-function)))
		    completion-function))
          (setq completions (funcall completion-function
				     prefix completion-max-candidates))

          ;; setup completion overlay
	  (setq cmpl-replaces-prefix
		(or (and (fboundp 'auto-overlay-local-binding)
			 (let ((completion-replaces-prefix
				cmpl-replaces-prefix))
			   (auto-overlay-local-binding
			    'completion-replaces-prefix)))
		    completion-replaces-prefix))
          (setq overlay
		(completion-setup-overlay
		 prefix t completions nil
		 cmpl-function cmpl-prefix-function cmpl-replaces-prefix
		 overlay))
          (move-overlay overlay (point) (point))

          ;; activate dynamic completion
          (when completion-use-dynamic (complete-dynamic overlay))

          ;; display completion echo text
          (when completion-use-echo (complete-echo overlay))

          ;; no need to do anything for hotkeys, it's all done when the
          ;; `completion-select' command is called

          ;; if a pop-up frame is already displayed, update it
          (if (overlay-get overlay 'popup-frame)
              (completion-popup-frame overlay)
            ;; otherwise, activate display of tooltip/menu/pop-up frame
            (when (and completion-auto-show completions)
              (completion-auto-show overlay)))
          )))))



(defun complete-dynamic (overlay)
  "Insert dynamic completion and update completion OVERLAY
accordingly. The point had better be within OVERLAY or
cauliflower will start growing out of your ears."

  (let ((pos (make-marker)))
    ;; insert new completion, if any
    (let ((prefix (overlay-get overlay 'prefix))
	  (completions (overlay-get overlay 'completions))
	  (prefix-len (overlay-get overlay 'prefix-length))
	  (cmpl-replaces-prefix
	   (overlay-get overlay 'completion-replaces-prefix))
	  cmpl len)
      (when completions
	;; For some unknown reason, the delete-region or insert (below) can
	;; sometimes delete or move the completion overlay, so we mark its
	;; start position with marker `pos' before doing anything else, in
	;; order to move the completion overlay into the correct new position
	;; later.
	(move-marker pos (overlay-start overlay))
	(set-marker-insertion-type pos nil)
        ;; delete prefix if `completion-replaces-prefix' is non-nil and
        ;; `auto-completion-mode' is disabled
        (delete-region
	 (- pos
	    (if (or (null cmpl-replaces-prefix)
		    (and (eq completion-resolve-behaviour 'accept)
			 (not (overlay-get overlay 'prefix-replaced))
			 (overlay-put overlay 'prefix-replaced t)))
		(overlay-get overlay 'prefix-length) 0))
	 (overlay-end overlay))
        ;; insert new completion
	(setq cmpl (car completions)
	      len  (length prefix))
	(unless (stringp cmpl)
	  (setq len  (cdr cmpl)
		cmpl (car cmpl)))
        (let ((overwrite-mode nil)) (insert cmpl))
        (move-overlay overlay
		      (+ pos (if cmpl-replaces-prefix 0 len))
		      (+ pos (length cmpl)))
        (overlay-put overlay 'prefix-length len)
        (overlay-put overlay 'completion-num 0)
	;; highlight alterations to prefix, if enabled
	(when (and completion-dynamic-highlight-prefix-alterations
		   (not cmpl-replaces-prefix))
	  (completion-highlight-prefix-alterations prefix cmpl pos len))
        ;; highlight common substring, if enabled
        (when completion-dynamic-highlight-common-substring
	  (completion-highlight-common-substring
	   prefix cmpl pos len overlay)))

      ;; move point to appropriate position in the overlay
      (completion-position-point-in-overlay overlay))

    ;; delete temporary marker
    (set-marker pos nil)))



(defun completion-auto-show (&optional overlay point)
  "Display list of completions for OVERLAY in tooltip/menu/pop-up frame.
The point had better be within OVERLAY or your hair will fall
out.

Which one is shown depends on the setting of `completion-auto-show'. If
`completion-auto-show-delay' is non-nil, the tooltip/menu/pop-up frame
will only be displayed after a delay.

If OVERLAY is not supplies, tries to find one at point.

If POINT is supplied, the tooltip/menu/pop-up frame will be displayed
immediately, but only if point is at POINT (used internally when called
from timer)."
  (interactive)

  ;; if no overlay supplied, try to find one at point
  (unless overlay (setq overlay (completion-overlay-at-point)))

  ;; cancel any running timer so we don't end up being called twice
  (cancel-timer completion-auto-timer)

  ;; make sure things are still in a sensible state (might not be if
  ;; displaying after a delay)
  (when (and overlay
             (overlay-buffer overlay)
             (or (null point) (= (point) point)))
    ;; if delaying, setup timer to call ourselves later
    (if (and completion-auto-show-delay (null point))
        (setq completion-auto-timer
              (run-with-timer completion-auto-show-delay nil
                              'completion-auto-show
                              overlay (point)))

      ;; otherwise, display whatever we're displaying
      (cond
       ((eq completion-auto-show 'tooltip)
        (completion-show-tooltip overlay))
       ((eq completion-auto-show 'menu)
        (completion-show-menu overlay))
       ((eq completion-auto-show 'pop-up)
        (completion-popup-frame overlay)))
      )))



(defun complete-echo (overlay)
  "Display completion candidates in the echo-area."
  (let ((message-log-max nil))
    (message (completion-construct-echo-text overlay))))



(defun completion-show-tooltip (&optional overlay point)
  "Show completion tooltip for completion OVERLAY.
The point had better be within OVERLAY or you'll have bad luck
in all your flower-arranging endevours for fourteen years.

If OVERLAY is not supplied, try to find one at point.

If POINT is supplied, a tooltip will only be displayed if
point is at POINT."
  (interactive)

  ;; if no overlay was supplied, try to find one at point
  (unless overlay (setq overlay (completion-overlay-at-point)))

  (when (and overlay
             window-system (fboundp 'x-show-tip)
             (or (null point) (= (point) point)))

    (let ((mouse-pos (mouse-pixel-position))
          (pos (save-excursion
                 (goto-char (overlay-start overlay))
                 (completion-frame-posn-at-point)))
          (fg (face-attribute 'completion-tooltip-face :foreground))
          (bg (face-attribute 'completion-tooltip-face :background))
          (font (face-attribute 'completion-tooltip-face :family))
          params text text-func)

      ;; construct the tooltip text using the "overlay-local" binding
      ;; of 'tooltip-function, or `completion-tooltip-function' if
      ;; there is none, or failing that
      ;; `completion-construct-tooltip-text'
      (setq text-func
            (or (and (fboundp 'auto-overlay-local-binding)
                     (auto-overlay-local-binding
                      'completion-tooltip-function))
                completion-tooltip-function
		'completion-construct-tooltip-text))
      (setq text (funcall text-func
                          (overlay-get overlay 'prefix)
                          (overlay-get overlay 'completions)
                          (overlay-get overlay 'completion-num)))

      ;; mouse position can be nil if mouse is outside Emacs frame in
      ;; certain window systems (e.g. windows); in this case, we move
      ;; mouse into frame (there's no way to restore its position
      ;; afterwards, since we can't find out its position)
      (unless (and (numberp (cadr mouse-pos))
                   (numberp (cddr mouse-pos)))
        (set-mouse-position (selected-frame) 1 0)
        (setq mouse-pos (mouse-pixel-position)))

      ;; set face and frame parameters
      (when (stringp fg)
        (setq params
              (tooltip-set-param params 'foreground-color fg))
        (setq params (tooltip-set-param params 'border-color fg)))
      (when (stringp bg)
        (setq params
              (tooltip-set-param params 'background-color bg)))
      (when (stringp font)
        (setq params (tooltip-set-param params 'font font)))
      (setq params
            (tooltip-set-param params 'internal-border-width 0))
      (setq params
            (tooltip-set-param params 'border-width 0))
;;      (setq params
;;            (tooltip-set-param
;;             params 'left
;;             (+ (car pos) completion-tooltip-x-offset)))
;;      (setq params
;;            (tooltip-set-param
;;             params 'top
;;             (+ (cdr pos) completion-tooltip-y-offset)))

      ;; make sure tooltip is cancelled before displaying it, otherwise
      ;; x-show-tip "magically" moves it to the top of the frame!
      (completion-cancel-tooltip)
      ;; show tooltip
      ;; Note: there's no reliable way to directly display a tooltip at the
      ;; *screen* position (which is what x-show-tip requires) of point, so we
      ;; use the kludge of calculating an offset from the mouse position and
      ;; displaying the tooltip relative to the mouse
      (x-show-tip text nil params completion-tooltip-timeout
                  (+ (- (car pos) (cadr mouse-pos))
                     (car completion-tooltip-offset))
                  (+ (- (cdr pos) (cddr mouse-pos)) (frame-char-height)
                     (cdr completion-tooltip-offset)))

      ;; if enabling hotkeys only when a tooltip or pop-up frame is active,
      ;; construct hotkey bindings from `completion-hotkey-list'
      (when (eq completion-use-hotkeys 'pop-up)
	(completion-enable-hotkeys overlay)
	(completion-enable-tooltip-keys overlay))

      ;; set flag to indicate tooltip is active for this overlay (this should
      ;; tooltip-related key bindings, but doesn't when tooltip is
      ;; auto-displayed, so we also add them to overlay's keymap)
      (completion-enable-tooltip-keys overlay)
      (setq completion-tooltip-active overlay)
      )))



(defun completion-show-menu (&optional overlay menu)
  "Show completion menu for completion OVERLAY.
The point had better be within OVERLAY or you'll have a sneezing
fit.

If no OVERLAY is supplied, one is found at point (this only
happens when this function is called interactively).

If MENU is supplied, use that to construct the menu, unless an
overlay overrides it. Defaults to the \"overlay local\" binding
of 'completion-menu, or `completion-menu' if
there is none."
  (interactive)
  (completion-cancel-tooltip)

  (unless overlay (setq overlay (completion-overlay-at-point)))

  (setq menu (or menu
                 (and (fboundp 'auto-overlay-local-binding)
                      (auto-overlay-local-binding
                       'completion-menu))
                 completion-menu
		 'completion-construct-menu))

  (when overlay
    (let (keymap result)
      (cond
       ;; if `menu' is a function, evaluate it to get menu
       ((functionp menu)
        (setq keymap
	      (funcall menu
		       (overlay-get overlay 'prefix)
		       (overlay-get overlay 'completions)
		       (overlay-get overlay 'completion-function)
		       (overlay-get overlay 'completion-prefix-function)
		       (overlay-get overlay 'completion-replaces-prefix)))
        ;; throw error if return value has wrong type
        (unless (or (null keymap) (keymapp keymap))
          (error "`completion-menu' returned wrong type:null or keymapp, %s"
                 (prin1-to-string keymap))))

       ;; if `menu' is a keymap, use that
       ((keymapp menu) (setq keymap menu))

       ;; otherwise, throw an error
       (t (error "Wrong type in `completion-menu': functionp or keymapp, %s"
		 (prin1-to-string menu))))


      ;; if we've constructed a menu, display it
      (when keymap
        (setq result
              (x-popup-menu (save-excursion
                              (goto-char (overlay-start overlay))
                              (completion-posn-at-point-as-event
                               nil nil
                               (car completion-menu-offset)
                               (+ (frame-char-height) 3
                                  (cdr completion-menu-offset))))
                            keymap))

        ;; if they ain't selected nuffin', don't do nuffin'!
        (when result
          ;; convert result to a vector for key lookup
          (setq result (apply 'vector result))

          (cond
           ;; if they selected a completion from the menu...
           ((string-match "^completion-insert"
                          (symbol-name (aref result (1- (length result)))))
            ;; insert selected completion
	    (destructuring-bind (cmpl len)
		(funcall (lookup-key keymap result))
	      ;; run accept hooks
	      (run-hook-with-args
	       'completion-accept-functions
	       (overlay-get overlay 'prefix)
	       cmpl)
	      ;; delete old provisional completion, including prefix unless
	      ;; it's already been deleted
	      (delete-region
	       (- (overlay-start overlay)
		  (if (and (overlay-get overlay 'completion-replaces-prefix)
			   (overlay-get overlay 'prefix-replaced))
		      0 (overlay-get overlay 'prefix-length)))
	       (overlay-end overlay))
	      (let ((overwrite-mode nil)) (insert cmpl)))
            (completion-delete-overlay overlay))

           ;; otherwise, run whatever they did select
           (t (funcall (lookup-key keymap result))))
          )))))



(defun completion-show-browser-menu (&optional overlay menu)
  "Show completion browser menu for completion OVERLAY.
The point had better be within OVERLAY or you'll get hives.

If no OVERLAY is supplied, one is found at point (this only
happens when this function is called interactively).

If MENU is supplied, use that to construct the menu, unless an
overlay overrides it. Defaults to the \"overlay local\" binding
of 'completion-browser-menu-function, or
`completion-browser-menu-function' if there is none."
  (interactive)

  ;; this function is really just a call to `completion-show-menu' with
  ;; a different default for the menu argument
  (setq menu (or menu
                 (and (fboundp 'auto-overlay-local-binding)
                      (auto-overlay-local-binding
                       'completion-browser-menu-function))
                 completion-browser-menu-function
		 'completion-construct-browser-menu))
  (completion-show-menu overlay menu))




;;; ===============================================================
;;;                   Completion pop-up frames

(defvar completion-popup-frame-parent-frame nil
  "Stores the parent frame of a popup frame.")
(make-variable-buffer-local 'completion-popup-frame-parent-frame)


(defvar completion-popup-frame-parent-overlay nil
  "Stores the parent completion overlay of a popup frame.")
(make-variable-buffer-local 'completion-popup-frame-parent-overlay)


(defvar completion-popup-frame-overlay nil
  "Stores pop-up frame overlay used to highlight selected completion.")
(make-variable-buffer-local 'completion-popup-frame-overlay)


(defvar completion-popup-frame-show-all nil
  "Non-nil when all completions are shown in a pop-up frame.")
(make-variable-buffer-local 'completion-popup-frame-show-all)


(defun completion-popup-frame (&optional overlay)
  "Pop up a frame at point displaying the completions for OVERLAY.
The point had better be within OVERLAY or your aubergines will be
cursed for a hundred years \(that's eggplant for any Americans
out there\).

If no OVERLAY is supplied, tried to find one at point."
  (interactive)
  (completion-cancel-tooltip)

  ;; if none was supplied, find overlay at point
  (unless overlay (setq overlay (completion-overlay-at-point)))

  (when (and overlay window-system)
    (let* ((parent-frame (selected-frame))
           (prefix (overlay-get overlay 'prefix))
           (completions (overlay-get overlay 'completions))
           (num (overlay-get overlay 'completion-num))
           (popup-fun (or (if (fboundp 'auto-overlay-local-binding)
			      (auto-overlay-local-binding
			       'completion-popup-frame-function))
			  completion-popup-frame-function
			  'completion-construct-popup-frame-text))
           (lines (funcall popup-fun prefix completions))
           (maxlen (if (null lines)
                       0 (apply 'max (mapcar 'length lines))))
           (pos (save-excursion
                  (goto-char (overlay-start overlay))
                  (completion-frame-posn-at-point)))
           ;; get or create pop-up frame
           (frame
            (or (overlay-get overlay 'popup-frame)
                (make-frame
                 `((user-size . t)
                   (user-position . t)
                   (minibuffer . nil)
                   (left-fringe . 0)
                   (right-fringe . 0)
                   (menu-bar-lines . nil)
                   (tool-bar-lines . nil)
                   (unsplittable . t)
                   (cursor-type . nil)
                   (border-width . 0))))))

      ;; initialise pop-up frame
      (overlay-put overlay 'popup-frame frame)
      (set-frame-size
       frame
       (1+ maxlen)
       (1+ (min (length completions)
                completion-popup-frame-max-height)))
      (set-frame-position
       frame
       (+ (car pos) (car completion-popup-frame-offset))
       (+ (cdr pos) (cdr completion-popup-frame-offset)))
      (select-frame-set-input-focus frame)
      (switch-to-buffer " *completion-ui*")
      (completion-popup-frame-mode)
      (setq completion-popup-frame-parent-frame parent-frame)
      (setq completion-popup-frame-parent-overlay overlay)

      ;; insert completions
      (erase-buffer)
      (mapc (lambda (str) (insert str "\n")) lines)
      (backward-delete-char 1)

      ;; highlight current completion
      (goto-char (point-min))
      (when num (forward-line num))
      (let ((pos (point)))
        (end-of-line)
        (unless (overlayp completion-popup-frame-overlay)
          (setq completion-popup-frame-overlay
                (make-overlay pos (point)))
          (overlay-put completion-popup-frame-overlay
                       'face 'completion-dynamic-face))
        (move-overlay completion-popup-frame-overlay pos (point))))

    ;; if enabling hotkeys only when a tooltip or pop-up frame is active,
    ;; construct hotkey bindings from `completion-hotkey-list'
    (when (eq completion-use-hotkeys 'pop-up)
      (completion-enable-hotkeys overlay))
    ))



;; The major mode function
(defun completion-popup-frame-mode ()
  "Major mode used in completion-UI pop-up frames."
  (kill-all-local-variables)
  (setq major-mode 'completion-popup-frame-mode)
  (use-local-map completion-popup-frame-mode-map)
  (setq mode-line-format nil))

;; indicate mode is only appropriate in special circumstances
(put 'completion-popup-frame-mode 'mode-class 'special)



(defun completion-popup-frame-motion (command &optional arg)
  "Call COMMAND to move point, then select completion at point,
Selecting the completion inserts it in the pop-up frame's parent
buffer and highlights it in the pop-up frame.

If ARG is supplied, it is passed through to COMMAND."

  ;; call COMMAND with ARG
  (funcall command arg)

  ;; highlight selected completion
  (forward-line 0)
  (let ((pos (point)))
    (end-of-line)
    (move-overlay completion-popup-frame-overlay pos (point)))

  ;; insert selected completion in parent buffer
  (let* ((num (line-number-at-pos))
	 (overlay completion-popup-frame-parent-overlay)
	 (prefix (overlay-get overlay 'prefix))
	 (cmpl-replaces-prefix
	  (overlay-get overlay 'completion-replaces-prefix))
	 (frame (selected-frame))
	 (pos (make-marker))
	 cmpl len)
    (save-excursion
      (set-buffer (overlay-buffer overlay))
      (setq cmpl (nth (1- num) (overlay-get overlay 'completions))
	    len  (length prefix))
      (unless (stringp cmpl)
	(setq len  (cdr cmpl)
	      cmpl (car cmpl)))
      (move-marker pos (overlay-start overlay))
      (set-marker-insertion-type pos nil)
      (delete-region
       (- (overlay-start overlay)
	  (if (and cmpl-replaces-prefix
		   (or (not (eq completion-resolve-behaviour 'accept))
		       (overlay-get overlay 'prefix-replaced)))
	      0 (overlay-get overlay 'prefix-length)))
       (overlay-end overlay))
      ;; insert new completion
      (let ((overwrite-mode nil)) (insert cmpl))
      (move-overlay overlay
		    (+ pos (if cmpl-replaces-prefix 0 len))
		    (+ pos (length cmpl)))
      (overlay-put overlay 'prefix-length len)
      (overlay-put overlay 'completion-num (1- num))
      ;; highlight alterations to prefix, if enabled
      (when (and completion-dynamic-highlight-prefix-alterations
		 (not cmpl-replaces-prefix))
	(completion-highlight-prefix-alterations prefix cmpl pos len))
      ;; highlight common substring, if enabled
      (when completion-dynamic-highlight-common-substring
	(completion-highlight-common-substring prefix cmpl pos len overlay)))

    ;; delete temporary marker
    (set-marker pos nil)
    ;; move point to appropriate position in the overlay
    (select-frame completion-popup-frame-parent-frame)
    (completion-position-point-in-overlay overlay)
    (select-frame-set-input-focus frame)))



(defun completion-popup-frame-dismiss ()
  "Delete current pop-up frame."
  (interactive)
  ;; reset overlay
  (overlay-put completion-popup-frame-parent-overlay
               'popup-frame nil)
  ;; if showing all completions, revert to showing just the first few
  (when completion-popup-frame-show-all
    (let ((prefix
           (overlay-get completion-popup-frame-parent-overlay 'prefix))
          (cmpl-fun (overlay-get completion-popup-frame-parent-overlay
				 'completion-function))
	  completions)
      (save-excursion
	(set-buffer (overlay-buffer completion-popup-frame-parent-overlay))
	(setq completions
	      (funcall cmpl-fun prefix completion-max-candidates)))
      (overlay-put completion-popup-frame-parent-overlay
                   'completions completions)))
  ;; disable hotkeys if only enabled when a tooltip or pop-up frame is active
  (when (eq completion-use-hotkeys 'pop-up)
    (completion-disable-hotkeys completion-popup-frame-parent-overlay))
  ;; delete pop-up frame
  (let ((frame (selected-frame)))
    (select-frame completion-popup-frame-parent-frame)
    (delete-frame frame)))



(defun completion-popup-frame-unread-key ()
  "Unread last key sequence, then kill popup frame.
The focus is returned to the parent buffer, which will then
receive the unread key sequence."
  (interactive)
  (setq unread-command-events (listify-key-sequence (this-command-keys)))
  (select-frame completion-popup-frame-parent-frame))



(defun completion-popup-frame-toggle-show-all ()
  "Toggle between showing some completions and all completions.
Initially, only the first `completion-max-candidates' completions
are shown in a pop-up frame, as with all the other completion
methods. Toggling will show all possible completions."
  (interactive)

  (let ((prefix (overlay-get completion-popup-frame-parent-overlay
                             'prefix))
	(cmpl-fun (overlay-get completion-popup-frame-parent-overlay
			       'completion-function))
	completions lines maxlen)

    (cond
     ;; if we weren't showing all completions, get all completions and
     ;; update completion overlay properties
     ((null completion-popup-frame-show-all)
      (message
       "Finding all completions (C-g to cancel if taking too long)...")
      (save-excursion
        (set-buffer (overlay-buffer completion-popup-frame-parent-overlay))
        (setq completions (funcall cmpl-fun prefix)))
      (overlay-put completion-popup-frame-parent-overlay
                   'completions completions))

     ;; if we were showing all completions, get list of best completions and
     ;; update completion overlay properties
     (completion-popup-frame-show-all
      (save-excursion
        (set-buffer (overlay-buffer completion-popup-frame-parent-overlay))
        (setq completions
	      (funcall cmpl-fun prefix completion-max-candidates)))
      (overlay-put completion-popup-frame-parent-overlay
                   'completions completions)))

    ;; reset pop-up frame properties
    (erase-buffer)
    (setq lines
          (completion-construct-popup-frame-text prefix completions))
    (setq maxlen (if (null lines) 0 (apply 'max (mapcar 'length lines))))
    (set-frame-size (selected-frame) (1+ maxlen) (frame-height))
    ;; insert completions in pop-up frame
    (mapc (lambda (str) (insert str "\n")) lines)
    (delete-backward-char 1)
    ;; highlight first completion
    (goto-char (point-min))
    (let ((pos (point)))
      (end-of-line)
      (move-overlay completion-popup-frame-overlay pos (point)))
    ;; toggle flag
    (setq completion-popup-frame-show-all
          (not completion-popup-frame-show-all))))



;;; ===============================================================
;;;                Commands for binding to keys

(defun completion-self-insert ()
  "Deal with completion-related stuff, then insert last input event."
  (interactive)
  ;; FIXME: whether to keep or delete provisional completion should
  ;;        depend on point's location relative to it

  ;; if we're auto-completing, hand over to `auto-completion-self-insert'
  (if auto-completion-mode
      (auto-completion-self-insert)
    ;; otherwise, resolve old and current completions, and insert last input
    ;; event
    (let ((overlay (completion-overlay-at-point)))
      (completion-resolve-old overlay)
      (completion-resolve-current overlay))
    (self-insert-command 1)))



(defun auto-completion-self-insert (&optional char syntax
                                              no-syntax-override)
  "Execute a completion function based on syntax of the character
to be inserted.

Decide what completion function to execute by looking up the
syntax of the character corresponding to the last input event in
`auto-completion-syntax-alist'. The syntax-derived function can
be overridden for individual characters by
`auto-completion-override-syntax-alist'.

If CHAR is supplied, it is used instead of the last input event
to determine the character typed. If SYNTAX is supplied, it
overrides the character's syntax, and is used instead to lookup
the behaviour in the alists. If NO-SYNTAX-OVERRIDE is non-nil,
the behaviour is determined only by syntax, even if it is
overridden for the character in question
\(i.e. `auto-completion-override-syntax-alist' is ignored\).

The default actions in `completion-dymamic-syntax-alist' all
insert the last input event, in addition to taking any completion
related action \(hence the name,
`auto-completion-self-insert'\). Therefore, unless you know what
you are doing, only bind `auto-completion-self-insert' to
printable characters.

The Emacs `self-insert-command' is remapped to this when
`completion-function' is set."
  (interactive)
  (completion-cancel-tooltip)

  ;; if CHAR or SYNTAX were supplied, use them; otherwise get character
  ;; and syntax from last input event (which relies on sensible key
  ;; bindings being used for this command)
  (when (null char) (setq char last-input-event))
  (when (null syntax) (setq syntax (char-syntax last-input-event)))

  (let* ((behaviour (if no-syntax-override
			(completion-lookup-behaviour nil syntax)
		      (completion-lookup-behaviour char syntax)))
	 (complete-behaviour
	  (completion-get-completion-behaviour behaviour))
	 (resolve-behaviour
	  (completion-get-resolve-behaviour behaviour))
	 (insert-behaviour
	  (completion-get-insertion-behaviour behaviour))
	 (overlay (completion-overlay-at-point))
	 wordstart prefix)

    ;; ----- resolve behaviour -----
    (completion-resolve-old overlay)
    ;; if behaviour alist entry is a function, call it
    (when (functionp resolve-behaviour)
      (setq resolve-behaviour (funcall resolve-behaviour)))

    ;; do whatever action was specified in alists
    (cond
     ;; no-op
     ((null resolve-behaviour))

     ;; accept
     ((eq resolve-behaviour 'accept)
      (setq prefix (string char))
      (setq wordstart t)
      ;; if there is a completion at point...
      (when overlay
	;; if point is not at start of overlay, delete overlay (effectively
	;; accepting old completion but without running hooks)
	(if (/= (point) (overlay-start overlay))
	    (completion-delete-overlay overlay)
	  ;; otherwise, accept completion
	  (completion-accept nil overlay))))

     ;; reject
     ((eq resolve-behaviour 'reject)
      (setq prefix (string char))
      (setq wordstart t)
      ;; if there is a completion at point...
      (when overlay
	;; if point is not at start of overlay, delete overlay (effectively
	;; accepting old completion without running hooks)
	(if (/= (point) (overlay-start overlay))
	    (completion-delete-overlay overlay)
	  ;; otherwise, reject completion
	  (completion-reject nil overlay))))

     ;; add to prefix
     ((eq resolve-behaviour 'add)
      ;; if we're at the start of a word, prevent adjacent word from being
      ;; deleted below if `completion-overwrite' is non-nil
      (when (completion-beginning-of-word-p) (setq wordstart t))
      ;; if point is within a completion overlay...
      (when overlay
	;; if point is not at start of overlay, delete overlay (effectively
	;; accepting the old completion) and behave as if no completion was in
	;; progress
	(if (/= (point) (overlay-start overlay))
	    (completion-delete-overlay overlay)
	  ;; otherwise, delete old completion, restore prefix if
	  ;; `completion-replaces-prefix', and add character to prefix
	  (delete-region (overlay-start overlay) (overlay-end overlay))
	  (when (overlay-get overlay 'completion-replaces-prefix)
	    (let ((overwrite-mode nil))
	      (insert (overlay-get overlay 'prefix)))
	    (overlay-put overlay 'prefix-replaced nil))
	  (setq prefix (concat (overlay-get overlay 'prefix)
			       (string char)))
	  ;; prevent any adjacent word from being deleted
	  (setq wordstart t))
	))

     ;; error
     (t (error "Invalid entry in `auto-completion-syntax-alist'\
  or `auto-completion-override-syntax-alist', %s"
	       (prin1-to-string resolve-behaviour))))


    ;; ----- insersion behaviour -----
    ;; if behaviour alist entry is a function, call it
    (when (functionp insert-behaviour)
      (setq insert-behaviour (funcall insert-behaviour)))

    ;; if we're inserting...
    (when insert-behaviour
      ;; use `self-insert-command' if possible, since `auto-fill-mode' depends
      ;; on it
      (if (eq char last-input-event)
	  (self-insert-command 1)
	(insert char))
      (when (and overlay (overlay-buffer overlay))
	(move-overlay overlay (point) (point))))


    ;; ----- completion behaviour -----
    ;; if behaviour alist entry is a function, call it
    (when (functionp complete-behaviour)
      (setq complete-behaviour (funcall complete-behaviour)))

    (cond
     ;; no-op
     ((null complete-behaviour))

     ;; if not completing, clear up any overlay left lying around
     ((eq complete-behaviour 'none)
      (when overlay
	(when (overlay-get overlay 'popup-frame)
	  (delete-frame (overlay-get overlay 'popup-frame)))
	(completion-delete-overlay overlay)))

     ;; if completing...
     ((or (eq complete-behaviour 'string)
	  (eq complete-behaviour 'word))
      ;; if point is in middle of a word, `completion-overwrite' is set, and
      ;; overwriting hasn't been disabled, delete rest of word prior to
      ;; completing
      (when (and completion-overwrite (completion-within-word-p)
		 (null wordstart))
	(let ((pos (point))
	      (word-thing
	       (if (fboundp 'auto-overlay-local-binding)
		   (auto-overlay-local-binding 'completion-word-thing)
		 completion-word-thing
		 'word)))
	  (save-excursion
	    (forward-thing word-thing)
	    (delete-region pos (point)))))

      (cond
       ;; if a prefix has been set, setup overlay with the prefix, and do
       ;; completion
       (prefix
	(completion-setup-overlay
	 prefix
	 (when overlay (1+ (overlay-get overlay 'prefix-length)))
	 nil nil nil nil nil overlay)
	(complete-in-buffer nil nil nil nil 'auto))

       ;; if doing basic completion, let prefix be found normally
       ((eq complete-behaviour 'string)
	(complete-in-buffer nil nil nil nil 'auto))

       ;; if completing word at point, delete any overlay at point to ensure
       ;; prefix is found anew, and do completion
       (t
	(when (setq overlay (completion-overlay-at-point))
	  (completion-delete-overlay overlay))
	(complete-in-buffer nil nil nil nil 'auto))))

     ;; error
     (t (error "Invalid entry in `auto-completion-syntax-alist'\
 or `auto-completion-override-syntax-alist', %s"
	       (prin1-to-string complete-behaviour))))
    ))



(defun complete-word-at-point
  (&optional cmpl-function cmpl-prefix-function cmpl-replaces-prefix)
  "Complete the word at or next to point.

CMPL-FUNCTION, CMPL-PREFIX-FUNCTION and CMPL-REPLACES-PREFIX
specify the `completion-function' and
`completion-prefix-function' to use, and whether
`completion-replaces-prefix'. If they are not specified, the
global values of these variables are used. See the global
variables for an explanation of their formats."
  (interactive)

  ;; get completion overlay at point
  (let ((overlay (completion-overlay-at-point)))

    ;; if point is at start of an existing overlay, delete old completion
    ;; before completing, preserving overlay so its prefix can be reused
    (if (and overlay (= (point) (overlay-start overlay)))
        (delete-region (overlay-start overlay) (overlay-end overlay))

      ;; if there's a completion at point but point is not at start,
      ;; delete overlay (effectively accepting old completion) and behave
      ;; as if no completion was in progress
      (when overlay (completion-delete-overlay overlay))

      ;; if point is in middle of a word and `completion-overwrite' is
      ;; enabled, delete rest of word before completing
      (when (and completion-overwrite (completion-within-word-p))
        (let ((pos (point)))
          (save-excursion
            (forward-thing completion-word-thing)
            (delete-region pos (point))))
        ;; if there is now a completion overlay at point, delete it
        (when (setq overlay (completion-overlay-at-point))
          (completion-delete-overlay overlay)))))

  ;; do completion
  (complete-in-buffer nil cmpl-function cmpl-prefix-function
		      cmpl-replaces-prefix))



(defun complete-or-cycle-word-at-point
  (&optional n cmpl-function cmpl-prefix-function cmpl-replaces-prefix)
  "Cycle through available completions if there are any,
otherwise complete the word at point.

CMPL-FUNCTION, CMPL-PREFIX-FUNCTION and CMPL-REPLACES-PREFIX
specify the `completion-function' and
`completion-prefix-function' to use, and whether
`completion-replaces-prefix'. If they are not specified, the
global values of these variables are used. See the global
variables for an explanation of their formats."
  (interactive "p")
  (if (completion-overlay-at-point)
      (completion-cycle n)
    (complete-word-at-point cmpl-function cmpl-prefix-function
			    cmpl-replaces-prefix)))


(defun complete-or-cycle-backwards-word-at-point
  (&optional n cmpl-function cmpl-prefix-function cmpl-replaces-prefix)
  "Cycle backwards through available completions if there are any,
otherwise complete the word at point.

CMPL-FUNCTION, CMPL-PREFIX-FUNCTION and CMPL-REPLACES-PREFIX
specify the `completion-function' and
`completion-prefix-function' to use, and whether
`completion-replaces-prefix'. If they are not specified, the
global values of these variables are used. See the global
variables for an explanation of their formats."
  (interactive "p")
  (if (completion-overlay-at-point)
      (completion-cycle (- n))
    (complete-word-at-point cmpl-function cmpl-prefix-function
			    cmpl-replaces-prefix)))



(defun completion-select (&optional n overlay)
  "Select completion corresponding to the last input event
when hotkey completion is active.

If integer N is supplied, insert completion corresponding to that
instead. If OVERLAY is supplied, use that instead of finding one
at point. The point had better be within OVERLAY or a meteorite
will smash through your ceiling.

Characters in `completion-hotkey-list' get bound to this
internally. It should *never* be bound in a keymap."
  (interactive)
  (completion-cancel-tooltip)

  (unless overlay (setq overlay (completion-overlay-at-point)))

  ;; get last input event by unreading and re-reading it, to ensure key
  ;; sequence translation can take place
  (let (key)
    (setq unread-command-events (listify-key-sequence (this-command-keys)))
    (setq key (read-key-sequence-vector ""))
    ;; find completion index corresponding to last input event
    (unless n
      ;; FIXME: work around apparent bug where keys are doubled in vector
      (when (> (length key) 1) (setq key (vector (aref key 0))))
      (setq n (completion--position
               key (mapcar 'vector completion-hotkey-list))))

    ;; if within a completion overlay...
    (when overlay
      (let ((completions (overlay-get overlay 'completions))
	    (cmpl-replaces-prefix
	     (overlay-get overlay 'completion-replaces-prefix))
	    cmpl len)
	(cond
	 ;; if there are no completions, run whatever would otherwise be
	 ;; bound to the key
	 ((null completions)
	  (when completion-trap-recursion
	    (error "Recursive call to `completion-select'"))
	  (completion-disable-hotkeys overlay)
	  (let ((completion-trap-recursion t))
	    (unwind-protect
		(command-execute (key-binding key t))
	      (completion-enable-hotkeys overlay))))

	 ;; if there are too few completions, display message
	 ;; FIXME: could just not enable these keys, but maybe that would be
	 ;;        confusing to the user?
	 ((>= n (length completions))
	  (beep)
	  (if (= (length completions) 1)
	      (message "Only 1 completion available")
	    (message "Only %d completions available"
		     (length (overlay-get overlay 'completions)))))

	 ;; otherwise, replace dynamic completion with selected one
	 (t
	  ;; delete old provisional completion, including prefix unless
	  ;; it's already been deleted
	  (setq cmpl (nth n completions)
		len (length (overlay-get overlay 'prefix)))
	  (unless (stringp cmpl)
	    (setq len (cdr cmpl)
		  cmpl (car cmpl)))
	  (delete-region
	   (- (overlay-start overlay)
	      (if (and cmpl-replaces-prefix
		       (overlay-get overlay 'prefix-replaced))
		  0 (overlay-get overlay 'prefix-length)))
	   (overlay-end overlay))
	  (let ((overwrite-mode nil)) (insert cmpl))
	  ;; run accept hooks
	  (run-hook-with-args 'completion-accept-functions
			      (overlay-get overlay 'prefix)
			      cmpl)
	  ;; delete overlay
	  (completion-delete-overlay overlay)))))
    ))


(defun completion-select-if-within-overlay ()
  "Select a completion to insert if there is one, otherwise run
whatever command would normally be bound to the key sequence used
to invoke this command."
  (interactive)
  (completion-run-if-within-overlay 'completion-select
                                    'completion-use-hotkeys))



(defun completion-accept (&optional arg overlay)
  "Accept current provisional completion.

The value of ARG is passed as the third argument to any functions
called from the `completion-accept-functions' hook. Interactively,
ARG is the prefix argument.

If optional argument OVERLAY is supplied, it is used instead of
looking for an overlay at the point. The point had better be
within OVERLAY or else your hair will fall out.

If a completion was accepted, returns a cons cell containing the
prefix and the entire accepted completion \(the concatenation of
the prefix and the completion string\). Otherwise returns nil."
  (interactive "P")

  ;; if we haven't been passed one, get completion overlay at point
  (unless overlay (setq overlay (completion-overlay-at-point)))

  (let (prefix cmpl len frame)
    ;; resolve any other old provisional completions
    (completion-resolve-old overlay)
    (completion-cancel-tooltip)

    (cond
     ;; if there are no completions, just delete the overlay
     ((and overlay (null (overlay-get overlay 'completions)))
      (completion-delete-overlay overlay))

     ;; if there are completions...
     ((and overlay (overlay-get overlay 'completions))
      (setq prefix (overlay-get overlay 'prefix))
      (setq cmpl (nth (overlay-get overlay 'completion-num)
		      (overlay-get overlay 'completions))
	    len (length (overlay-get overlay 'prefix)))
      (unless (stringp cmpl)
	(setq len (cdr cmpl)
	      cmpl (car cmpl)))
      ;; delete prefix + provisional completion if there is a completion to
      ;; accept, and prefix hasn't already been replaced with completion (in
      ;; that case, deleting the overlay, below, is sufficient)
      (if (and (overlay-get overlay 'completion-replaces-prefix)
	       (overlay-get overlay 'prefix-replaced))
	  (goto-char (overlay-end overlay))
        (delete-region (- (overlay-start overlay)
			  (overlay-get overlay 'prefix-length))
                       (overlay-end overlay))
	;; accept current completion
	(let ((overwrite-mode nil)) (insert cmpl)))
      ;; run accept hooks
      (run-hook-with-args 'completion-accept-functions
                          prefix cmpl arg)
      ;; delete any pop-up frame
      (when (setq frame (overlay-get overlay 'popup-frame))
        (delete-frame frame))
      ;; delete overlay
      (completion-delete-overlay overlay)
      (cons prefix cmpl)))))



(defun completion-reject (&optional arg overlay)
  "Reject current provisional completion.

The value of ARG is passed as the third argument to any functions
called from the `completion-reject-functions' hook. Interactively,
ARG is the prefix argument.

If optional argument OVERLAY is supplied, it is used instead of
looking for an overlay at the point. The point had better be
within OVERLAY or else your hair will fall out.

If a completion was rejected, returns a cons cell containing the
prefix and the entire rejected completion \(the concatenation of
the prefix and the completion string\). Otherwise returns nil."
  (interactive "P")

  ;; if we haven't been passed one, get completion overlay at point
  (unless overlay (setq overlay (completion-overlay-at-point)))

  (let (prefix completion cmpl-function frame)
    ;; resolve any other old provisional completions
    (completion-resolve-old overlay)
    (completion-cancel-tooltip)

    ;; if point is in a completion overlay...
    (when overlay
      (setq prefix (overlay-get overlay 'prefix))
      (setq completion (nth (overlay-get overlay 'completion-num)
			    (overlay-get overlay 'completions)))
      ;; reject current completion
      (delete-region
       (- (overlay-start overlay)
	  (if (and (overlay-get overlay 'completion-replaces-prefix)
		   (eq completion-resolve-behaviour 'accept)
		   (overlay-get overlay 'prefix-replaced))
	      0 (overlay-get overlay 'prefix-length)))
       (overlay-end overlay))
      ;; restore original prefix
      (insert (overlay-get overlay 'prefix))
      ;; run reject hooks
      (run-hook-with-args 'completion-reject-functions
                          prefix completion arg)
      ;; delete any pop-up frame
      (when (setq frame (overlay-get overlay 'popup-frame))
        (delete-frame frame))
      ;; delete overlay
      (completion-delete-overlay overlay)
      ;; return cons cell containing prefix and rejected completion
      (cons prefix completion))))



(defun completion-resolve-then-run-command ()
 "Resolve current completion, then run command
that would normally be bound to this key."
 (interactive)
 (completion-run-if-within-overlay
  (lambda () (interactive) (completion-resolve-current nil nil ? ))
  'completion-function 'before))



(defun completion-scoot-ahead (&optional overlay)
  "Accept the characters from the current completion,
and recomplete the resulting string.

When called from Lisp programs, use OVERLAY instead of finding
one. The point had better be within OVERLAY or the oceans will
boil away."
  (interactive)
  (completion-cancel-tooltip)

  (unless overlay (setq overlay (completion-overlay-at-point)))

  ;; if within a completion overlay, accept characters it contains
  (when (and overlay (/= (point) (overlay-end overlay)))
    (goto-char (overlay-end overlay))
    (move-overlay overlay (point) (point))
    (let ((cmpl (nth (overlay-get overlay 'completion-num)
		     (overlay-get overlay 'completions))))
      (completion-setup-overlay
       (if (stringp cmpl) cmpl (car cmpl))
       nil nil nil nil nil nil overlay)))

  ;; if auto-completing, do so
  (if auto-completion-mode
      (complete-in-buffer nil nil nil nil 'auto)
    ;; otherwise, if a pop-up frame is being displayed, update it
    (when (overlay-get overlay 'popup-frame)
      (completion-popup-frame overlay))))



(defun completion-cycle (&optional n overlay no-auto)
  "Cycle through available completions.

Optional argument N specifies the number of completions to cycle
forwards \(backwards if negative\). Default is 1. Interactively,
N is the prefix argument.

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or you'll be stuck by
lightening.

If NO-AUTO is non-nil, the tooltip/menu/pop-up frame will *not*
be auto-displayed."
  (interactive "P")
  (when (null n) (setq n 1))

  ;; if we haven't been passed one, get completion overlay at point
  (unless overlay (setq overlay (completion-overlay-at-point)))

  ;; if within a completion overlay which has completions, cycle through them
  (when (and overlay (overlay-get overlay 'completions))
    (let ((completions (overlay-get overlay 'completions))
	  (prefix (overlay-get overlay 'prefix))
	  (cmpl-replaces-prefix
	   (overlay-get overlay 'completion-replaces-prefix))
	  (pos (make-marker))
	  i cmpl len)
      (setq i (mod (+ (or (overlay-get overlay 'completion-num) -1) n)
		   (length completions))
	    cmpl (nth i completions)
	    len (length prefix))
      (unless (stringp cmpl)
	(setq len (cdr cmpl)
	      cmpl (car cmpl)))
      ;; delete old completion, including prefix unless it's already been
      ;; deleted
      (move-marker pos (overlay-start overlay))
      (set-marker-insertion-type pos nil)
      (delete-region
       (- (overlay-start overlay)
	  (if (and cmpl-replaces-prefix
		   (or (not (eq completion-resolve-behaviour 'accept))
		       (overlay-get overlay 'prefix-replaced)))
	      0 (overlay-get overlay 'prefix-length)))
       (overlay-end overlay))
      ;; insert new completion
      (let ((overwrite-mode nil)) (insert cmpl))
      (move-overlay overlay
		    (+ pos (if cmpl-replaces-prefix 0 len))
		    (+ pos (length cmpl)))
      (overlay-put overlay 'prefix-length len)
      (overlay-put overlay 'completion-num i)
      ;; highlight alterations to prefix, if enabled
      (when (and completion-dynamic-highlight-prefix-alterations
		 (not cmpl-replaces-prefix))
	(completion-highlight-prefix-alterations prefix cmpl pos len))
      ;; highlight longest common substring, if enabled
      (when completion-dynamic-highlight-common-substring
	(completion-highlight-common-substring prefix cmpl pos len overlay))
      ;; move point to appropriate position in the overlay
      (completion-position-point-in-overlay overlay)
      ;; display echo text if using it
      (when completion-use-echo (complete-echo overlay))
      ;; if pop-up frame is displayed, update it
      (if (overlay-get overlay 'popup-frame)
          (completion-popup-frame overlay)
        ;; otherwise, display tooltip/menu/pop-up frame if using them and
        ;; tooltip isn't already active
        (when (and completion-auto-show (null no-auto))
          (completion-auto-show overlay)))

      ;; delete temporary marker
      (set-marker pos nil))))



(defun completion-tooltip-cycle (&optional n overlay)
  "Cycle forwards through N completions and redisplay the tooltip.
A negative argument cycles backwards.

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or you'll be attacked by a mad
cow."
  (interactive)
  (completion-cycle n overlay t)
  (completion-show-tooltip))


(defun completion-tooltip-cycle-backwards (&optional n overlay)
  "Cycle backwards through N completions and redisplay the tooltip.
A negative argument cycles backwards.

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or you'll be attacked by a mad
sheep."
  (interactive)
  (completion-cycle (if n (- n) -1) overlay t)
  (completion-show-tooltip))


(defun completion-tab-complete (&optional overlay)
  "Tab-complete completion at point
\(i.e. insert longest common prefix of all the completions\).

If OVERLAY is supplied, use that instead of finding one. The
point had better be within OVERLAY or your teeth will turn bright
green over night."
  (interactive)
  (completion-cancel-tooltip)

  (unless overlay (setq overlay (completion-overlay-at-point)))

  ;; if within a completion overlay
  (when overlay
    (let* ((prefix (overlay-get overlay 'prefix))
	   (completions (overlay-get overlay 'completions))
	   (str (try-completion
		 "" (mapcar
		     (lambda (cmpl)
		       (if (stringp cmpl)
			   (substring cmpl (length prefix))
			 (substring (car cmpl) (cdr cmpl))))
		     completions))))
      ;; try-completion returns t if there's only one completion
      (when (eq str t)
	(setq str (car (overlay-get overlay 'completions)))
	(unless (stringp str) (setq str (car str))))

      ;; do tab-completion
      (unless (or (null str) (string= str ""))
        (delete-region (overlay-start overlay) (overlay-end overlay))
        (let ((overwrite-mode nil)) (insert str))
        (move-overlay overlay (point) (point))
        (overlay-put overlay 'prefix (concat prefix str))
	(overlay-put overlay 'prefix-length
		     (length (overlay-get overlay 'prefix)))
        (overlay-put overlay 'completions nil)
        ;; when auto-completing, do so
        (if auto-completion-mode
            (complete-in-buffer nil nil nil nil 'auto)
          ;; otherwise, if a pop-up frame is being displayed, update it
          (when (overlay-get overlay 'popup-frame)
            (completion-popup-frame overlay))))
      )))



(defun completion-backward-delete (command &rest args)
  "Run backward-delete COMMAND, passing it ARGS.
Any provisional completion at the point is first rejected. If
COMMAND deletes into a word and auto-completion is enabled,
complete what remains of that word."

  ;; start by cancelling any tooltip that's stil hanging around
  (completion-cancel-tooltip)

  (let* ((overlay (completion-overlay-at-point))
         (wordstart (completion-beginning-of-word-p))
         (word-thing (if (fboundp 'auto-overlay-local-binding)
                         (auto-overlay-local-binding
                          'completion-word-thing)
                       completion-word-thing
		       'word))
         (popup (when overlay (overlay-get overlay 'popup-frame)))
         (word-pos (save-excursion
                     (forward-thing word-thing -1) (point))))

    ;(combine-after-change-calls

      ;; restore original prefix
      (when (and overlay
		 (null 	(overlay-get overlay 'completion-replaces-prefix)))
	(goto-char (- (overlay-start overlay)
		      (overlay-get overlay 'prefix-length)))
	(delete-region (point) (overlay-start overlay))
	(let ((overwrite-mode nil))
	  (insert (overlay-get overlay 'prefix)))
	(move-overlay overlay (point) (overlay-end overlay)))


      ;; ----- not auto-completing -----
      (if (not auto-completion-mode)
          (progn
            ;; if within a completion...
            (when overlay
              ;; if rejecting old completions, delete everything after
              ;; the point
              (when (eq completion-resolve-behaviour 'reject)
                (delete-region (point) (overlay-end overlay)))
	      ;; delete overlay, effectively accepting (rest of) the
              ;; completion at point
              (completion-delete-overlay overlay))
            ;; resolve old provisional completions and delete backwards
            (completion-resolve-old)
            (apply command args))


        ;; ----- auto-completing -----
        ;; resolve any old provisional completions
        (completion-resolve-old overlay)

        ;; if point is in a completion...
        (when overlay
          ;; if point is at start of completion, delete completion but
          ;; keep overlay
          (if (= (point) (overlay-start overlay))
	      (progn
		(delete-region (overlay-start overlay) (overlay-end overlay))
		;; restore prefix if `completion-replaces-prefix' and it has
		;; indeed been replaced
		(when (and (overlay-get overlay 'completion-replaces-prefix)
			   (overlay-get overlay 'prefix-replaced))
		  (let ((overwrite-mode nil))
		    (insert (overlay-get overlay 'prefix)))
		  (move-overlay overlay (point) (point))
		  (overlay-put overlay 'prefix-replaced nil)))
            ;; otherwise, delete provisional completion characters after
            ;; point, then delete the overlay, effectively accepting
            ;; (rest of) completion, preserving pop-up frame
            (delete-region (point) (overlay-end overlay))
            (completion-delete-overlay overlay t)
            (setq overlay nil)))

        ;; delete backwards
        (apply command args)

        (cond
         ;; if we're not in or at the end of a word...
         ((and (not (completion-within-word-p))
               (not (completion-end-of-word-p)))
          ;; delete any overlay and pop-up frame at point
          (when overlay (completion-delete-overlay overlay))
          ;; cancel any timer that's been set up
          (when (timerp completion-backward-delete-timer)
            (cancel-timer completion-backward-delete-timer))
          (setq completion-backward-delete-timer nil))


         ;; otherwise, we're in or at the end of a word, so complete the
         ;; word at point
         (t
          ;; if point was at start of completion or start of word before
          ;; deleting, and we're now within or at end of a word...
          (when (or overlay
                    (and wordstart
                         (or (completion-within-word-p)
                             (completion-end-of-word-p))))
            ;; delete any overlay, since prefix is wrong and we need it
            ;; out the way so that we can get new one below
            (when overlay
              (completion-delete-overlay
               overlay (not (and popup (<= (point) word-pos)))))
            ;; setup overlay to prevent word after point being deleted
            (let ((pos (point)) prefix prefix-fun)
              (setq prefix-fun
                    (or (and (fboundp 'auto-overlay-local-binding)
                             (auto-overlay-local-binding
                              'completion-prefix))
                        completion-prefix-function
	      		'completion-prefix))
              (setq prefix (funcall prefix-fun))
              (setq overlay (completion-setup-overlay prefix))
              (move-overlay overlay (point) (point))
              ;; if we've not deleted beyond start of word, and a pop-up
              ;; frame was being displayed, make sure it's updated when
              ;; completing
              (when (and popup (> (point) word-pos))
                (overlay-put overlay 'popup-frame popup))))

          ;; if there's no existing timer, set one up to complete
          ;; remainder of word after some idle time
          (when (timerp completion-backward-delete-timer)
            (cancel-timer completion-backward-delete-timer))
          (if auto-completion-backward-delete-delay
              (setq completion-backward-delete-timer
                    (run-with-idle-timer
                     auto-completion-backward-delete-delay nil
                     ;; FIXME: tooltip key-bindings don't work - why?
                     `(lambda ()
                        (setq completion-backward-delete-timer nil)
                        (complete-in-buffer nil nil nil nil
					    'auto ,(point)))))
            ;; if completing with no delay, do so
            (complete-in-buffer nil nil nil nil 'auto (point)))
          ))));)
  )



(defun completion-delete (command &rest args)
  "Call forward-delete COMMAND, passing it ARGS.
If there is a provisional completion at point after deleting, reject
it."
  ;; start by cancelling any tooltip that's stil hanging around
  (completion-cancel-tooltip)
  ;; call the deletion command
  (apply command args)
  ;; if there's a completion overlay at point after deleting, reject it
  (let ((overlay (completion-overlay-at-point)))
    (when overlay
      (delete-region (overlay-start overlay) (overlay-end overlay))
      (completion-delete-overlay overlay)))
  (completion-resolve-old))



(defun completion-delete-char (n &optional killflag)
  "Delete the following N characters (previous if N is negative).
If there is a provisional completion at point after deleting,
reject it.  \(If N is negative, behaviour is instead as for
`completion-backward-delete-char'.\)

Non-nil optional second arg KILLFLAG means kill instead (save in
kill ring). Interactively, N is the prefix arg (default 1), and
KILLFLAG is set if n was explicitly specified."
  (interactive "P")
  (when (and (interactive-p) n) (setq killflag t))
  (setq n (prefix-numeric-value n))

  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-delete-char (- n) killflag)
    (completion-delete 'delete-char n killflag)))



(defun completion-backward-delete-char (n &optional killflag)
  "Delete the previous N characters (following if N is negative).
Any provisional completion at point is first rejected. If
deleting backwards into a word, and `auto-completion-mode' is
enabled, complete what remains of that word. \(If N is negative,
behaviour is instead as for `completion-delete-char'.\)

Optional second arg KILLFLAG non-nil means kill instead (save in
kill ring). Interactively, N is the prefix arg (default 1), and
KILLFLAG is set if N was explicitly specified."
  (interactive "P")
  (when (and (interactive-p) n) (setq killflag t))
  (setq n (prefix-numeric-value n))

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'delete-char (- n) killflag)
    (completion-backward-delete 'backward-delete-char n killflag)))



(defun completion-backward-delete-char-untabify (n &optional killflag)
  "Delete N characters backward, changing tabs into spaces.
Any provisional completion at point is first rejected. If
deleting backwards into a word, and `auto-completion-mode' is
enabled, complete what remains of that word. \(If N is negative,
behaviour is instead as for `completion-delete-char'.\)

Optional second arg KILLFLAG non-nil means kill instead (save in
kill ring). Interactively, N is the prefix arg (default 1), and
KILLFLAG is set if N was explicitly specified.

The exact behavior depends on `backward-delete-char-untabify-method'."
  (interactive "P")
  (when (and (interactive-p) n) (setq killflag t))
  (setq n (prefix-numeric-value n))

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'delete-char (- n) killflag)
    (completion-backward-delete 'backward-delete-char-untabify
                                n killflag)))



(defun completion-kill-word (&optional n)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times. If there is a provisional
completion at point after deleting, reject it. \(If N is
negative, behaviour is instead as for
`completion-backward-kill-word'.\)"
  (interactive "p")

  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-kill-word (- n))
    (completion-delete 'kill-word n)))



(defun completion-backward-kill-word (&optional n)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times. Any provisional
completion at point is first rejected. If deleting backwards into
a word, and `auto-completion-mode' is enabled, complete what
remains of that word.  \(If N is negative, behaviour is instead
as for `completion-kill-word'.\)"
  (interactive "p")

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-word (- n))
    (completion-backward-delete 'backward-kill-word n)))



(defun completion-kill-sentence (&optional n)
  "Kill from point to end of sentence.
With argument, do this that many times. If there is a provisional
completion at point after deleting, reject it. \(If N is
negative, behaviour is instead as for
`completion-backward-kill-sentence'.\)"
  (interactive "p")

  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-kill-sentence (- n))
    (completion-delete 'kill-sentence n)))



(defun completion-backward-kill-sentence (&optional n)
  "Kill back from point to start of sentence.
With argument, do this that many times. Any provisional
completion at point is first rejected. If deleting backwards into
a word, and `auto-completion-mode' is enabled, complete what
remains of that word.  \(If N is negative, behaviour is instead
as for `completion-kill-sentence'.\)"
  (interactive "p")

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-sentence (- n))
    (completion-backward-delete 'backward-kill-sentence n)))



(defun completion-kill-sexp (&optional n)
  "Kill the sexp (balanced expression) following point.
With argument, do this that many times. If there is a provisional
completion at point after deleting, reject it. \(If N is
negative, behaviour is instead as for
`completion-backward-kill-sexp'.\)"
  (interactive "p")

  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-kill-sexp (- n))
    (completion-delete 'kill-sexp n)))



(defun completion-backward-kill-sexp (&optional n)
  "Kill the sexp (balanced expression) before point.
With argument, do this that many times. Any provisional
completion at point is first rejected. If deleting backwards into
a word, and `auto-completion-mode' is enabled, complete what
remains of that word.  \(If N is negative, behaviour is instead
as for `completion-kill-sexp'.\)"
  (interactive "p")

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-sexp (- n))
    (completion-backward-delete 'backward-kill-sexp n)))



(defun completion-kill-line (&optional n)
  "Kill the line following point.
With argument, do this that many times. If there is a provisional
completion at point after deleting, reject it. \(If N is
negative, behaviour is instead as for
`completion-backward-kill-line'.\)"
  (interactive "P")

  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (and (integerp n) (< n 0))
      (completion-backward-delete 'kill-line (- n))
    (completion-delete 'kill-line n)))



(defun completion-backward-kill-line (&optional n)
  "Kill the line before point.
With argument, do this that many times. Any provisional
completion at point is first rejected. If deleting backwards into
a word, and `auto-completion-mode' is enabled, complete what
remains of that word.  \(If N is negative, behaviour is instead
as for `completion-kill-line'.\)"
  (interactive "p")

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-line (- n))
    (completion-backward-delete 'kill-line n)))



(defun completion-kill-paragraph (&optional n)
  "Kill forward to end of paragraph.
With argument, do this that many times. If there is a provisional
completion at point after deleting, reject it. \(If N is
negative, behaviour is instead as for
`completion-backward-kill-paragraph'.\)"
  (interactive "p")

  ;; if deleting backwards, call `completion-backward-delete' instead
  (if (< n 0)
      (completion-backward-delete 'backward-kill-paragraph (- n))
    (completion-delete 'kill-paragraph n)))



(defun completion-backward-kill-paragraph (&optional n)
  "Kill backward to start of paragraph.
With argument, do this that many times. Any provisional
completion at point is first rejected. If deleting backwards into
a word, and `auto-completion-mode' is enabled, complete what
remains of that word.  \(If N is negative, behaviour is instead
as for `completion-kill-paragraph'.\)"
  (interactive "p")

  ;; if deleting forwards, call `completion-delete' instead
  (if (< n 0)
      (completion-delete 'kill-paragraph (- n))
    (completion-backward-delete 'backward-kill-paragraph n)))




;;; ==============================================================
;;;                    Internal functions

(defun completion-prefix ()
  "Return the completion prefix at point.
The `completion-prefix-function' is set to this by default."

  (let ((word-thing (if (fboundp 'auto-overlay-local-binding)
                        (auto-overlay-local-binding
                         'completion-word-thing)
                      completion-word-thing
		      'word))
        (overlay (completion-overlay-at-point))
        (pos (point)))

    ;; if point is within existing completion overlay, return its prefix
    (if overlay
        (overlay-get overlay 'prefix)
      ;; otherwise, prefix is the word before point
      (save-excursion
        (forward-thing word-thing -1)
        (buffer-substring-no-properties (point) pos)))))



(defun completion-setup-overlay
  (prefix &optional prefix-length completions num
	  cmpl-function cmpl-prefix-function cmpl-replaces-prefix overlay)
  "Get completion overlay at point, or create a new one if none exists,
and set its properties according to PREFIX, PREFIX-LENGTH,
COMPLETIONS and NUM. If PREFIX-LENGTH is t, the overlay's
prefix-length property is left unchanges if already set. If NUM
is t, the overlay's completion-num property is left unchanged."

  (unless overlay (setq overlay (completion-overlay-at-point)))
  ;; if overlay does not already exists, create one
  (unless overlay
    (setq overlay (make-overlay (point) (point) nil nil t))
    ;; set permanent overlay properties
    (overlay-put overlay 'completion-overlay t)
    (overlay-put overlay 'completion-function
		 (or cmpl-function completion-function))
    (overlay-put overlay 'completion-prefix-function
		 (or cmpl-prefix-function completion-prefix-function))
    (overlay-put overlay 'completion-replaces-prefix
		 (or cmpl-replaces-prefix completion-replaces-prefix))
    (overlay-put overlay 'face 'completion-dynamic-face)
    (overlay-put overlay 'help-echo 'completion-construct-help-echo-text)
    (overlay-put overlay 'priority 100)
    ;; set overlay keymap
    (let ((map (make-sparse-keymap)))
      (overlay-put overlay 'keymap map)
      (set-keymap-parent map (if auto-completion-mode
                                 auto-completion-dynamic-map
                               completion-dynamic-map))
      ;; construct hotkey selection bindings from `completion-hotkey-list'
      (when (eq completion-use-hotkeys t)
	(completion-enable-hotkeys overlay)))
    ;; setup common substring overlay if enabled
    (when completion-dynamic-highlight-common-substring
      (let ((o (make-overlay (point) (point))))
        (overlay-put overlay 'common-substring o)
        (overlay-put o 'face 'completion-dynamic-common-substring-face)
        (overlay-put o 'priority 101)))
    ;; add overlay to list
    (push overlay completion-overlay-list))

  ;; update modifiable overlay properties
  (overlay-put overlay 'prefix prefix)
  (unless (and (eq prefix-length t)
	       (overlay-get overlay 'prefix-length))
    (unless (numberp prefix-length) (setq prefix-length (length prefix)))
    (overlay-put overlay 'prefix-length prefix-length))
  (overlay-put overlay 'completions completions)
  (unless (eq num t) (overlay-put overlay 'completion-num num))

  ;; return the new overlay
  overlay)



(defun completion-delete-overlay (overlay &optional keep-popup)
  "Delete completion overlay, and clean up after it.
If KEEP-POPUP is non-nil, prevent deletion of any pop-up frame
associated with OVERLAY."
  (when (overlayp (overlay-get overlay 'common-substring))
    (delete-overlay (overlay-get overlay 'common-substring)))
  (delete-overlay overlay)
  (setq completion-overlay-list (delq overlay completion-overlay-list))
  (when (and (not keep-popup) (overlay-get overlay 'popup-frame))
    (delete-frame (overlay-get overlay 'popup-frame))))



(defun completion-overlay-at-point (&optional point)
  "Return dynamic completion overlay overlapping point.
\(There should only be one; if not, one is returned at random\)"
  (setq point (or point (point)))

  ;; and overlays starting at POINT
  (let (overlay-list)
    (catch 'found
      ;; check overlays overlapping POINT (including zero-length)
      (setq overlay-list (overlays-in point point))
      (dolist (o overlay-list)
        (when (overlay-get o 'completion-overlay)
          (throw 'found o)))

      ;; check overlays ending at POINT
      (setq overlay-list (overlays-in (1- point) point))
      (dolist (o overlay-list)
        (when (and (overlay-get o 'completion-overlay)
                   (= (overlay-end o) point))
          (throw 'found o)))

      ;; check overlays starting at POINT
      (setq overlay-list (overlays-in point (1+ point)))
      (dolist (o overlay-list)
        (when (and (overlay-get o 'completion-overlay)
                   (= (overlay-start o) point))
          (throw 'found o)))
      )))



(defun completion-overlays-in (start end)
  "Return list of completion overlays between START and END."

  ;; get overlays between START and END
  (let ((o-list (overlays-in start end))
        overlay-list)
    ;; filter overlay list
    (dolist (o o-list)
      (when (overlay-get o 'completion-overlay)
        (push o overlay-list)))
    ;; return the overlay list
    overlay-list))



(defun completion-position-point-in-overlay (overlay)
  "Move point to appropriate position in OVERLAY,
the start for `auto-completion-mode' or if it is to be rejected,
the end of the common prefix for `accept-common' or
the end if it is to be accepted."
  (cond
   ;; reject or auto-completion-mode
   ((or auto-completion-mode
	(overlay-get overlay 'completion-replaces-prefix)
	(eq completion-resolve-behaviour 'reject))
    (goto-char (overlay-start overlay)))

   ;; accept-common
   ((eq completion-resolve-behaviour 'accept-common)
    (if (overlayp (overlay-get overlay 'common-substring))
        ;; if the common-prefix already exists goto the end of it
        (goto-char (overlay-end (overlay-get overlay 'common-substring)))
      ;; if the common-prefix doesn't exist search for it
      (let* ((prefix (overlay-get overlay 'prefix))
	     (completions (overlay-get overlay 'completions))
             (str (try-completion prefix completions)))
        ;; (try-completion returns t if there's only one completion)
        (when (eq str t) (setq str (car completions)))
	(when (and str (not (string= str "")))
	  (setq str (substring str (length prefix)))
	  (goto-char (+ (overlay-start overlay) (length str)))))))

   ;; accept
   (t (goto-char (overlay-end overlay)))))



(defun completion-highlight-prefix-alterations (prefix cmpl pos len)
  "Highlight altered characters in PREFIX.
CMPL is the current dynamic completion, and POS is the position
of the end of PREFIX in the buffer."
  (let ((p-len (length prefix)))
    ;; compare characters of prefix and highlight differences
    (dotimes (i (min len p-len))
      (unless (eq (aref cmpl i) (aref prefix i))
	(put-text-property
	 (+ pos i) (+ pos i 1)
	 (if font-lock-mode 'font-lock-face 'face)
	 'completion-dynamic-prefix-alterations-face)))
    ;; if prefix length has been altered, highlight all the remaining altered
    ;; prefix
    (when (< len p-len)
      (put-text-property
       (+ pos len) (+ pos p-len)
       (if font-lock-mode 'font-lock-face 'face)
       'completion-dynamic-prefix-alterations-face))))



(defun completion-highlight-common-substring (prefix cmpl pos len overlay)
  "Highlight longest common substring of all completions of PREFIX.
CMPL is the current dynamic completion, POS is the position of
the end of PREFIX in the buffer, LEN is the length of the prefix
inserted in the buffer, and OVERLAY is the dynamic completion
overlay."
  (let* ((cmpl-replaces-prefix
	  (overlay-get overlay 'completion-replaces-prefix))
	 (substr (try-completion
		  "" (mapcar
		      (lambda (cmpl)
			(if (stringp cmpl)
			    (if cmpl-replaces-prefix
				cmpl
			      (substring cmpl (length prefix)))
			  (if cmpl-replaces-prefix
			      (car cmpl)
			    (substring (car cmpl) (cdr cmpl)))))
		      (overlay-get overlay 'completions)))))
    ;; note: try-completion returns t if there's only one completion
    (move-overlay (overlay-get overlay 'common-substring)
		  (+ pos (if cmpl-replaces-prefix 0 len))
		  (if (eq substr t)
		      (+ pos (if cmpl-replaces-prefix 0 len))
		    (+ pos (if cmpl-replaces-prefix 0 len)
		       (length substr))))))



(defun completion-resolve-old (&optional overlay)
  "Resolve old dynamic completions according to the setting of
`completion-reslove-method'. Any completion overlay specified by
OVERLAY will be left alone."

  ;; temporarily remove ignored overlay from list
  (setq completion-overlay-list
        (delq overlay completion-overlay-list))

  (cond
   ;; leave old completions (but accept zero-length ones)
   ((eq completion-resolve-behaviour 'leave)
    (let (cmpl)
      (mapc (lambda (o)
	      (setq cmpl (buffer-substring-no-properties
			  (- (overlay-start o)
			     (if (and (overlay-get
				       o 'completion-replaces-prefix)
				      (overlay-get
				       o 'prefix-replaced))
				 0 (length (overlay-get o 'prefix))))
			  (overlay-end o)))
	      (overlay-put o 'evaporate t)
	      (when (overlay-get o 'popup-frame)
		(delete-frame (overlay-get o 'popup-frame))
		(overlay-put overlay 'popup-frame nil))
	      ;; if overlay hasn't evaporated, reset evaporate property,
	      ;; otherwise delete it from overlay list
	      (if (overlay-buffer o)
		  (overlay-put o 'evaporate nil)
		(setq completion-overlay-list
		      (delq o completion-overlay-list))
		(run-hook-with-args 'completion-accept-functions
				    (overlay-get o 'prefix) cmpl)))
	    completion-overlay-list)))

   ;; accept old completions
   ((eq completion-resolve-behaviour 'accept)
    (mapc (lambda (o)
            (run-hook-with-args 'completion-accept-functions
                                (overlay-get o 'prefix)
				(buffer-substring-no-properties
				 (- (overlay-start o)
				    (if (and (overlay-get
					      o 'completion-replaces-prefix)
					     (overlay-get
					      o 'prefix-replaced))
					0 (length (overlay-get o 'prefix))))
				 (overlay-end o)))
            ;; if `completion-replaces-prefix' is non-nil, delete prefix
            ;; before accepting; no one in their right mind would want this
            ;; behaviour, but we shouldn't discriminate against the insane
            (when (and (overlay-get o 'completion-replaces-prefix)
		       (not (overlay-get o 'prefix-replaced)))
              (delete-region (- (overlay-start o)
				(overlay-get o 'prefix-length))
                             (overlay-start o)))
            (completion-delete-overlay o))
	  completion-overlay-list)
    (setq completion-overlay-list nil))

   ;; reject old completions
   ((eq completion-resolve-behaviour 'reject)
    (save-excursion
      (mapc (lambda (o)
	      (run-hook-with-args
	       'completion-reject-functions
	       (overlay-get o 'prefix)
	       (buffer-substring-no-properties
		(- (overlay-start o)
		   (if (and (overlay-get o 'completion-replaces-prefix)
			    (overlay-get o 'prefix-replaced))
		       0 (length (overlay-get o 'prefix))))
		(overlay-end o)))
	      (goto-char (overlay-start o))
	      (delete-region
	       (- (overlay-start o)
		  (if (and (overlay-get o completion-replaces-prefix)
			   (overlay-get o 'prefix-replaced))
		      0 (overlay-get o 'prefix-length)))
	       (overlay-end o))
	      (let ((overwrite-mode nil)) (insert (overlay-get o 'prefix)))
	      (completion-delete-overlay o))
          completion-overlay-list))  ; mapc target
    (setq completion-overlay-list nil))

   ;; leave longest common substring
   ((eq completion-resolve-behaviour 'accept-common)
    (mapc (lambda (o)
	    ;; if common substring is highlighted, use that for location
	    (if (overlay-get o 'common-substring)
		(delete-region (overlay-end (overlay-get o 'common-substring))
			       (overlay-end o))
	      ;; otherwise, find longest common substring
	      (let ((pfxlen (length (overlay-get o 'prefix))))
		(delete-region
		 (+ (overlay-start o)
		    (length (try-completion
			     "" (mapcar
				 (lambda (cmpl) (substring cmpl pfxlen))
				 (overlay-get o 'completions)))))
		 (overlay-end o))))
	    (completion-delete-overlay o))
	  completion-overlay-list))

   ;; ask 'em
   ((eq completion-resolve-behaviour 'ask)
    (save-excursion
      (mapc (lambda (o)
              (goto-char (overlay-end o))
              ;; FIXME: remove hard-coded face
              (overlay-put o 'face '(background-color . "red"))
              (if (y-or-n-p "Accept completion? ")
                  ;; accept
		  (progn
		    (when (and (overlay-get o 'completion-replaces-prefix)
			       (not (overlay-get o 'prefix-replaced)))
		      (delete-region (- (overlay-start o)
					(overlay-get o 'prefix-length))
				     (overlay-start o)))
		    (run-hook-with-args
		     'completion-accept-functions
		     (overlay-get o 'prefix)
		     (buffer-substring-no-properties
		      (- (overlay-start o)
			 (if (and (overlay-get o 'completion-replaces-prefix)
				  (overlay-get o 'prefix-replaced))
			     0 (length (overlay-get o 'prefix))))
		      (overlay-end o))
		     nil))

                ;; reject
                (run-hook-with-args
                 'completion-reject-functions
                 (overlay-get o 'prefix)
                 (buffer-substring-no-properties
		  (- (overlay-start o)
		     (if (and (overlay-get o 'completion-replaces-prefix)
			      (overlay-get o 'prefix-replaced))
			 0 (length (overlay-get o 'prefix))))
		  (overlay-end o))
                 nil)
		(goto-char (overlay-start o))
                (delete-region (- (overlay-start o)
				  (if (and (overlay-get
					    o 'completion-replaces-prefix)
					   (overlay-get
					    o 'prefix-replaced))
				      0 (overlay-get o 'prefix-length)))
			       (overlay-end o))
		(let ((overwrite-mode nil)) (insert (overlay-get o 'prefix))))

              ;; delete overlay and any pop-up frame associated with it
              (completion-delete-overlay o))
            completion-overlay-list)  ; mapc target
      (setq completion-overlay-list nil)))
   )

  ;; add ignored overlay back into the list
  (when (overlayp overlay) (push overlay completion-overlay-list)))



(defun completion-resolve-current (&optional overlay char syntax)
  "Resolve current completion according to customization settings.

If OVERLAY is supplied, use that instead of trying to find one at
point. The point had better be within OVERLAY or your pet
mosquito will suffer an untimely death.

If CHAR and/or SYNTAX are supplied and `auto-completion-mode' is
enabled, resolve current completion as though the character CHAR
with syntax class SYNTAX was inserted at point (without actually
inserting anything)."

  ;; if no overlay was supplied, try to find one at point
  (unless overlay (setq overlay (completion-overlay-at-point)))
  ;; resolve provisional completions not at point
  (completion-resolve-old overlay)

  ;; if there's a completion at point...
  (when overlay
    (let (resolve)

      ;; if `auto-completion-mode' is disabled, or neither CHAR nor
      ;; SYNTAX were supplied...
      (if (or (not auto-completion-mode) (not (or char syntax)))
          (cond
           ((or (eq completion-resolve-behaviour 'reject)
                (eq completion-resolve-behaviour 'accept-common))
            (setq resolve 'reject))
           ((eq completion-resolve-behaviour 'accept)
            (setq resolve 'accept))
           (t (setq resolve 'other)))

        ;; otherwise, if point is not at start of overlay, we want to
        ;; effectively accept completion without running hooks
        (if (/= (point) (overlay-start overlay))
            (setq resolve 'other)
          ;; otherwise, lookup behaviour for CHAR and SYNTAX
          (setq resolve (completion-get-resolve-behaviour
                         (completion-lookup-behaviour char syntax)))))


      (cond
       ;; if rejecting...
       ((eq resolve 'reject)
        ;; if point is at the start of a completion, reject normally
        (if (= (point) (overlay-start overlay))
            (completion-reject nil overlay)
          ;; otherwise, delete everything after point but keep whatever
          ;; comes before it
          (delete-region (point) (overlay-end overlay))
          (completion-delete-overlay overlay)))

       ;; if accepting, do so
       ((eq resolve 'accept)
        (completion-accept overlay))

       ;; anything else effectively accepts the completion but without
       ;; running accept hooks
       (t (completion-delete-overlay overlay)))
      )))



(defun completion-enable-hotkeys (overlay)
  "Enable completion hotkeys for OVERLAY."
  (dolist (key completion-hotkey-list)
    (define-key (overlay-get overlay 'keymap) (vector key)
      'completion-select)))


(defun completion-disable-hotkeys (overlay)
  "Disable completion hotkeys for OVERLAY."
  (dolist (key completion-hotkey-list)
    (define-key (overlay-get overlay 'keymap) (vector key) nil)))


(defun completion-enable-tooltip-keys (overlay)
  "Enable tooltip key bindings for OVERLAY."
  (map-keymap
   (lambda (key binding)
     (define-key (overlay-get overlay 'keymap) (vector key) binding))
   completion-tooltip-map))


(defun completion-disable-tooltip-keys (overlay)
  "Disable tooltip key bindings for OVERLAY."
  (map-keymap
   (lambda (key binding)
     (define-key (overlay-get overlay 'keymap) (vector key) nil))
   completion-tooltip-map))


(defun completion-cancel-tooltip ()
  "Hide the completion tooltip and cancel timers."
  (interactive)
  ;; cancel timer
  (when (timerp completion-auto-timer)
    (cancel-timer completion-auto-timer))
  ;; disable hotkeys if only enabled when a tooltip or pop-up frame is active
  (when (and (eq completion-use-hotkeys 'pop-up) completion-tooltip-active)
    (completion-disable-hotkeys completion-tooltip-active))
  ;; cancel tooltip
  (when (and completion-function window-system (fboundp 'x-show-tip))
    (tooltip-hide)
    (when (overlayp completion-tooltip-active)
      (completion-disable-tooltip-keys completion-tooltip-active))
    (setq completion-tooltip-active nil)))



(defun completion-run-if-condition
  (command variable condition &optional when)
  "Run COMMAND if CONDITION is non-nil.

If WHEN is null or 'instead, run whatever would normally be bound
to the key sequence used to invoke this command if not within a
completion overlay. If WHEN is 'before or 'after, run the normal
binding before or after COMMAND.

VARIABLE should be a symbol that deactivates the keymap in which
COMMAND is bound when its value is set to nil. It is reset at the
end of this function.

Intended to be invoked (directly or indirectly) via a key
sequence in a keymap."

  ;; throw and error if executing recursively
  (when completion-trap-recursion
    (error "Recursive call to `completion-run-if-condition';\
 supplied variable probably doesn't disable keymap"))

  ;; run command if running before, or if running instead and CONDITION
  ;; is non-nil
  (when (or (eq when 'before)
            (and (or (null when) (eq when 'instead))
                 condition))
    (command-execute command))

  ;; run whatever would normally be bound to the key sequence,
  ;; unless running instead and CONDITION is non-nil
  (unless (and (or (null when) (eq when 'instead)) condition)
    (let ((completion-trap-recursion t)
          (restore (eval variable))
          command)
      (set variable nil)
      ;; we add (this-command-keys) to `unread-command-events' and then
      ;; re-read it in order to ensure key sequence translation takes place
      (setq unread-command-events (listify-key-sequence (this-command-keys)))
      (setq command (key-binding (read-key-sequence "") t))
      (unwind-protect
          (when (commandp command)
            (command-execute command)
            (setq last-command command))  ; doesn't work - clobbered later :(
        (set variable restore))))

  ;; run command if running after
  (when (eq when 'after) (command-execute command))
  )



(defun completion-run-if-within-overlay
  (command variable &optional when)
  "Run COMMAND if within a completion overlay.

If WHEN is null or 'instead, run whatever would normally be bound
to the key sequence used to invoke this command if not within a
completion overlay. If WHEN is 'before or 'after, run the normal
binding before or after COMMAND.

VARIABLE should be a symbol that deactivates the keymap in which
COMMAND is bound when its value is set to nil. It is reset at the
end of this function.

Intended to be (invoked directly or indirectly) via a key
sequence in a keymap."
  (completion-run-if-condition
   command variable (completion-overlay-at-point) when))



(defun completion-construct-tooltip-text
  (prefix completions &optional num)
  "Function to return completion text for a tooltip.
Optional argument NUM specifies the number of the currently
inserted dynamic completion."

  (let* ((text "") str
         (maxlen (if (null completions) 0
                   (apply 'max (mapcar (lambda (cmpl)
					 (if (stringp cmpl)
					     (length cmpl)
					   (length (car cmpl))))
				       completions)))))

    (dotimes (i (length completions))
      ;; pad all strings to same length
      (setq str (nth i completions))
      (unless (stringp str) (setq str (car str)))
      (setq str (concat str (make-string (- maxlen (length str)) ? )))
      ;; if using hotkeys and one is assigned to current completion,
      ;; show it next to completion text
      (when (and completion-use-hotkeys
                 (< i (length completion-hotkey-list)))
        (setq str
              (concat str " "
                      (format "(%s)"
                              (key-description
                               (vector (nth i completion-hotkey-list)))))))
      ;; if current completion is the inserted dynamic completion, use
      ;; `completion-dynamic-face' to highlight it
      (when (and num (= i num))
        ;; setting 'face attribute to 'completion-dynamic-face
        ;; doesn't seem to work with defface using display classes
        (put-text-property
         0 (length str) 'face
         `((foreground-color . ,(face-attribute 'completion-dynamic-face
                                                :foreground))
           (background-color . ,(face-attribute 'completion-dynamic-face
                                                :background)))
         str))
      (setq text (concat text str "\n")))

    ;; return constructed text
    text))



(defun completion-construct-echo-text (overlay)
  "Function to return completion text for echo area."

  (let* ((prefix (overlay-get overlay 'prefix))
         (completions (overlay-get overlay 'completions))
         (text "") cmpl)

    (dotimes (i (length completions))
      (setq cmpl (nth i completions))
      (unless (stringp cmpl) (setq cmpl (car cmpl)))
      ;; if using hotkeys and one is assigned to current completion,
      ;; show it next to completion text
      (cond
       ((and (eq completion-use-hotkeys t)
             (< i (length completion-hotkey-list)))
        (setq cmpl
              (concat
               (format "(%s) "
                       (key-description
                        (vector (nth i completion-hotkey-list)))) cmpl)))
       ((eq completion-use-hotkeys t)
        (setq cmpl (concat "() " cmpl))))
      (setq text (concat text cmpl "  ")))

    ;; return constructed text
    text))



(defun completion-construct-help-echo-text (dummy1 overlay dummy2)
  "Function to return text for help-echo property
of completion overlay."

  (let* ((text "") str
         (prefix (overlay-get overlay 'prefix))
         (completions (overlay-get overlay 'completions))
         (num (overlay-get overlay 'completion-num)))

    ;; if `tooltip-mode' is enabled, construct text for tooltip
    (if tooltip-mode
        (dotimes (i (length completions))
          ;; if using hotkeys and one is assigned to current
          ;; completion, show it next to completion text
          (if (or (eq completion-use-hotkeys t)
		  (and (eq completion-use-hotkeys 'pop-up)
		       (or (eq completion-tooltip-active overlay)
			   (overlay-get overlay 'popup-frame))))
	      (if (< i (length completion-hotkey-list))
		  (setq str
			(format "(%s) "
				(key-description
				 (vector (nth i completion-hotkey-list)))))
		(setq str "     "))
	    (setq str ""))
          ;; add completion to text
          (setq str (concat str (nth i completions)))
          (setq text (concat text str "\n")))

      ;; otherwise, construct text for echo area
      (setq text (completion-construct-echo-text overlay)))

    ;; return constructed text
    text))



(defun completion-construct-popup-frame-text (prefix completions)
  "Construct the list of lines for a pop-up frame."
  (let ((maxlen (if (null completions) 0
                   (apply 'max (mapcar (lambda (cmpl)
					 (if (stringp cmpl)
					     (length cmpl)
					   (length (car cmpl))))
				       completions))))
        (lines nil)
	str)
    (dotimes (i (length completions))
      (setq str (nth i completions))
      (unless (stringp str) (setq str (car str)))
      (setq lines
            (append lines
                    (list
                     (concat
		      str
                      ;; pad to same length
                      (make-string (- maxlen (length str)) ? )
                      ;; add hotkey for current completion, if any
                      (if (and completion-use-hotkeys
                               (< i (length completion-hotkey-list)))
                          (format " (%s)"
                                  (key-description
                                   (vector (nth i completion-hotkey-list))))
                        ""))))))
    lines))  ; return pop-up frame lines



(defun completion-construct-menu (prefix completions &rest ignored)
  "Construct and return menu keymap defining the completion menu.

\(IGNORED argument is to absorb additional unneeded argument when
called from `completion-show-menu'.\)"

  (let ((menu (make-sparse-keymap))
        (num (length completions))
        n)

    ;; construct menu keymap from available completions
    (dotimes (i num)
      (setq n (- num i 1))
      (define-key menu
        (vector (intern (concat "completion-insert-" (number-to-string n))))
        (list 'menu-item
	      (if (stringp (nth n completions))
		  (nth n completions)
		(car (nth n completions)))
	      `(lambda ()
		 (list ,(if (stringp (nth n completions))
			    (nth n completions) (car (nth n completions)))
		       ,(if (stringp (nth n completions))
			    (length prefix) (cdr (nth n completions)))))
	      ;; if a hotkey is associated with completion, show it in menu
	      :keys (when (and completion-use-hotkeys
			       (< n (length completion-hotkey-list)))
		      (key-description
		       (vector (nth n completion-hotkey-list)))))))

    ;; add entry to switch to completion browser
    (define-key-after menu [separator-browser] '(menu-item "--"))
    (define-key-after menu [completion-browser-menu-function]
      (list 'menu-item "Browser..." 'completion-show-browser-menu))

    ;; return the menu keymap
    menu))



(defun completion-construct-browser-menu
  (prefix completions
   cmpl-function cmpl-prefix-function cmpl-replaces-prefix
   &optional menu-item-func sub-menu-func)
  "Construct the completion browser menu keymap
from the supplied PREFIX (COMPLETIONS is ignored and replaced by
all completions of PREFIX in the current dictionary), using
CMPL-FUNCTION to get completions.

MENU-ITEM-FUNC and SUB-MENU-FUNC override the default functions
for creating the sub-menus and menu items. Both functions are
passed a 4-item list containing PREFIX, a list of completions of
PREFIX, MENU-ITEM-FUNC and SUB-MENU-FUNC."

  ;; inform user it's in progress (note: can't display "done" message
  ;; since this function returns as soon as main menu is constructed,
  ;; before all submenus have been constructed by :filter functions)
  (message "Creating predictive completion browser\
 (C-g to cancel if taking too long)...")

  ;; default menu creation functions
  (unless menu-item-func
    (setq menu-item-func 'completion-browser-menu-item))
  (unless sub-menu-func
    (setq sub-menu-func 'completion-browser-sub-menu))

  ;; find all completions of prefix
  (setq completions (funcall cmpl-function prefix))

  ;; main browser menu is just a browser submenu...
  (let ((menu (funcall sub-menu-func
                       prefix completions
                       cmpl-function
		       cmpl-prefix-function
		       cmpl-replaces-prefix
		       menu-item-func sub-menu-func)))
    ;; ... with an item added for switching to the basic completion
    ;; menu
    (define-key-after menu [separator-basic] '(menu-item "--"))
    (define-key-after menu [completion-menu]
      (list 'menu-item "Basic..." 'completion-show-menu))

    ;; return keymap
    menu))



;; Note:
;;
;; I should probably use some `imenu' function to create the menu,
;; since `imenu' already deals with "bucketising" menus (an ugly
;; necessity which should anyway be replaced with menu scrollbars,
;; preferably with just-in-time calculation of menu entries --
;; heads-up Emacs devs!).
;;
;; My excuses are that `imenu--mouse-menu' etc. are undocumented,
;; rolling my own was easier, and anyway I think my buckets are better
;; (they're optimal in the information-theoretic sense that you need
;; to make the least number of choices to get to the entry you want).
;;
;; One day I might patch the `imenu' "bucketising" code, and use
;; `imenu' here instead. Don't hold your breath.

(defun completion-browser-sub-menu
  (prefix completions
   cmpl-function cmpl-prefix-function cmpl-replaces-prefix
   menu-item-func sub-menu-func)
  "Construct a predictive completion browser sub-menu keymap."

  (let* ((menu (make-sparse-keymap))
         (num-completions (length completions)))

    (cond

     ;; if there's only 1 entry, don't bother with sub-menu, just set keymap
     ;; to be the item itself
     ((= num-completions 1)
      (let* ((cmpl (car completions))
	     (entry (funcall menu-item-func
			     prefix cmpl
			     cmpl-function
			     cmpl-prefix-function
			     cmpl-replaces-prefix
			     menu-item-func sub-menu-func)))
	(cond
	 ;; if entry is a menu keymap, use it as the menu, adding completion
	 ;; itself to the top
	 ((keymapp entry)
	  (define-key entry [separator-item-sub-menu] '(menu-item "--"))
	  (define-key entry [completion-insert-root]
	    (list
	     'menu-item cmpl
	     `(lambda ()
		(list ,(if (stringp cmpl) cmpl (car cmpl))
		      ,(if (stringp cmpl) (length prefix) (cdr cmpl))))))
	  (setq menu entry))

	 (t  ;; if entry is a single item, add it to the menu
	  (define-key menu [completion-insert-0]
	    (list
	     'menu-item cmpl
	     `(lambda ()
		(list ,(if (stringp cmpl) cmpl (car cmpl))
		      ,(if (stringp cmpl) (length prefix) (cdr cmpl))))))))
	))


     ;; if menu does not need to be divided into buckets, just add the
     ;; completions themselves to the keymap
     ((<= num-completions completion-browser-max-items)
      (dotimes (i num-completions)
	(define-key-after menu
	  (vector (intern (concat "completion-insert-"
				  (number-to-string i))))
	  (list 'menu-item
		(if (stringp (nth i completions))
		    (nth i completions)
		  (car (nth i completions)))
		(funcall menu-item-func
			 prefix
			 (nth i completions)
			 cmpl-function
			 cmpl-prefix-function
			 cmpl-replaces-prefix
			 menu-item-func sub-menu-func))
	  )))


     ;; if menu needs to be divided into buckets, construct a menu keymap
     ;; containing the bucket menus
     (t
      (let* ((num-buckets
              (cond
               ;; maximize number of buckets, minimize size of
               ;; contents
               ((eq completion-browser-buckets 'max)
                completion-browser-max-items)
               ;; minimize number of buckets, maximize size of
               ;; contents
               ((eq completion-browser-buckets 'min)
                (min completion-browser-max-items
		     (1+ (/ (1- num-completions)
			    completion-browser-max-items))))
               ;; balance number of buckets and size of contents
               (t
                (min completion-browser-max-items
                     (round (sqrt num-completions))))))
             (num-per-bucket (/ num-completions num-buckets))
             (num-large-buckets (% num-completions num-buckets))
             (num-small-buckets (- num-buckets num-large-buckets))
             i j)

        (dotimes (b num-buckets)
          ;; if bucket has only 1 entry, don't bother with bucket
          ;; menu, just add completion itself to keymap
          (if (and (= 1 num-per-bucket) (< b num-small-buckets))
              (define-key-after menu
		(vector (intern (concat "completion-insert-"
					(number-to-string i))))
		(list 'menu-item
		      (if (stringp (nth i completions))
			  (nth i completions)
			(car (nth i completions)))
		      (funcall menu-item-func
			   prefix
			   (nth i completions)
			   cmpl-function
			   cmpl-prefix-function
			   cmpl-replaces-prefix
			   menu-item-func sub-menu-func)))

            ;; if bucket has more than 1 entry...
            ;; get index of first completion in bucket
            (setq i (+ (* (min b num-small-buckets) num-per-bucket)
                       (* (max 0 (- b num-small-buckets))
                          (1+ num-per-bucket))))
            ;; get index of last completion in bucket
            (setq j (1- (+ i num-per-bucket
			   (if (< b num-small-buckets) 0 1))))
            ;; add bucket menu to keymap
            (define-key-after menu
              (vector (intern (concat "bucket-" (number-to-string b))))
              (list 'menu-item
                    (concat "From \""
                            (nth i completions)
                            "\" to \""
                            (nth j completions) "\"")
                    ;; call function to generate sub-menu
                    (funcall sub-menu-func
                             prefix
			     (completion--sublist completions i (1+ j))
                             cmpl-function
			     cmpl-prefix-function
			     cmpl-replaces-prefix
			     menu-item-func sub-menu-func))))
          ))))

    ;; return constructed menu
    menu))



(defun completion-browser-menu-item
  (prefix cmpl
   cmpl-function cmpl-prefix-function cmpl-replaces-prefix
   menu-item-func sub-menu-func)
  "Construct predictive completion browser menu item."

  (let (completions)
    ;; If `completion-replaces-prefix' is null, get completions for entry,
    ;; dropping the empty string which corresponds to the same entry again
    ;; (which would lead to infinite recursion). It makes no sense to get
    ;; completions of completions (of completions of completions...) when
    ;; doing something other than prefix-completion, so the entry is just the
    ;; original completion itself if `completion-replaces-prefix' is non-nil.
    (when (and completion-browser-recurse-on-completions
               (not cmpl-replaces-prefix)
	       (not (string= cmpl "")))
      (setq completions (cdr (funcall cmpl-function cmpl))))

    ;; if there are no completions (other than the entry itself),
    ;; create a selectable completion item
    (if (null completions)
        `(lambda ()
	   (list ,(if (stringp cmpl) cmpl (car cmpl))
		 ,(if (stringp cmpl) (length prefix) (cdr cmpl))))
      ;; otherwise, create a sub-menu containing them
      (let ((menu (funcall sub-menu-func prefix completions
                           cmpl-function
			   cmpl-prefix-function
			   cmpl-replaces-prefix
			   menu-item-func sub-menu-func)))
	;; add completion itself to the menu
        (define-key menu [separator-item-sub-menu] '(menu-item "--"))
        (define-key menu [completion-insert-root]
          (list 'menu-item
		cmpl
		`(lambda ()
		   (list ,(if (stringp cmpl) cmpl (car cmpl))
			 ,(if (stringp cmpl) (length prefix) (cdr cmpl))))))
        ;; return the menu keymap
        menu))))



(defun completion-beginning-of-word-p (&optional point)
  "Return non-nil if POINT is at beginning of a word
\(POINT defaults to the point\)."
  (unless point (setq point (point)))
  (save-excursion
    (goto-char point)
    (let (bounds)
      (and (< point (point-max))
           (setq bounds
                 (bounds-of-thing-at-point
                  (if (fboundp 'auto-overlay-local-binding)
                      (auto-overlay-local-binding
                       'completion-word-thing)
                    completion-word-thing
		    'word)))
           (= point (car bounds))))))



(defun completion-within-word-p (&optional point)
  "Return non-nil if POINT is within or at end of a word
\(POINT defaults to the point\)."
  (unless point (setq point (point)))
  (save-excursion
    (goto-char point)
    (let (bounds)
      (and (setq bounds
                 (bounds-of-thing-at-point
                  (if (fboundp 'auto-overlay-local-binding)
                      (auto-overlay-local-binding
                       'completion-word-thing)
                    completion-word-thing
		    'word)))
           (> point (car bounds))
           (< point (cdr bounds))))))



(defun completion-end-of-word-p (&optional point)
  "Return non-nil if POINT is at end of a word
\(POINT defaults to the point\)"
  (unless point (setq point (point)))
  (save-excursion
    (goto-char point)
    (let (bounds)
      (and (> point (point-min))
           (setq bounds
                 (bounds-of-thing-at-point
                  (if (fboundp 'auto-overlay-local-binding)
                      (auto-overlay-local-binding
                       'completion-word-thing)
                    completion-word-thing
		    'word)))
           (= point (cdr bounds))))))



(defun completion-posn-at-point-as-event
  (&optional position window dx dy)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of WINDOW, as a mouse-1 click
event (identical to the event that would be triggered by clicking
mouse button 1 at the top left corner of the glyph).

POSITION and WINDOW default to the position of point in the
selected window.

DX and DY specify optional offsets from the top left of the glyph."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (unless dx (setq dx 0))
  (unless dy (setq dy 0))

  (let* ((pos (posn-at-point position window))
         (x-y (posn-x-y pos))
         (edges (window-inside-pixel-edges window))
         (win-x-y (window-pixel-edges window)))
    ;; adjust for window edges
    (setcar (nthcdr 2 pos)
            (cons (+ (car x-y) (car  edges) (- (car win-x-y))  dx)
                  (+ (cdr x-y) (cadr edges) (- (cadr win-x-y)) dy)))
    (list 'mouse-1 pos)))



(defun completion-window-posn-at-point (&optional position window)
  "Return pixel position of top left of corner glyph at POSITION,
relative to top left corner of WINDOW. Defaults to the position
of point in the selected window.

See also `completion-window-inside-posn-at-point' and
`completion-frame-posn-at-point'."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))

  (let ((x-y (posn-x-y (posn-at-point position window)))
        (edges (window-inside-pixel-edges window))
        (win-x-y (window-pixel-edges window)))
    (cons (+ (car x-y) (car  edges) (- (car win-x-y)))
          (+ (cdr x-y) (cadr edges) (- (cadr win-x-y))))))



(defun completion-window-inside-posn-at-point
  (&optional position window)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of the text area in WINDOW. Defaults
to the position of point in the selected window.

See also `completion-window-posn-at-point' and
`completion-frame-posn-at-point'.."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (posn-x-y (posn-at-point position window)))



(defun completion-frame-posn-at-point (&optional position window)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults
to the position of point in the selected window.

See also `completion-window-posn-at-point' and
`completion-window-inside-posn-at-point'."

  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))

  (let ((x-y (posn-x-y (posn-at-point position window)))
        (edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car  edges))
          (+ (cdr x-y) (cadr edges)))))




;;; ===============================================================
;;;                     Compatibility Stuff

;; prevent bogus compiler warnings
(eval-when-compile
  (defun completion-compat-window-offsets (dummy)))



(unless (fboundp 'posn-at-point)
  ;;  (require 'completion-ui-compat)


  (defun completion-compat-frame-posn-at-point
    (&optional position window)
    "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults
to the position of point in the selected window."

    (unless window (setq window (selected-window)))
    (unless position (setq position (window-point window)))

    ;; get window-relative position in units of characters
    (let* ((x-y (compute-motion (window-start) '(0 . 0)
                                position
                                (cons (window-width) (window-height))
                                (window-width)
                                ; prob. shouldn't be 0
                                (cons (window-hscroll) 0)
                                window))
           (x (nth 1 x-y))
           (y (nth 2 x-y))
           (offset (completion-compat-window-offsets window))
           (restore (mouse-pixel-position))
           pixel-pos)

      ;; move and restore mouse position using position in units of
      ;; characters to get position in pixels
      (set-mouse-position (window-frame window)
                          (+ x (car offset)) (+ y (cdr offset)))
      (setq pixel-pos (cdr (mouse-pixel-position)))
      (set-mouse-pixel-position (car restore) (cadr restore)
                                (cddr restore))

      ;; return pixel position
      (setcdr pixel-pos
              (- (cdr pixel-pos)
                 (/ (frame-char-height (window-frame window)) 2)))
      pixel-pos))



  (defun completion-compat-posn-at-point-as-event
    (&optional position window dx dy)
    "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of WINDOW, as a mouse-1 click
event (identical to the event that would be triggered by clicking
mouse button 1 at the top left corner of the glyph).

POSITION and WINDOW default to the position of point in the
selected window.

DX and DY specify optional offsets from the top left of the
glyph."

    (unless window (setq window (selected-window)))
    (unless position (setq position (window-point window)))
    (unless dx (setq dx 0))
    (unless dy (setq dy 0))

    ;; get window-relative position in units of characters
    (let* ((x-y (compute-motion (window-start) '(0 . 0)
                                position
                                (cons (window-width) (window-height))
                                (window-width)
                                        ; prob. shouldn't be 0
                                (cons (window-hscroll) 0)
                                window))
           (x (nth 1 x-y))
           (y (nth 2 x-y))
           (offset (completion-compat-window-offsets window))
           (restore (mouse-pixel-position))
           (frame (window-frame window))
           (edges (window-edges window))
           pixel-pos)

      ;; move and restore mouse position using position in units of
      ;; characters to get position in pixels
      (set-mouse-position (window-frame window)
                          (+ x (car offset)) (+ y (cdr offset)))
      (setq pixel-pos (cdr (mouse-pixel-position)))
      (set-mouse-pixel-position (car restore) (cadr restore)
                                (cddr restore))

      ;; convert pixel position from frame-relative to window-relative
      ;; (this is crude and will fail e.g. if using different sized
      ;; fonts)
      (setcar pixel-pos (- (car pixel-pos) 1
                           (* (frame-char-width frame) (car edges))))
      (setcdr pixel-pos (- (cdr pixel-pos) 1
                           (* (frame-char-height frame) (nth 1 edges))
                           (/ (frame-char-height frame) 2)))

      ;; return a fake event containing the position
      (setcar pixel-pos (+ (car pixel-pos) dx))
      (setcdr pixel-pos (+ (cdr pixel-pos) dy))
      (list 'mouse-1 (list window position pixel-pos))))



;;; Borrowed from senator.el (I promise I'll give it back when I'm
;;; finished...)

  (defun completion-compat-window-offsets (&optional window)
    "Return offsets of WINDOW relative to WINDOW's frame.
Return a cons cell (XOFFSET . YOFFSET) so the position (X . Y) in
WINDOW is equal to the position ((+ X XOFFSET) .  (+ Y YOFFSET))
in WINDOW'S frame."
    (let* ((window  (or window (selected-window)))
           (e       (window-edges window))
           (left    (nth 0 e))
           (top     (nth 1 e))
           (right   (nth 2 e))
           (bottom  (nth 3 e))
           (x       (+ left (/ (- right left) 2)))
           (y       (+ top  (/ (- bottom top) 2)))
           (wpos    (coordinates-in-window-p (cons x y) window))
           (xoffset 0)
           (yoffset 0))
      (if (consp wpos)
          (let* ((f  (window-frame window))
                 (cy (/ 1.0 (float (frame-char-height f)))))
            (setq xoffset (- x (car wpos))
                  yoffset (float (- y (cdr wpos))))
            ;; If Emacs 21 add to:
            ;; - XOFFSET the WINDOW left margin width.
            ;; - YOFFSET the height of header lines above WINDOW.
            (if (> emacs-major-version 20)
                (progn
                  (setq wpos    (cons (+ left xoffset) 0.0)
                        bottom  (float bottom))
                  (while (< (cdr wpos) bottom)
                    (if (eq (coordinates-in-window-p wpos window)
                            'header-line)
                        (setq yoffset (+ yoffset cy)))
                    (setcdr wpos (+ (cdr wpos) cy)))
                  (setq xoffset
                        (floor (+ xoffset
                                  (or (car (window-margins window))
                                      0))))))
            (setq yoffset (floor yoffset))))
      (cons xoffset yoffset)))


  (defun completion-compat-line-number-at-pos (pos)
    "Return (narrowed) buffer line number at position POS.
\(Defaults to the point.\)"
    (1+ (count-lines (point-min) pos)))


  (defalias 'completion-posn-at-point-as-event
    'completion-compat-posn-at-point-as-event)
  (defalias 'completion-frame-posn-at-point
    'completion-compat-frame-posn-at-point)
)



;;; =================================================================
;;;                 Set modification hook functions

;; (add-hook 'after-change-functions
;;           (lambda (&rest unused) (completion-cancel-tooltip)))

;; we reset tooltip flag after any command because Emacs hides tooltips
;; after any command
(add-hook 'pre-command-hook 'completion-cancel-tooltip)



;;; =================================================================
;;;                      Compatibility hacks

;; If the current Emacs version doesn't support overlay keybindings half
;; decently, have to simulate them using the
;; `completion-run-if-within-overlay' hack. So far, no Emacs version supports
;; things properly for zero-length overlays, so we always have to do this!

(when (<= emacs-major-version 21)
  (completion-simulate-overlay-bindings completion-dynamic-map completion-map
                                        'completion-function)
  (completion-simulate-overlay-bindings auto-completion-dynamic-map
                                        auto-completion-map
                                        'auto-completion-mode t))



(provide 'completion-ui)

;;; completion-ui.el ends here
