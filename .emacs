;; Localization
(prefer-coding-system 'utf-8)    

;;;;;;;;;;
;; startup

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)


;; end startup
;;;;;;;;;;;;;;


;;; 
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path "~/elisp/")
(add-subdirs-to-load-path "~/emacs/site-lisp/packages/")


;;;;;;;;;;;
;; require

(require 'elscreen)  ;; screen like tabs


(require 'auto-complete)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'haskell-mode)

;; developement
;;(require 'python)
;;(require 'pysmell)


(require 'yasnippet) ;TODO: configure it properly
(require 'flymake)



;; end require
;;;;;;;;;;;;;;




;;;;;;;;;;;;;; 
;; colortheme

(defvar current-color-theme
  "the current color-theme")

(if (not window-system) 
    nil
  (progn
    (require 'color-theme)
    (setq favorite-color-themes
          '((color-theme-robin-hood)
            (color-theme-goldenrod)
            (color-theme-comidia)
            (color-theme-charcoal-black)
            (color-theme-clarity) 
            (color-theme-ld-dark) 
            (color-theme-matrix) 
            (color-theme-oswald) 
            (color-theme-kingsajz)
            (color-theme-parus) 
            (color-theme-dark-blue2) 
            (color-theme-gray30) 
            (color-theme-subtle-hacker))) 
    (random t)                          ;set the seed according to the
                                        ;system clock
    (setq current-color-theme 
          (nth (random (length favorite-color-themes)) 
               favorite-color-themes))
    (eval current-color-theme)))


;; (defface ac-menu-face
;;   'default
;;   ;'((t (:background background-color :foreground foreground-color)))
;;   "Face for candidate menu."
;;   :group 'auto-complete)

;; (defface ac-selection-face
;;   'highlight-face
;;   "Face for the selected candidate."
;;   :group 'auto-complete)


;; end colortheme
;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;
;; keybindings 
;; -- never switch to overwrite mode, not even accidentally

(global-set-key [insert] 
  (function 
   (lambda () (interactive) 
     (message "Sorry, overwrite mode has been disabled forever."))))


;; end keybindings
;;;;;;;;;;;;;;;;;;


;;;;;;;;;
;; python

; (add-hook 'python-mode-hook (lambda () (pysmell-mode 1)))

;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; (add-hook 'python-mode-hook
;;                   (lambda ()
;;             (set (make-variable-buffer-local 'beginning-of-defun-function)
;;                  'py-beginning-of-def-or-class)
;;             (setq outline-regexp "def\\|class ")
;;             (local-set-key [return] 'reindent-then-newline-and-indent)
;;                         (eldoc-mode 1)
;;                         (define-key python-mode-map "\"" 'electric-pair)
;;                         (define-key python-mode-map "\'" 'electric-pair)
;;                         (define-key python-mode-map "(" 'electric-pair)
;;                         (define-key python-mode-map "[" 'electric-pair)
;;                         (define-key python-mode-map "{" 'electric-pair)))

;; (mapc (lambda (hook)
;;         (add-hook hook (lambda ()
;;                          (setq show-trailing-whitespace t))))
;;       '(python-mode-hook))

;; (defun ryan-python-tab ()
;;   (interactive)
;;   (if (eql (ac-start) 0)
;;       (indent-for-tab-command)))
 
;; (defadvice ac-start (before advice-turn-on-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) t))
;; (defadvice ac-cleanup (after advice-turn-off-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) nil))
 
;; (define-key python-mode-map "\t" 'ryan-python-tab)


;; ;; Initialize Pymacs

;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)

;; ;; ;; Initialize Rope                                                                                             
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)

;; ;; ;; autocompletion

;; (defun ac-python-find ()
;;   "Python `ac-find-function'."
;;   (require 'thingatpt)
;;   (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
;;     (if (null symbol)
;;         (if (string= "." (buffer-substring (- (point) 1) (point)))
;;             (point)
;;           nil)
;;       symbol)))

;; (defun ac-python-candidate ()
;;   "Python `ac-candidates-function'"
;;   (let (candidates)
;;     (dolist (source ac-sources)
;;       (if (symbolp source)
;;           (setq source (symbol-value source)))
;;       (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
;;              (requires (cdr-safe (assq 'requires source)))
;;              cand)
;;         (if (or (null requires)
;;                 (>= (length ac-target) requires))
;;             (setq cand
;;                   (delq nil
;;                         (mapcar (lambda (candidate)
;;                                   (propertize candidate 'source source))
;;                                 (funcall (cdr (assq 'candidates source)))))))
;;         (if (and (> ac-limit 1)
;;                  (> (length cand) ac-limit))
;;             (setcdr (nthcdr (1- ac-limit) cand) nil))
;;         (setq candidates (append candidates cand))))
;;     (delete-dups candidates)))
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;                  (auto-complete-mode 1)
;;                  (set (make-local-variable 'ac-sources)
;;                       (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
;;                  (set (make-local-variable 'ac-find-function)
;; 		      'ac-python-find)
;;                  (set (make-local-variable 'ac-candidate-function)
;; 		      'ac-python-candidate)
;;                  (set (make-local-variable 'ac-auto-start) nil)))

;; ;; helper function
;; (defun prefix-list-elements (list prefix)
;;   (let (value)
;;     (nreverse
;;      (dolist (element list value)
;;       (setq value (cons (format "%s%s" prefix element) value))))))

;; (defvar ac-source-rope
;;   '((candidates
;;      . (lambda ()
;;          (prefix-list-elements (rope-completions) ac-target))))
;;   "Source for Rope")


;; end of python
;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; org-mode

;; global-keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; ac-completion
(defvar ac-org-candidates nil)
(defvar ac-org-pattern nil)
(defun ac-org-construct-candidates ()
  "Pabbrev source for org."
  (let* ((end (point))
	 (beg1 (save-excursion
		(skip-chars-backward (org-re "[:alnum:]_@"))
		(point)))
	 (beg (save-excursion
	       (skip-chars-backward "a-zA-Z0-9_:$")
	       (point)))
	 (confirm (lambda (x) (stringp (car x))))
	 (searchhead (equal (char-before beg) ?*))
	 (struct
	  (when (and (member (char-before beg1) '(?. ?<))
		     (setq a (assoc (buffer-substring beg1 (point))
				    org-structure-template-alist)))
		(org-complete-expand-structure-template (1- beg1) a)
		(throw 'exit t)))
	 (tag (and (equal (char-before beg1) ?:)
		   (equal (char-after (point-at-bol)) ?*)))
	 (prop (and (equal (char-before beg1) ?:)
		    (not (equal (char-after (point-at-bol)) ?*))))
	 (texp (equal (char-before beg) ?\\))
	 (link (equal (char-before beg) ?\[))
	 (opt (equal (buffer-substring (max (point-at-bol) (- beg 2))
				       beg)
		     "#+"))
	 (startup (string-match "^#\\+STARTUP:.*"
				(buffer-substring (point-at-bol) (point))))
	 (completion-ignore-case opt)
	 (type nil)
	 (tbl nil)
	 (table (cond
		 (opt
		  (setq type :opt)
		  (require 'org-exp)
		  (append
		   (mapcar
		    (lambda (x)
		      (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
		      (cons (match-string 2 x) (match-string 1 x)))
		    (org-split-string (org-get-current-options) "\n"))
		   (mapcar 'list org-additional-option-like-keywords)))
		 (startup
		  (setq type :startup)
		  org-startup-options)
		 (link (append org-link-abbrev-alist-local
			       org-link-abbrev-alist))
		 (texp
		  (setq type :tex)
		  org-html-entities)
		 ((string-match "\\`\\*+[ \t]+\\'"
				(buffer-substring (point-at-bol) beg))
		  (setq type :todo)
		  (mapcar 'list org-todo-keywords-1))
		 (searchhead
		  (setq type :searchhead)
		  (save-excursion
		   (goto-char (point-min))
		   (while (re-search-forward org-todo-line-regexp nil t)
			  (push (list
				 (org-make-org-heading-search-string
				  (match-string 3) t))
				tbl)))
		  tbl)
		 (tag (setq type :tag beg beg1)
		      (or org-tag-alist (org-get-buffer-tags)))
		 (prop (setq type :prop beg beg1)
		       (mapcar 'list (org-buffer-property-keys nil t t)))
		 (t (progn
		     (call-interactively org-completion-fallback-command)
		     (throw 'exit nil))))))
    (setq ac-org-pattern (buffer-substring-no-properties beg end))
    table))

(defvar ac-source-org nil)
(setq ac-source-org
      `((sigil . "o")
	(init . (lambda () (setq ac-org-candidates
				 (condition-case nil
						 (ac-org-construct-candidates)))))
	(candidates . (lambda ()
			(all-completions ac-target ac-org-candidates)))))





;;;;;;;;;;
;; haskell

(require 'ac-haskell)

(defun flymake-Haskell-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-Haskell-cmdline))

(defun flymake-get-Haskell-cmdline (source base-dir)
  (list "flycheck_haskell.pl"
	(list source base-dir)))

(push '(".+\\.hs$" flymake-Haskell-init flymake-simple-java-cleanup)
      flymake-allowed-file-name-masks)
(push '(".+\\.lhs$" flymake-Haskell-init flymake-simple-java-cleanup)
      flymake-allowed-file-name-masks)
(push
 '("^\\(\.+\.hs\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
   1 2 3 4) flymake-err-line-patterns)

;; optional setting
;; if you want to use flymake always, then add the following hook.
(add-hook
 'haskell-mode-hook
  '(lambda ()
    (if (not (null buffer-file-name)) (flymake-mode))))


;; end of haskell
;;;;;;;;;;;;;;;;


;;;;;;;;;;
;; clojure

(add-to-list 'load-path "~/opt/clojure-mode")
(require 'clojure-mode)


(add-to-list 'load-path "~/opt/swank-clojure")
(require 'swank-clojure-autoload) 

(swank-clojure-config
 (setq swank-clojure-jar-path "~/.clojure/clojure.jar") 
 (setq swank-clojure-extra-classpaths 
       (list "~/.clojure/clojure-contrib.jar"))) 

(eval-after-load "slime" 
  '(progn (slime-setup '(slime-repl)))) 

(add-to-list 'load-path "~/opt/slime") 
(require 'slime) 
(slime-setup) 

;;;;;;;;;;;;
;; yasnippet

;; (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(yas/initialize)
(yas/load-directory "~/emacs/site-lisp/packages/snippets/")

;; end of yasnippet
;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;
;; Functions

;; alias for qrr
(defalias 'qrr 'query-replace-regexp)

;; Autocompilation
(defun autocompile()
  "compile itself if ~/.emacs"
  (interactive)
  (if (string= (buffer-file-name) (concat default-directory ".emacs"))
      (byte-compile-file (buffer-file-name))))

(defun insert-date (format) 
        "Wrapper around format-time-string." 
        (interactive "MFormat: ") 
        (insert (format-time-string format)))

(defun insert-standard-date () 
        "Inserts standard date time string." 
        (interactive) 
        (insert (format-time-string "%c")))

(defun electric-pair ()
  "Insert character pair without surronding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

;; end of functions
;;;;;;;;;;;;;;;;;;;







