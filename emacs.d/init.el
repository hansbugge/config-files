;;;;; GNU Emacs init file
;;;;; Hans Bugge Grathwohl
;;;;; hansbugge@gmail.com

;;;;;;;;;;;;;;;;;;;;;;
;;; Global settings

;; Initialize package and use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Get PATH variable from shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Libraries
(use-package diminish :ensure t :demand t)

;; Subtle bell function
;; From https://www.emacswiki.org/emacs/AlarmBell#toc3
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer
           0.1
           nil
           (lambda (fg) (set-face-foreground 'mode-line fg))
           orig-fg))))

;; Do not show the GNU splash screen
(setq inhibit-startup-message t)

;; Opposite of other-windows
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

;; Backwards mark-sexp
(global-set-key (kbd "C-M-S-SPC") (lambda () (interactive (mark-sexp -1))))

;; Show matching parentheses
(show-paren-mode t)

;; Show column-number in the mode line
(column-number-mode t)

;; 'y' and 'n', instead of 'yes' and 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Trailing whitespace
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda ()
                   (setq show-trailing-whitespace t)
                   (font-lock-add-keywords
                    nil
                    '(("\t" 0 'trailing-whitespace prepend))))))

;; Truncate long lines
(add-hook 'prog-mode-hook
          (lambda () (toggle-truncate-lines t)))

;; Better commenting-tool
(use-package comment-dwim-2
  :ensure t
  :bind (("M-;" . comment-dwim-2)))

;; External "custom" file
(setq custom-file "~/.emacs.d/custom-gnu.el")
(load custom-file)

;; Store all backup files (filenames ending in tilde) in the tmp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;; ido-completion
(icomplete-mode 1)
(setq ido-enable-flex-matching t)
(ido-mode 1)
(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; The only thing from cua-mode I want:
(global-set-key (kbd "<C-return>") 'cua-rectangle-mark-mode)

;; Shortcuts to certain files using registers
;; E.g. `C-x r j e` for visiting init.el
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?n '(file . "~/notes.org"))

;; Attempt to avoid crash bug in version
;; GNU Emacs 26.1 (build 1, x86_64-apple-darwin13.4.0, Carbon Version 157 AppKit 1265.21) of 2018-06-18
;; <C-tab> and <C-S-tab> were set to `mac-next-tab-or-toggle-tab-bar` and `mac-previous-tab-or-toggle-tab-bar`
;; respectively, which I suspect has been leading to the "bear trap for: <rdar://problem/20935868>" crash.

(global-unset-key (kbd "<C-tab>"))
(global-unset-key (kbd "<C-S-tab>"))

;; Indicate empty lines (end of buffer)
(setq-default indicate-empty-lines t)

;; Smex adds ido to M-x
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-M-x" . execute-extended-command)))

;; Expand region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

;; Highlighting symbol under point
(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :hook
  (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.4))

;; Use spaces instead of tabs, unless specified otherwise
(setq-default indent-tabs-mode nil)

;; Indentation helpers

(defun my-setup-indent (n)
  ;; java/c/c++
  (setq-local standard-indent n)
  (setq-local c-basic-offset n)
  ;; web development
  (setq-local js-indent-level n) ; js-mode
  (setq-local sgml-basic-offset n) ; sgml-mode (used for JSX)
  (setq-local web-mode-attr-indent-offset n) ; web-mode
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-sql-indent-offset n) ; web-mode
  (setq-local web-mode-attr-value-indent-offset n) ; web-mode
  (setq-local typescript-indent-level n) ; typescript-mode
  (setq-local css-indent-offset n) ; css-mode
  (setq-local tide-format-options `(:indentSize ,n :tabSize ,n))
  (setq-local sh-basic-offset n)) ; shell scripts

(defun setup-indent-with-two-spaces ()
  (interactive)
  (my-setup-indent 2)
  (message "Tab size set to 2"))

(defun setup-indent-with-four-spaces ()
  (interactive)
  (my-setup-indent 4)
  (message "Tab size set to 4"))

;; Highlight version control changes in the fringe
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1))

(defun hans/switch-theme (theme)
  "Disable all themes, then load THEME"
  (apply 'disable-theme custom-enabled-themes)
  (load-theme theme))

(defun lights-on ()
  "Switch to light theme"
  (interactive)
  (hans/switch-theme 'solarized-light))

(defun lights-off ()
  "Switch to dark theme"
  (interactive)
  (hans/switch-theme 'solarized-dark))

;; https://www.emacswiki.org/emacs/IncrementNumber
(defun increment-number-at-point (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(global-set-key (kbd "C-c +") 'increment-number-at-point)

;; Solarized color theme
(use-package solarized-theme
  :ensure t
  :if window-system
  :config
  (load-theme 'solarized-dark)
  ;; For some reason solarized does not set
  ;; "company-tooltip-selection".  This is a temporary hack until that
  ;; is solved.
  (custom-set-faces '(company-tooltip-selection
                      ((t (:foreground "#00736F" :background "#69CABF"))))))

;; Narrowing
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(if window-system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs running in a GUI
    (progn
      ;; Hide the tool-bar
      (tool-bar-mode 0)

      ;; Font
      (set-face-attribute 'default nil :height 168) ;; 140 * 1.2 = 168

      ;; Highlight current line
      (global-hl-line-mode 1)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; Emacs running in MacOS GUI
      (when (eq window-system 'mac)
        ;; Allow scrolling while doing isearch (buggy)
        ;; (put 'mac-mwheel-scroll 'isearch-scroll t)
        (setq isearch-allow-scroll t)

        ;; Meta is option key
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'hyper)

        ;; Aquamacs-style "cua" keybindings
        (global-set-key [(hyper a)] 'mark-whole-buffer)
        (global-set-key [(hyper v)] 'yank)
        (global-set-key [(hyper c)] 'kill-ring-save)
        (global-set-key [(hyper x)] 'kill-region)
        (global-set-key [(hyper s)] 'save-buffer)
        (global-set-key [(hyper l)] 'goto-line)
        (global-set-key [(hyper z)] 'undo)
        (global-set-key [(hyper q)] 'save-buffers-kill-terminal)

        ;; Unbind C-z which minimizes window
        (global-unset-key (kbd "C-z"))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs running in a terminal
  (xterm-mouse-mode t)
  (menu-bar-mode 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My own functions
(use-package wrap
  :load-path "lisp"
  :bind (("M-(" . hans/wrap)))

(use-package kill-buffer-file-name
  :load-path "lisp"
  :commands kill-buffer-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aleš' functions

(use-package fill-paragraph
  :load-path "lisp"
  :bind (:map LaTeX-mode-map
         ("M-q" . ales/fill-paragraph)
         :map markdown-mode-map
         ("M-q" . ales/fill-paragraph )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Agda

(defun program-exists-in-path (program)
  "Check if PROGRAM is available"
  (zerop (call-process "command" nil nil nil "-v" program)))

(when (program-exists-in-path "agda-mode")
  (load-file (let ((coding-system-for-read 'utf-8))
	       (shell-command-to-string "agda-mode locate")))

  ;; I want to load the agda input mode even if I'm not doing agda
  (load-file (let ((coding-system-for-read 'utf-8))
	       (let ((str (shell-command-to-string "agda-mode locate")))
		 ;; `agda-mode locate' almost gives the right path, we just need
		 ;; to change the filename
		 (when (string-match "agda2.el" str)
		   (replace-match "agda-input.el" 1 nil str)))))

  (set-input-method "Agda")
  (toggle-input-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rearranging lines
;;; http://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "<M-up>")  'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX

(use-package tex-mode
  :mode "\\.tex\\'"
  :bind (:map latex-mode-map
              ("C-c $" . LaTeX-displaymath-environment))
  :config
  (defun LaTeX-displaymath-environment (b e)
    "Wraps selection in \\ [ (without space) switches."
    (interactive
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list (point) (point))))
    (let ((narrowed-buffer-length   	; we save the resulting buffer length
					; so we can indent correctly in the end.
           (save-restriction
             (narrow-to-region b e)
             (goto-char (point-min))
             (insert "\\[\n")
             (goto-char (point-max))
             (insert "\n\\]")
             (- (point-max) (point-min)))))
      (indent-region b (+ b narrowed-buffer-length))
      (when (= b e) 			; we have not selected a region, so
					; place the cursor in ready position.
        (previous-line)
        (indent-for-tab-command)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual line mode (word wrapping)

;; I prefer logical navigation to visual
(define-key visual-line-mode-map [remap kill-line] nil)
(define-key visual-line-mode-map [remap move-beginning-of-line] nil)
(define-key visual-line-mode-map [remap move-end-of-line]  nil)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual fill column

(use-package visual-fill-column
  :ensure t
  :defer t
  :config
  (setq-default fill-column 120))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell

(use-package flyspell
  :ensure t
  :defer t
  :config
  (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
  (define-key flyspell-mouse-map [mouse-3] #'undefined)
  (define-key flyspell-mode-map (kbd "C-c $") nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme

(let ((petite-path "/usr/local/bin/petite"))
  (when (program-exists-in-path petite-path)
    (setq scheme-program-name petite-path)
    (load-file "~/.emacs.d/scheme-setup.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell stuff

(use-package haskell-mode
  :ensure t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :config
  ;; ignore the stack-generated dump-hi files when browsing with C-x C-f
  (add-to-list 'ido-ignore-files "\\.dump-hi\\'")
  ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  )

(use-package flymake-hlint
  :disabled
  :ensure t
  :hook (haskell-mode . flymake-hlint-load))

(use-package intero
  :ensure t
  :hook (haskell-mode . intero-mode))

(use-package helm-hoogle
  :ensure t
  :after haskell-mode)

(use-package dante
  :disabled
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (defun my-dante-haskell-mode-hook ()
    (dante-mode 1)
    (flycheck-mode 1))
  (add-hook 'haskell-mode-hook 'my-dante-haskell-mode-hook)
  :config
  (defun my-dante-mode-hook ()
    (flycheck-add-next-checker 'haskell-dante
                               '(warning . haskell-hlint)))
  (add-hook 'dante-mode-hook 'my-dante-mode-hook))

;; ATtempt to Repair At Point / jyp
(use-package attrap
  :ensure t
  :after dante-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown mode

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"
  :init (setq markdown-command "pandoc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/unmark-next-like-this)
         ("C-M->" . mc/skip-to-next-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neotree

(use-package neotree
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
  :config
  (defun magit-reset-master-to-origin ()
    (interactive)
    (magit-branch-reset "master" "origin/master"))
  ;;;; For older versions of magit that used magit-popup instead of transient:
  ;; (magit-define-popup-action 'magit-fetch-popup ?x
  ;;   "reset master to origin/master" 'magit-reset-master-to-origin)
  (transient-append-suffix 'magit-fetch "m"
    '("x" "reset master to origin/master" magit-reset-master-to-origin)))

(use-package gitignore-mode
  :ensure t
  :mode ("\\.gitignore"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cubical type theory mode

(use-package cubicaltt
  :load-path "~/cubicaltt"
  :mode ("\\.ctt\\'" . ctt-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ocaml

;; (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;;   (when (and opam-share (file-directory-p opam-share))
;;     ;; Register Merlin
;;     (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;     (autoload 'merlin-mode "merlin" nil t nil)
;;     ;; Automatically start it in OCaml buffers
;;     (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;     (add-hook 'caml-mode-hook 'merlin-mode t)
;;     ;; Use opam switch to lookup ocamlmerlin binary
;;     (setq merlin-command 'opam)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elm

(use-package flycheck-elm
  :ensure t
  :after (flycheck))

(use-package elm-mode
  :ensure t
  :after (flycheck-elm)
  :mode "\\.elm\\'"
  :init
  (defun my-elm-mode-hook ()
    (flycheck-elm-setup)
    (setq company-backends '(company-elm))
    (elm-oracle-setup-completion))
  (add-hook 'elm-mode-hook 'my-elm-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck

(use-package flycheck
  :ensure t
  :hook ((elm-mode . flycheck-mode)
         (typescript-mode . flycheck-mode)
         (web-mode . flycheck-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ws-butler

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :hook ((prog-mode . ws-butler-mode)
         (markdown-mode . ws-butler-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company

(use-package company
  :ensure t
  :defer 5 ;; load after 5 seconds of idle time
  :config
  (global-company-mode t)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  :bind ("H-C-c" . 'company-manual-begin)
  :diminish company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deon CSL

(use-package csl-mode
  :load-path
  "~/deon-dsl/csl-syntax-highlighting/emacs/"
  :mode
  "\\.csl\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kotlin

(use-package kotlin-mode
  :ensure t
  :mode
  "\\.kt\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nyan cat

(use-package nyan-mode
  :ensure t
  :commands nyan-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Idris

(use-package idris-mode
  :ensure t
  :mode
  "\\.idr\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript / React

(use-package rjsx-mode
  :ensure t
  :mode
  "\\.js\\'"
  :config
  (setq js2-strict-missing-semi-warning nil)
  ;; (add-hook 'js-mode-hook 'setup-indent-with-four-spaces)
  )

(use-package typescript-mode
  :ensure t
  :mode
  "\\.ts\\'")

(use-package web-mode
  :ensure t
  :mode
  "\\.tsx\\'"
  :config
  (defun my-web-mode-hook ()
    (setq web-mode-enable-auto-quoting nil)
    ;; (setq indent-line-function 'typescript-indent-line)
    (highlight-symbol-mode 0)
    )
  (add-hook 'web-mode-hook 'my-web-mode-hook))

(use-package tide
  :ensure t
  :hook ((typescript-mode . tide-setup)
         (web-mode . tide-setup))
  :init
  (defun my-tide-mode-hook ()
    (tide-hl-identifier-mode 1))
  (add-hook 'tide-mode-hook 'my-tide-mode-hook)
  :config
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prettier

(use-package prettier-js
  :ensure t
  :commands prettier-js
  :config
  (setq prettier-js-command "npx")
  (setq prettier-js-args '("prettier")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yaml

(use-package yaml-mode
  :ensure t
  :mode
  "\\.yaml\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dhall
(use-package dhall-mode
  :ensure t
  :mode
  "\\.dhall\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo tree

(use-package undo-tree
  :ensure t
  :commands undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fill column indicator

(use-package fill-column-indicator
  :ensure t
  :commands fci-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; God mode
(use-package god-mode
  :ensure t
  :bind ("<escape>" . god-local-mode)
  :defines saved-god-mode-line-faces
  :config
  (defun god-mode-line-on ()
    "Alter the mode-line display. Intended to be used when in `god-mode'."
    (setq-local saved-god-mode-line-faces
                (cons (face-attribute 'mode-line :background)
                      (face-attribute 'mode-line :foreground)))
    (set-face-foreground 'mode-line "#b58900")
    (message "Entering god mode..."))
  (defun god-mode-line-off ()
    "Revert back to ordinary mode-line display."
    (when saved-god-mode-line-faces
      (set-face-background 'mode-line (car saved-god-mode-line-faces))
      (set-face-foreground 'mode-line (cdr saved-god-mode-line-faces)))
    (message "Exiting god mode..."))
  (add-hook 'god-mode-enabled-hook 'god-mode-line-on)
  (add-hook 'god-mode-disabled-hook 'god-mode-line-off)
  (define-key god-local-mode-map (kbd ".") 'repeat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fsharp-mode
(use-package fsharp-mode
  :ensure t
  :mode "\\.fs\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eglot (Emacs Polyglot)

(use-package eglot
  :ensure t
  :pin melpa
  :config
  (add-to-list
   'eglot-server-programs
   '(csl-mode . ("language-server"
                 "--csl-std-lib"
                 "/Users/hansbugge/deon-dsl/cslstdlib/StdLib.csl"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sml-mode
(use-package sml-mode
  :ensure t
  :mode "\\.sml\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restclient-mode

(use-package restclient
  :ensure t
  :commands 'restclient-mode
  :mode ("\\.rest\\'" . restclient-mode)
  :init
  (defun my-restclient-mode-hook ()
    (setq-local company-backends '(company-restclient)))
   (add-hook 'restclient-mode-hook 'my-restclient-mode-hook)
  :config
  (add-to-list 'restclient-content-type-modes '("application/edn" . clojure-mode)))

(use-package company-restclient
  :ensure t
  :after restclient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gradle

(use-package groovy-mode
  :ensure t)

(use-package gradle-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Htmlize

(use-package htmlize
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editor config

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
  :diminish editorconfig-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure

(use-package clojure-mode
  :ensure t
  :after flycheck
  :bind (("C-M-<backspace>" . kill-backward-up-list))
  :init
  (defun my-clojure-mode-hook ()
    (electric-pair-local-mode 1)
    (if (buffer-file-name) (flycheck-mode))
    (eldoc-mode 1))
  (add-hook 'clojure-mode-hook 'my-clojure-mode-hook))

(use-package cider
  :ensure t
  :config
  (setq cider-offer-to-open-cljs-app-in-browser nil))

(use-package rainbow-delimiters
  :ensure t
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package aggressive-indent
  :ensure t
  :hook ((clojure-mode . aggressive-indent-mode)))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clj-refactor
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired+

(use-package dired+
  :load-path "lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nix

(use-package nix-mode
  :ensure t)

(use-package nix-buffer
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; csv-mode

(use-package csv-mode
  :ensure t)
