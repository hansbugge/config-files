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

;; Do not show the GNU splash screen
(setq inhibit-startup-message t)

;; Show matching parentheses
(show-paren-mode t)

;; Show column-number in the mode line
(column-number-mode t)

;; 'y' and 'n', instead of 'yes' and 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Trailing whitespace
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace t))))

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

;; Smex adds ido to M-x
(use-package smex
  :ensure t
  :bind (("M-x" . smex)))

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
  (prog-mode . highlight-symbol-mode))

;; Use spaces instead of tabs, unless specified otherwise
(setq-default indent-tabs-mode nil)

;; Highlight version control changes in the fringe
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1))

;; Solarized color theme
(use-package solarized-theme
  :ensure t
  :if window-system
  :config
  (load-theme 'solarized-dark))
;; Execute the following sexp to disable all themes:
;; (apply 'disable-theme custom-enabled-themes)

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
      (set-face-attribute 'default nil :height 140)

      ;; Highlight current line
      (global-hl-line-mode 1)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; Emacs running in MacOS GUI
      (when (eq window-system 'mac)
        ;; Allow scrolling while doing isearch (buggy)
        (put 'mac-mwheel-scroll 'isearch-scroll t)
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
              ("M-q" . ales/fill-paragraph)))

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
  :commands 'haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package flymake-hlint
  :disabled
  :ensure t
  :hook (haskell-mode . flymake-hlint-load))

(use-package intero
  :disabled
  :ensure t
  :hook (haskell-mode . intero-mode)
  )

(use-package dante
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown mode

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/unmark-next-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neotree

(use-package neotree
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit

(use-package magit
  :ensure t
  :defer t)

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

(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'"
  :init
  (defun my-elm-mode-hook ()
    (setq company-backends '(company-elm))
    (elm-oracle-setup-completion))
  (add-hook 'elm-mode-hook 'my-elm-mode-hook))

(use-package flycheck-elm
  :ensure t
  :after (flycheck elm-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck

(use-package flycheck
  :ensure t
  :hook (elm-mode . flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ws-butler

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company

(use-package company
  :ensure t
  :defer 5 ;; load after 5 seconds of idle time
  :config (global-company-mode t)
  :diminish company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deon CSL

(use-package csl-mode
  :load-path
  "~/deon-dsl/csl-syntax-highlighting/"
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
  (setq js2-strict-missing-semi-warning nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yaml

(use-package yaml-mode
  :ensure t
  :mode
  "\\.yaml\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo tree

(use-package undo-tree
  :ensure t
  :commands undo-tree-mode)
