;;;;; GNU Emacs init file
;;;;; Hans Bugge Grathwohl
;;;;; mail@hansbugge.dk / hansbugge@gmail.com

;;;;;;;;;;;;;;;;;;;;;;
;;; Global settings

;; Initialize package and use-package
(require 'package)
(setq package-enable-at-startup nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
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
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; Avoid popping up a window with compilation warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; Always ask for confirmation when quitting Emacs
;; (it's easy to press C-x-c by accident)
(setq confirm-kill-emacs 'yes-or-no-p)

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

(defvar hans/theme-file (concat user-emacs-directory "theme.el"))

(defun hans/switch-theme (theme persist)
  "Disable all themes, then load THEME"
  (when custom-enabled-themes
    (apply 'disable-theme custom-enabled-themes))
  (when persist
    (with-temp-file hans/theme-file
      (insert (prin1-to-string theme))
      theme))
  (load-theme theme))

(defun get-string-from-file (file-path)
  "Return file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun hans/load-theme ()
  "Load theme from theme.el"
  (let ((saved-theme (intern-soft (get-string-from-file hans/theme-file))))
    (when saved-theme
      (hans/switch-theme saved-theme nil))))

(defun lights-on ()
  "Switch to light theme"
  (interactive)
  (hans/switch-theme 'solarized-light 't))

(defun lights-off ()
  "Switch to dark theme"
  (interactive)
  (hans/switch-theme 'solarized-dark 't))

(defun lights-toggle ()
  "Toggle between lights and dark theme"
  (interactive)
  (if (member 'solarized-light custom-enabled-themes)
      (lights-off)
    (lights-on)))

;; https://www.emacswiki.org/emacs/IncrementNumber
(defun increment-number-at-point (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let ((inc-by (or arg 1)))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (let* ((field-width (- (match-end 0) (match-beginning 0)))
                 (answer (+ (string-to-number (match-string 0) 10) inc-by))
                 (answer (if (< answer 0)
                             (+ (expt 10 field-width) answer)
                           answer)))
            (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                   answer))))))))

(global-set-key (kbd "C-c +") 'increment-number-at-point)

;; Solarized color theme
(use-package solarized-theme
  :ensure t
  :if window-system
  :config
  (hans/load-theme)
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
      ;; At 2x retina scaling, i.e. logical 1080p resolution, 140 is
      ;; plenty (and also nice and crisp).  At higher resolutions
      ;; increments of 1.2 look nice (140, 168, 202)
      (set-face-attribute 'default nil :height 140)

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

        ;; Unbind C-z and C-x C-z which minimizes window
        (global-unset-key (kbd "C-z"))
        (global-unset-key (kbd "C-x C-z"))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs running in a terminal
  (xterm-mouse-mode t)
  (menu-bar-mode 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specifically for MacOS
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My own functions
(use-package wrap
  :load-path "lisp"
  :bind (("M-(" . hans/wrap)))

(use-package kill-buffer-file-name
  :load-path "lisp"
  :commands kill-buffer-file-name)

(defun hans/maximize-frame ()
  "Maximize current frame"
  (interactive)
  (let* ((geometry (frame-monitor-attribute 'geometry))
         (width (- (nth 2 geometry) 35))
         (height (nth 3 geometry))
         (frame (selected-frame)))
    (set-frame-position frame 0 0)
    (set-frame-size frame width height 't)))

(defun hans/half-size-frame ()
  (interactive)
  (let* ((geometry (frame-monitor-attribute 'geometry))
         (width (/ (nth 2 geometry) 2))
         (height (nth 3 geometry))
         (frame (selected-frame)))
    (set-frame-position frame 0 0)
    (set-frame-size frame width height 't)))

;; Overrides compose-mail-other-frame, but I'll survive
(global-set-key (kbd "C-x 5 m") #'hans/maximize-frame)
(global-set-key (kbd "C-x 5 h") #'hans/half-size-frame)
(global-set-key (kbd "C-x 5 f") #'toggle-frame-fullscreen)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aleš' functions

(use-package fill-paragraph
  :load-path "lisp"
  :bind (:map
         LaTeX-mode-map ("M-q" . ales/fill-paragraph)
         :map
         markdown-mode-map ("M-q" . ales/fill-paragraph)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Agda

(defun program-exists-in-path (program)
  "Check if PROGRAM is available"
  (zerop (call-process "command" nil nil nil "-v" program)))

;; (when (program-exists-in-path "agda-mode")
;;   (load-file (let ((coding-system-for-read 'utf-8))
;;                (shell-command-to-string "agda-mode locate")))

;;   ;; I want to load the agda input mode even if I'm not doing agda
;;   (load-file (let ((coding-system-for-read 'utf-8))
;;                (let ((str (shell-command-to-string "agda-mode locate")))
;;                  ;; `agda-mode locate' almost gives the right path, we just need
;;                  ;; to change the filename
;;                  (when (string-match "agda2.el" str)
;;                    (replace-match "agda-input.el" 1 nil str)))))

;;   (set-input-method "Agda")
;;   (toggle-input-method))

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
    (let ((narrowed-buffer-length       ; we save the resulting buffer length
                                        ; so we can indent correctly in the end.
           (save-restriction
             (narrow-to-region b e)
             (goto-char (point-min))
             (insert "\\[\n")
             (goto-char (point-max))
             (insert "\n\\]")
             (- (point-max) (point-min)))))
      (indent-region b (+ b narrowed-buffer-length))
      (when (= b e)                     ; we have not selected a region, so
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

;; (let ((petite-path "/usr/local/bin/petite"))
;;   (when (program-exists-in-path petite-path)
;;     (setq scheme-program-name petite-path)
;;     (load-file "~/.emacs.d/scheme-setup.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell stuff

(use-package haskell-mode
  :ensure t
  :disabled
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
  :disabled
  :ensure t
  :hook (haskell-mode . intero-mode))

(use-package helm-hoogle
  :ensure t
  :disabled
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
  :disabled
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

;; At the moment (2022-04-19) the following issue prevents me from using forge:
;; https://github.com/magit/ghub/issues/81
;; (use-package forge
;;   :init
;;   ;; https://magnus.therning.org/2021-12-08-magit_forge-and-self-hosted-gitlab.html
;;   ;; Dependency:
;;   ;; brew install gnupg
;;   (setq
;;    forge-alist '(("gitlab.deondigital.com" "gitlab.deondigital.com/api/v4"
;;                   "gitlab.deondigital.com" forge-gitlab-repository)
;;                  ("github.com" "api.github.com"
;;                   "github.com" forge-github-repository)
;;                  ("gitlab.com" "gitlab.com/api/v4"
;;                   "gitlab.com" forge-gitlab-repository)))
;;   :after magit)

(use-package gitignore-mode
  :ensure t
  :mode ("\\.gitignore"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cubical type theory mode

;; (use-package cubicaltt
;;   :load-path "~/cubicaltt"
;;   :mode ("\\.ctt\\'" . ctt-mode))

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
  :disabled
  :after (flycheck))

(use-package elm-mode
  :ensure t
  :disabled
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
         (javascript-mode . flycheck-mode)
         (web-mode . flycheck-mode)
         (sh-mode . flycheck-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ws-butler

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :hook ((prog-mode . ws-butler-mode)
         (markdown-mode . ws-butler-mode)
         (yaml-mode . ws-butler-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company

(use-package company
  :ensure t
  :defer 5 ;; load after 5 seconds of idle time
  :config
  (global-company-mode t)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; Cmd is H (hyper) in Yamamoto Emacs and s (super) in GNU Emacs 28
  :bind (("H-C-c" . 'company-manual-begin)
         ("C-s-c" . 'company-manual-begin))
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
  :disabled
  :commands nyan-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Idris

(use-package idris-mode
  :ensure t
  :disabled
  :mode
  "\\.idr\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript / React

(use-package rjsx-mode
  :ensure t
  :disabled
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
         (js-mode . tide-setup)
         (web-mode . tide-setup))
  :init
  (defun my-tide-mode-hook ()
    (tide-hl-identifier-mode 1)
    (setup-indent-with-two-spaces))
  (add-hook 'tide-mode-hook 'my-tide-mode-hook)
  :config
  (setq flycheck-javascript-eslint-executable "eslint")
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prettier

(use-package prettier-js
  :ensure t
  :init
  ;; https://github.com/prettier/prettier-emacs/issues/29
  (defun maybe-use-prettier ()
    "Enable prettier-js-mode if an rc file is located."
    (if (locate-dominating-file default-directory ".prettierrc")
        (prettier-js-mode +1)))

  :hook
  ((typescript-mode . maybe-use-prettier)
   (web-mode . maybe-use-prettier)
   (javascript-mode . maybe-use-prettier)))

;; (use-package prettier-js
;;   :ensure t
;;   :disabled
;;   :commands prettier-js
;;   :config
;;   (setq prettier-js-command "npx")
;;   (setq prettier-js-args '("prettier")))

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
  :disabled
  :mode
  "\\.dhall\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo tree

(use-package undo-tree
  :ensure t
  :disabled
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
  :disabled
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
  :disabled
  :mode "\\.fs\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eglot (Emacs Polyglot)

;;; Some weird bug in eglot makes eldoc not work in cider when eglot is loaded
;;; Related: https://github.com/joaotavora/eglot/issues/534
(use-package eglot
  :disabled
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(csl-mode . ("language-server"))))

;; (use-package eglot
;;   :ensure t
;;   :pin melpa
;;   :config
;;   (add-to-list
;;    'eglot-server-programs
;;    '(csl-mode . ("language-server"
;;                  ;; Perhaps not necessary any more
;;                  ;; "--csl-std-lib"
;;                  ;; "/Users/hansbugge/deon-dsl/cslstdlib/StdLib.csl"
;;                  ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp-mode (alternative to eglot)

(use-package lsp-mode
  ;; :disabled
  :ensure t
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp)
         (python-mode . lsp))
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-auto-configure t)
  (setq lsp-eldoc-enable-hover nil) ; to avoid conflicting with CIDER eldoc
  (setq lsp-lens-enable t)
  (add-to-list 'lsp-enabled-clients 'clojure-lsp)
  (setq lsp-enable-indentation nil) ; clojure-lsp runs cljfmt on indent which is too aggresive
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.depscache\\'")
  (setq lsp-signature-auto-activate nil)
  ;; problem with "Directory [...] does not exist. Create?"  seems to
  ;; be fixed by blacklisting the depscache from LSP.  I don't yet
  ;; know how to do that in this config.
  )

(use-package lsp-ui
  ;; :disabled
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil))

;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sml-mode
(use-package sml-mode
  :ensure t
  :disabled
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
  :ensure t
  :disabled)

(use-package gradle-mode
  :ensure t
  :disabled)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Htmlize

(use-package htmlize
  :disabled
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
    (when (buffer-file-name) (flycheck-mode)))
  (add-hook 'clojure-mode-hook 'my-clojure-mode-hook))

;; try it everywhere:
;; (electric-pair-mode 1)

(use-package cider
  :ensure t
  :config
  (setq cider-offer-to-open-cljs-app-in-browser nil)
  ;; :init
  ;; (add-hook 'cider-connected-hook #'cider-upgrade-nrepl-connection)
  )

(use-package rainbow-delimiters
  :ensure t
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package hungry-delete
  :ensure t
  :config
  (setq hungry-delete-join-reluctantly 't)
  ;; (setq hungry-delete-chars-to-skip " \t\n\r\f\v")
  ;; (add-to-list hungry-delete-except-modes 'foo-mode)
  (global-hungry-delete-mode 1))

;; Smartparens
(use-package smartparens :ensure t
  :hook ((emacs-lisp-mode . smartparens-strict-mode)
         (scheme-mode . smartparens-strict-mode)
         (clojure-mode . smartparens-strict-mode))
  :bind (("DEL" . sp-backward-delete-char)
         ("C-d" . sp-delete-char))
  :config
  ;; Automatically insert newlines when pressing enter like so:
  ;; {|} =RET=>
  ;; {
  ;;   |
  ;; }
  (dolist (open '("{" "["))
    (sp-with-modes
        '(typescript-mode web-mode javascript-mode)
      (sp-local-pair open nil :post-handlers '(:add ("||\n[i]" "RET"))))))

(require 'smartparens-config)
(smartparens-global-mode +1)

;
; https://github.com/Fuco1/smartparens/issues/633
(defun bso/delete-region-if-should (oldfun &rest r)
  (if (and delete-active-region (region-active-p))
      (sp-kill-region (mark) (point))
    (apply oldfun r)))
(advice-add 'sp-backward-delete-char :around #'bso/delete-region-if-should)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "Smart" backspaces a la intellij

;; ;; Inspired by:
;; ;; https://emacs.stackexchange.com/a/55220
;; (defun smart-backspace ()
;;   (interactive)
;;   (if (save-excursion
;;         (goto-char (line-beginning-position))
;;         (looking-at-p "[[:space:]]*$"))
;;       (progn
;;         (join-line)
;;         (indent-according-to-mode))
;;       (let ((smart-backspace-mode nil))
;;         (command-execute (or
;;                           (key-binding (this-single-command-keys))
;;                           'delete-backward-char)))))

;; (defvar smart-backspace-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "DEL") 'smart-backspace)
;;     map))

;; (define-minor-mode smart-backspace-mode
;;   "When pressing backspace on a blank line, join with previous line.
;; Otherwise invoke normal backspace function."
;;   :global t)

(use-package aggressive-indent
  :ensure t
  :hook ((clojure-mode . aggressive-indent-mode))
  ;; :config
  ;; ;; Prevent aggresive-indent from ruining indentation in the rest of
  ;; ;; the file when accidentally inserting a closing bracket too much
  ;; (add-to-list
  ;;  'aggressive-indent-dont-indent-if
  ;;  '(and (derived-mode-p 'clojure-mode)
  ;;        (null (thing-at-point 'sexp))))
  )

(use-package flycheck-clj-kondo
  ;; :disabled t
  :ensure t)

(use-package clj-refactor
  :disabled
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired+

;; (use-package dired+
;;   :load-path "lisp")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wgrep - Writable grep buffers

(use-package wgrep
  :ensure t)

;;;;;;;;;;;;;
;; scss

(use-package scss-mode
  :ensure t)

;;;;;;;;;;;;;;;
;; dockerfile-mode

(use-package dockerfile-mode
  :ensure t)

;;;;;;;;;;;;;;;
;; keycast-mode

(use-package keycast
  :ensure t)

;;;;;;;;;;;;;;;
;; windmove (from prelude)
;; use shift + arrow keys to switch between visible buffers

(use-package windmove
  :config
  (windmove-default-keybindings))

;;;;;;;;;;;;;;;
;; avy
;; https://github.com/abo-abo/avy
;; https://karthinks.com/software/avy-can-do-anything/

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char-timer)
         :map clojure-mode-map
         ("C-:" . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-:" . avy-isearch)))

;;;;;;;;;;;;;;;;
;; python
(use-package eval-sexp-fu
  :ensure t)

(use-package elpy
  :disabled
  :ensure t
  :init
  (eval-sexp-fu-flash-mode)
  (elpy-enable)
  :config
  ;; (setq-default indent-tabs-mode nil)
  ;; (setq-default tab-width 4)
  ;; (setq indent-line-function 'insert-tab)
  (setq python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --existing --simple-prompt"
        python-shell-prompt-detect-failure-warning nil
        elpy-get-info-from-shell t)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  :bind (:map elpy-mode-map
              ("C-c C-v" . nil)
              ("C-c C-v C-v" . elpy-shell-send-statement)
              ("C-c C-v C-r" . elpy-shell-send-region-or-buffer)
              ("C-c C-c" . elpy-shell-send-group)
              ("C-c C-k" . elpy-shell-send-buffer)))

(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi))
  (setq lsp-jedi-executable-command "/Users/hansbugge/.local/bin/jedi-language-server"))
