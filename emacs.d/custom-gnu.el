(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-verbatim-environments '("verbatim" "verbatim*" "pseudocode" "lstlisting"))
 '(LaTeX-verbatim-macros-with-braces '("code"))
 '(LaTeX-verbatim-macros-with-delims '("verb" "verb*"))
 '(TeX-command-list
   '(("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("XeLatexMk" "latexmk -pv -pdflatex=\"xelatex --synctex=1 %O %S\" -pdf -dvi- -ps- %t" TeX-run-TeX nil
      (latex-mode))
     ("LatexMk" "latexmk -pv %t" TeX-run-TeX nil
      (latex-mode))
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %(extraopts) %`%S%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
     ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g %n %o %b")))
 '(TeX-view-program-selection
   '((output-dvi "open")
     (output-pdf "Skim")
     (output-html "open")))
 '(agda2-include-dirs '("." "/Users/hbugge/Agda/agda-stdlib/src"))
 '(custom-safe-themes
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "bf58fd87997ec29aef8edfdd8f4ab854f5389e9d23a63b0835c497987a6c5084" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(elm-compile-arguments '("--yes" "--warn" "--output=target/elm.js"))
 '(global-auto-revert-mode t)
 '(gradle-mode nil)
 '(haskell-interactive-popup-errors nil)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(forge hungry-delete lsp-jedi elpy eval-sexp-fu avy avu lsp-ui smartparens company-lsp keycast keycast-mode z3-mode scss-mode wgrep undo-tree yaml-mode rjsx-mode idris-mode gitignore-mode csv-mode nix-buffer nix-mode flycheck-clj-kondo aggressive-indent aggresive-indent aggresive-indent-mode rainbow-delimiters cider clojure-mode clojure prettier-js editorconfig htmlize htlmize groovy-mode gradle-mode gradle company-restclient restclient restclient-mode dhall-mode sml-mode fsharp-mode god-mode helm-hoogle dockerfile-mode attrap-attrap web-mode tide typescript-mode visual-fill-column longlines-mode fill-column-indicator flycheck-elm nyan-mode kotlin-mode ws-butler magit neotree multiple-cursors markdown-mode intero flymake-hlint haskell-mode solarized-theme diff-hl expand-region smex ido-vertical-mode flychdeck-elm))
 '(safe-local-variable-values
   '((eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (dockerfile-image-name . "rs-core-cordapp")
     (cider-shadow-default-options . ":app")
     (cider-default-cljs-repl . shadow)
     (cider-shadow-cljs-default-options . ":app")
     (intero-targets "sic:lib" "sic:test:sic-test")
     (intero-targets "language-lib:lib" "language-lib:test:dil-service-test")
     (intero-targets)
     (intero-targets "language-lib:test:dil-service-test")
     (TeX-master . "../thesis")
     (intero-targets "dil-service:test:dil-service-test")
     (dante-target . "dil-service:lib")
     (dante-target . "dil-service:test:dil-service-test dil-service:lib")
     (dante-target . "dil-service:lib dil-service:test:dil-service-test")
     (dante-target . "dil-service:test:dil-service-test")
     (dante-project-root . "~/deon-dsl/dil-service")
     (dante-target . "test:dil-service-test")
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)
     (preview-scale-function . 1.2)
     (eval agda-input-add-translations
           '(("bi" . "mathbb{I}")
             ("longmapsto" . "longmapsto")))
     (eval agda-input-add-translations
           '(("bi" . "mathbb{I}")
             ("longmapsto" . "⟼")))
     (eval agda-input-add-translations
           '(("bi" . "𝕀")
             ("longmapsto" . "⟼")))
     (eval agda-input-add-translations
           '(("gu" . "⛨")))))
 '(send-mail-function 'mailclient-send-it)
 '(visual-line-mode nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-function-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-primitive-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "deep sky blue"))))
 '(company-tooltip-selection ((t (:foreground "#00736F" :background "#69CABF")))))
