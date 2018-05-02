(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "pseudocode" "lstlisting")))
 '(LaTeX-verbatim-macros-with-braces (quote ("code")))
 '(LaTeX-verbatim-macros-with-delims (quote ("verb" "verb*")))
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
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
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   (quote
    (("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g %n %o %b"))))
 '(TeX-view-program-selection
   (quote
    ((output-dvi "open")
     (output-pdf "Skim")
     (output-html "open"))))
 '(agda2-include-dirs (quote ("." "/Users/hbugge/Agda/agda-stdlib/src")))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "bf58fd87997ec29aef8edfdd8f4ab854f5389e9d23a63b0835c497987a6c5084" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(global-auto-revert-mode t)
 '(haskell-interactive-popup-errors nil)
 '(package-selected-packages
   (quote
    (attrap attrap-attrap web-mode tide typescript-mode visual-fill-column longlines-mode fill-column-indicator flycheck-elm nyan-mode kotlin-mode ws-butler elm-mode magit neotree multiple-cursors markdown-mode intero flymake-hlint haskell-mode solarized-theme diff-hl expand-region smex ido-vertical-mode flychdeck-elm)))
 '(safe-local-variable-values
   (quote
    ((dante-target . "dil-service:lib")
     (dante-target . "dil-service:test:dil-service-test dil-service:lib")
     (dante-target . "dil-service:lib dil-service:test:dil-service-test")
     (dante-target . "dil-service:test:dil-service-test")
     (dante-project-root . "~/deon-dsl/dil-service")
     (dante-target . "test:dil-service-test")
     (haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4)
     (preview-scale-function . 1.2)
     (eval agda-input-add-translations
           (quote
            (("bi" . "mathbb{I}")
             ("longmapsto" . "longmapsto"))))
     (eval agda-input-add-translations
           (quote
            (("bi" . "mathbb{I}")
             ("longmapsto" . "⟼"))))
     (eval agda-input-add-translations
           (quote
            (("bi" . "𝕀")
             ("longmapsto" . "⟼"))))
     (eval agda-input-add-translations
           (quote
            (("gu" . "⛨")))))))
 '(send-mail-function (quote mailclient-send-it))
 '(visual-line-mode nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-function-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-primitive-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "deep sky blue")))))
