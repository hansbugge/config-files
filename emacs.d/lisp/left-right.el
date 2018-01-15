;; make the \left ... \right modified parens easier on the eye
(font-lock-add-keywords
             'latex-mode
             '(("\\\\left(" (0 (put-text-property (match-beginning 0) (match-end 0) 'display "｟")))
               ("\\\\right)" (0 (put-text-property (match-beginning 0) (match-end 0) 'display "｠")))
               ("\\\\left\\[" (0 (put-text-property (match-beginning 0) (match-end 0) 'display "⟦")))
               ("\\\\right\\]" (0 (put-text-property (match-beginning 0) (match-end 0) 'display "⟧")))
               ("\\\\left\\\\{" (0 (put-text-property (match-beginning 0) (match-end 0) 'display "⦃")))
               ("\\\\right\\\\}" (0 (put-text-property (match-beginning 0) (match-end 0) 'display "⦄")))
               ))
