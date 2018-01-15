(defun hans/wrap (char)
  "Wraps region in CHAR. If CHAR is a parenthesis ('(','{','[', etc.) insert fitting closing parenthesis."
  (interactive (list (char-to-string (read-char "Wrap region in: "))))
  (let* ((startendpairs '(("(" . ")")
                          ("[" . "]")
                          ("{" . "}")
                          ("<" . ">")
                          ("]" . "[")))
         (endchar (or (cdr-safe (assoc char startendpairs)) char)))
    (if (use-region-p)
        (insert-pair 0 char endchar)
      (insert char endchar))))
