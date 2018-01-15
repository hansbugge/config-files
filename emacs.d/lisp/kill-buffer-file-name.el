(defun kill-buffer-file-name ()
  "Put `buffer-file-name' on top of kill-ring."
  (interactive)
  (kill-new buffer-file-name))
