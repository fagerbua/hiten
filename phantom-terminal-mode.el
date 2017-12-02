(defvar phantom-terminal-mode-hook nil)

(defvar phantom-terminal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "RET" 'phantom-terminal-click)
    map)
  "Keymap for phantom-terminal major mode")

(defvar phantom-terminal-buffer-name "*phantom-terminal*")
(defvar phantom-terminal-process-buffer-name "*phantom-terminal: process*")
(defvar phantom-terminal-process-name "phantom-terminal-node")

(defun phantom-terminal-mode ()
  "Major mode for browsing using phantom-terminal"
  (interactive)
  (kill-all-local-variables)
  (use-local-map phantom-terminal-mode-map)
  (phantom-terminal-start-browser)
  (phantom-terminal-load-page)
  (setq major-mode 'phantom-terminal-mode)
  (setq mode-name "Phantom-Terminal")
  (run-hooks 'phantom-terminal-mode-hook))

(defun phantom-terminal-start-browser ()
  (when (get-buffer phantom-terminal-process-buffer-name)
    (kill-buffer phantom-terminal-process-buffer-name))
  (start-process phantom-terminal-process-name phantom-terminal-process-buffer-name "node" "index.js"))

(defun phantom-terminal-load-page ()
  (when (get-buffer phantom-terminal-buffer-name)
    (kill-buffer phantom-terminal-buffer-name))
  (eww-open-file "browsed.html")
  (rename-buffer phantom-terminal-buffer-name))

(provide 'phantom-terminal-mode)
