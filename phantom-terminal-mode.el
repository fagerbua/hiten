(require 'eww)

(defvar phantom-terminal-process-buffer-name "*phantom-terminal: process*")
(defvar phantom-terminal-process-name "phantom-terminal-node")
(defvar phantom-terminal-current-process)

(define-minor-mode eww-phantom-terminal-mode
  "Minor mode for using eww to render phantom-terminal stuff"
  :lighter " eww-phantom"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "\r")  'phantom-terminal-click)
            (define-key map (kbd "g") 'phantom-terminal-reload-page)
            map)
  (phantom-terminal-start-browser)
  (phantom-terminal-load-page))

(defun phantom-terminal-start-browser ()
  (when (get-buffer phantom-terminal-process-buffer-name)
    (kill-buffer phantom-terminal-process-buffer-name))
  (when (get-process phantom-terminal-process-name)
    (kill-process phantom-terminal-process-name))
  (setq phantom-terminal-current-process
        (start-process phantom-terminal-process-name phantom-terminal-process-buffer-name "node" "index.js")))

(defun phantom-terminal-load-page ()
  (eww-open-file "browsed.html")
  (let ((inhibit-read-only t))
    (while (re-search-forward "\\[\\[C[:digit:]*" nil t)
      (replace-match "BUTTON" nil nil))))

(defun phantom-terminal-reload-page ()
  (process-send-string phantom-terminal-current-process "R\n")
  (phantom-terminal-load-page))

(defun phantom-terminal-click (id)
  (process-send-string phantom-terminal-current-process (concat "C" id "\n")))
(provide 'eww-phantom-terminal-mode)
