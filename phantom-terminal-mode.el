(require 'shr)

(defvar phantom-terminal-process-buffer-name "*phantom-terminal: process*")
(defvar phantom-terminal-process-name "phantom-terminal-node")
(defvar phantom-terminal-buffer-name "*phantom-terminal*")
(defvar phantom-terminal-current-process)

(defvar phantom-terminal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "\r")  'phantom-terminal-click)
    (define-key map (kbd "g") 'phantom-terminal-reload-page)
    map))

(define-derived-mode phantom-terminal-mode fundamental-mode
  "PhantomTerm"
  "Major mode for using shr.el to render phantom-terminal stuff"
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
  (let ((current-point (point)))
    (erase-buffer)
    (call-process "pandoc" nil t nil "-f" "html" "-t" "plain" "browsed.html")
    (let ((inhibit-read-only t))
      (while (re-search-forward "\\[\\[C[:digit:]*" nil t)
        (replace-match "BUTTON" nil nil)))
    (goto-char current-point)))

(defun phantom-terminal-reload-page ()
  (interactive)
  (process-send-string phantom-terminal-current-process "R\n")
  (phantom-terminal-load-page))

(defun phantom-terminal-click (id)
  (process-send-string phantom-terminal-current-process (concat "C" id "\n")))

(defun phantom-terminal ()
  "Launch phantom-terminal-mode in a separate buffer"
  (interactive)
  (get-buffer-create phantom-terminal-buffer-name)
  (pop-to-buffer phantom-terminal-buffer-name)
  (phantom-terminal-mode))

(provide 'phantom-terminal-mode)
