(require 'shr)

(defvar phantom-terminal-process-buffer-name "*phantom-terminal: process*")
(defvar phantom-terminal-process-name "phantom-terminal-node")
(defvar phantom-terminal-buffer-name "*phantom-terminal*")
(defvar phantom-terminal-current-process)
(defvar phantom-terminal-current-buffer)
(defvar phantom-terminal-current-timer)

(defvar phantom-terminal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'phantom-terminal-click)
    (define-key map (kbd "g") 'phantom-terminal-reload-page-at-least-once)
    (define-key map (kbd "o") 'phantom-terminal-goto-url)
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
  (when (equal major-mode 'phantom-terminal-mode)
    (let ((current-point (point)))
      (erase-buffer)
      (call-process "pandoc" nil t nil "-f" "html" "-t" "plain" "browsed.html")
      (let ((inhibit-read-only t))
        (while (re-search-forward "\\[\\[C[:digit:]*" nil t)
          (replace-match "BUTTON" nil nil)))
      (goto-char current-point))))

(defun phantom-terminal-reload-page ()
  (process-send-string phantom-terminal-current-process "R\n")
  (phantom-terminal-load-page))

(defun phantom-terminal-reload-page-at-least-once (&optional repeat-every)
  "Reload the browser display.
   Automatically repeats every REPEAT-EVERY seconds.
   Call with a double prefix argument to stop automatic reloads."
  (interactive "P")
  (phantom-terminal-reload-page)
  (when repeat-every
    (cond ((equal repeat-every '(16))
           (phantom-terminal-clear-reload-timer))
          ((> repeat-every 0)
           (phantom-terminal-reload-every repeat-every)))))

(defun phantom-terminal-reload-every (interval)
  (message "Reloading every %d seconds" interval)
  (setq phantom-terminal-current-timer (run-at-time t interval 'phantom-terminal-reload-page)))

(defun phantom-terminal-clear-reload-timer ()
  (message "Stopping automatic reloads")
  (cancel-timer phantom-terminal-current-timer))

(defun phantom-terminal-click (id)
  (interactive "n")
  (process-send-string phantom-terminal-current-process (concat "C" (number-to-string id) "\n")))

(defun phantom-terminal-goto-url (url)
  (interactive "M")
  (process-send-string phantom-terminal-current-process (concat "O" url "\n"))
  (phantom-terminal-load-page))

(defun phantom-terminal ()
  "Launch phantom-terminal-mode in a separate buffer"
  (interactive)
  (setq phantom-terminal-current-buffer (get-buffer-create phantom-terminal-buffer-name))
  (pop-to-buffer phantom-terminal-buffer-name)
  (phantom-terminal-mode))

(provide 'phantom-terminal-mode)
