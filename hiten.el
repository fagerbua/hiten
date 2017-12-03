;;; hiten-mode.el --- Emacs frontend for headless Chrome -*- lexical-binding: t -*-

;; Copyright (C) 2017         Jarle Fagerheim

;; Author: Jarle Fagerheim <jarle@fagerbua.no>
;; URL: http://github.com/fagerbua/hiten

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar hiten-process-buffer-name "*hiten: process*")
(defvar hiten-process-name "hiten-node")
(defvar hiten-buffer-name "*hiten*")
(defvar hiten-current-process)
(defvar hiten-current-buffer)
(defvar hiten-current-timer)

(defvar hiten-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'hiten-reload-page-at-least-once)
    (define-key map (kbd "o") 'hiten-goto-url)
    (define-key map (kbd "c") 'widget-button-press)
    map))

(define-derived-mode hiten-mode fundamental-mode
  "Hiten"
  "Major mode for browsing the web using headless Chrome"
  (hiten-start-browser)
  (hiten-load-page))

(defun hiten-start-browser ()
  (when (get-buffer hiten-process-buffer-name)
    (kill-buffer hiten-process-buffer-name))
  (when (get-process hiten-process-name)
    (kill-process hiten-process-name))
  (setq hiten-current-process
        (start-process hiten-process-name hiten-process-buffer-name "node" "index.js"))
  (set-process-filter hiten-current-process #'hiten-process-output-filter))

(defun hiten-process-output-filter (_ output)
  (message "%S" output)
  (when (string-match-p "WROTE" output)
    (hiten-load-page)))

(defun hiten-load-page ()
  (when (equal major-mode 'hiten-mode)
    (let ((current-point (point)))
      (erase-buffer)
      (call-process "pandoc" nil t nil "-f" "html" "-t" "plain" "browsed.html")
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([0-9]+\\):C:\\(.*\\)\\]\\]" nil t)
        (let ((id (match-string 1))
              (caption (match-string 2)))
          (replace-match "" nil nil)
          (widget-create 'push-button
                         :notify (lambda (&rest _) (hiten-click id))
                         caption)))
      (goto-char current-point))))

(defun hiten-reload-page ()
  (process-send-string hiten-current-process "R\n")
  (hiten-load-page))

(defun hiten-reload-page-at-least-once (&optional repeat-every)
  "Reload the browser display.
   Automatically repeats every REPEAT-EVERY seconds.
   Call with a double prefix argument to stop automatic reloads."
  (interactive "P")
  (hiten-reload-page)
  (when repeat-every
    (cond ((equal repeat-every '(16))
           (hiten-clear-reload-timer))
          ((> repeat-every 0)
           (hiten-reload-every repeat-every)))))

(defun hiten-reload-every (interval)
  (message "Reloading every %d seconds" interval)
  (setq hiten-current-timer (run-at-time t interval 'hiten-reload-page)))

(defun hiten-clear-reload-timer ()
  (message "Stopping automatic reloads")
  (cancel-timer hiten-current-timer))

(defun hiten-click (id)
  (process-send-string hiten-current-process (concat "C" id "\n")))

(defun hiten-goto-url (url)
  (interactive "M")
  (process-send-string hiten-current-process (concat "O" url "\n"))
  (hiten-load-page))

(defun hiten ()
  "Launch hiten-mode in a separate buffer"
  (interactive)
  (setq hiten-current-buffer (get-buffer-create hiten-buffer-name))
  (pop-to-buffer hiten-buffer-name)
  (hiten-mode))

(provide 'hiten-mode)
