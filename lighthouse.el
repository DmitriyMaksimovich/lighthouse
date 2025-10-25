;;; lighthouse.el --- Highlight multiple lines persistently in a buffer -*- lexical-binding: t -*-

;;; Commentary:
;; This package provides functionality to highlight multiple lines in an Emacs buffer.
;; Highlights persist across buffer switches and are removed when the buffer is killed.
;; Commands:
;; - `lighthouse-toggle': Toggle highlight on the current line.
;; - `lighthouse-clear-all': Remove all highlights in the current buffer.

;;; Code:

(defvar-local lighthouse--overlays nil
  "List of overlays used for highlighting lines in the current buffer.")

(defun lighthouse--get-line-number ()
  "Return the current line number."
  (line-number-at-pos))

(defun lighthouse--highlight-line (line-num)
  "Highlight the line at LINE-NUM in the current buffer."
  (save-excursion
    (goto-line line-num)
    (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
      (overlay-put overlay 'face '(:background "yellow" :extend t))
      (overlay-put overlay 'lighthouse t)
      (push overlay lighthouse--overlays))))

(defun lighthouse--unhighlight-line (line-num)
  "Remove highlight from the line at LINE-NUM in the current buffer."
  (let ((to-remove nil))
    (dolist (ov lighthouse--overlays)
      (when (and (overlay-buffer ov) ; Ensure overlay is still valid
                 (= (line-number-at-pos (overlay-start ov)) line-num))
        (push ov to-remove)))
    (dolist (ov to-remove)
      (delete-overlay ov)
      (setq lighthouse--overlays (delete ov line-highlight--overlays)))))

(defun lighthouse-toggle ()
  "Toggle highlighting of the current line."
  (interactive)
  (let ((line-num (lighthouse--get-line-number)))
    (if (member line-num (lighthouse--get-highlighted-lines))
        (lighthouse--unhighlight-line line-num)
      (lighthouse--highlight-line line-num))))

(defun lighthouse--get-highlighted-lines ()
  "Return a list of line numbers that are currently highlighted."
  (let (lines)
    (dolist (ov lighthouse--overlays)
      (when (overlay-buffer ov)
        (push (line-number-at-pos (overlay-start ov)) lines)))
    lines))

(defun lighthouse-clear-all ()
  "Remove all line highlights in the current buffer."
  (interactive)
  (dolist (ov lighthouse--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq lighthouse--overlays nil))

(defun lighthouse--cleanup ()
  "Clean up highlights when the buffer is killed."
  (lighthouse-clear-all))

;;;###autoload
(define-minor-mode lighthouse-mode
  "Minor mode for persistent line highlighting."
  :lighter " LHL"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c h") #'lighthouse-toggle)
            (define-key map (kbd "C-c c") #'lighthouse-clear-all)
            map)
  (if lighthouse-mode
      (add-hook 'kill-buffer-hook #'lighthouse--cleanup nil t)
    (remove-hook 'kill-buffer-hook #'lighthouse--cleanup t)
    (lighthouse-clear-all)))

(provide 'lighthouse)
;;; lighthouse.el ends here
