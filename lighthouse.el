;;; lighthouse.el --- Highlight multiple lines persistently in a buffer -*- lexical-binding: t -*-

;;; Commentary:
;; This package provides functionality to highlight multiple lines in an Emacs buffer.
;; Highlights persist across buffer switches and are removed when the buffer is killed.
;; The highlight color can be customized via `lighthouse-highlight-color'.
;; Commands:
;; - `lighthouse-toggle': Toggle highlight on the current line.
;; - `lighthouse-clear-all': Remove all highlights in the current buffer.
;; - `lighthouse-next': Jump to the next highlighted line, cycling to the first if at the last.
;; - `lighthouse-prev': Jump to the previous highlighted line, cycling to the last if at the first.

;;; Code:

(defcustom lighthouse-highlight-color "#504945"
  "Background color used for highlighting lines in lighthouse-mode.
Can be a color name (e.g., \"orange\") or a hex code (e.g., \"#504945\")."
  :type 'string
  :group 'lighthouse
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Update existing highlights when the color changes
         (when (and (boundp 'lighthouse--overlays) lighthouse--overlays)
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               (when lighthouse--overlays
                 (dolist (ov lighthouse--overlays)
                   (when (overlay-buffer ov)
                     (overlay-put ov 'face `(:background ,value :extend t))))))))))

(defvar-local lighthouse--overlays nil
  "List of overlays used for highlighting lines in the current buffer.")

(defun lighthouse--highlight-line (line-num)
  "Highlight the line at LINE-NUM in the current buffer."
  (save-excursion
    (goto-line line-num)
    (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
      (overlay-put overlay 'face `(:background ,lighthouse-highlight-color :extend t))
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
      (setq lighthouse--overlays (delete ov lighthouse--overlays)))))

(defun lighthouse-toggle ()
  "Toggle highlighting of the current line."
  (interactive)
  (let ((line-num (line-number-at-pos)))
    (if (member line-num (lighthouse--get-highlighted-lines))
        (lighthouse--unhighlight-line line-num)
      (lighthouse--highlight-line line-num))))

(defun lighthouse--get-highlighted-lines ()
  "Return a sorted list of line numbers that are currently highlighted."
  (let (lines)
    (dolist (ov lighthouse--overlays)
      (when (overlay-buffer ov)
        (push (line-number-at-pos (overlay-start ov)) lines)))
    (sort lines #'<)))

(defun lighthouse-next ()
  "Jump to the next highlighted line, cycling to the first if at the last."
  (interactive)
  (let* ((lines (lighthouse--get-highlighted-lines))
         (current-line (line-number-at-pos))
         (next-line (if (null lines)
                        (user-error "No highlighted lines")
                      (or (cl-find-if (lambda (x) (> x current-line)) lines)
                          (car lines)))))
    (when next-line
      (goto-line next-line)
      (recenter))))

(defun lighthouse-prev ()
  "Jump to the previous highlighted line, cycling to the last if at the first."
  (interactive)
  (let* ((lines (lighthouse--get-highlighted-lines))
         (current-line (line-number-at-pos))
         (prev-line (if (null lines)
                        (user-error "No highlighted lines")
                      (or (car (last (cl-remove-if (lambda (x) (>= x current-line)) lines)))
                          (car (last lines))))))
    (when prev-line
      (goto-line prev-line)
      (recenter))))

(defun lighthouse-clear-all ()
  "Remove all line highlights in the current buffer."
  (interactive)
  (dolist (ov lighthouse--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq lighthouse--overlays nil))

;;;###autoload
(define-minor-mode lighthouse-mode
  "Minor mode for persistent line highlighting."
  :lighter " LH"
  :group 'lighthouse
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c h") #'lighthouse-toggle)
            (define-key map (kbd "C-c c") #'lighthouse-clear-all)
            (define-key map (kbd "C-c n") #'lighthouse-next)
            (define-key map (kbd "C-c p") #'lighthouse-prev)
            map)
  (if lighthouse-mode
      (add-hook 'kill-buffer-hook #'lighthouse-clear-all nil t)
    (remove-hook 'kill-buffer-hook #'lighthouse-clear-all t)
    (lighthouse-clear-all)))

(provide 'lighthouse)
;;; Lighthouse.el ends here
