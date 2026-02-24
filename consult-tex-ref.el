;;; consult-tex-ref.el --- Patched version of consult-tex -*- lexical-binding: t -*-

;;; The package has been patched to work with the recent version of consult and stripped of citation
;;; commands: use citar instead...
;;;
;;; Original commentary for consult-tex:
;; This package provides consult based commands to work with tex references.
;; Consult enables the preview of the text near the definition of a label in
;; order to facilitate the selection of the correct reference, based on
;; information obtained from the context.
;;
;; Multi-file projects are supported via TeX-master (AUCTeX) or by scanning
;; \\input and \\include directives in the master file.

;; The commands provided are
;;  'consult-tex-reference' - Use consult to find a reference
;;  'consult-tex-insert-reference' - Use consult to insert a reference

;;; Code:
(require 'consult)

;;;###autoload
(defun consult-tex-reference ()
  "Use consult to find a reference."
  (interactive)
  (push-mark (point) t)
  (when (fboundp 'evil--jumps-push) (evil--jumps-push))
  (goto-char (consult-tex--find-reference)))


;;;###autoload
(defun consult-tex-insert-reference ()
  "Use consult to insert a reference."
  (interactive)
  (when (or (eq (char-before) ? ) (eq (char-before) ?~)) (delete-char -1))
  (insert (format "~\\ref{%s}"
		  (save-excursion
		    (goto-char (consult-tex--find-reference))
		    (re-search-forward "\\(.*\\)}" nil t)
		    (match-string-no-properties 1)))))


(defun consult-tex--collect-files ()
  "Return a list of all tex files in the project starting from the master file."
  (let* ((master-name (or (and (boundp 'TeX-master)
                               (stringp TeX-master)
                               TeX-master)
                          (file-name-sans-extension (buffer-file-name))))
         (master (expand-file-name
                  (concat master-name
                          (unless (string-suffix-p ".tex" master-name) ".tex"))
                  (file-name-directory (buffer-file-name))))
         (master-dir (file-name-directory master))
         (files (list master)))
    (with-temp-buffer
      (insert-file-contents master)
      (goto-char 0)
      (while (re-search-forward "\\\\\\(?:input\\|include\\){\\([^}]+\\)}" nil t)
        (let ((included (expand-file-name
                         (concat (match-string 1)
                                 (unless (string-suffix-p ".tex" (match-string 1)) ".tex"))
                         master-dir)))
          (when (file-exists-p included)
            (push included files)))))
    (delete-dups (nreverse files))))

(defun consult-tex--annotate (cand)
  "Show line number and source line for CAND as annotation."
  (when-let* ((loc (get-text-property 0 'consult-location cand))
              (marker (car loc))
              (line (cdr loc)))
    (list cand
          (propertize (format " %d" line) 'face 'consult-line-number-prefix)
          (when (marker-buffer marker)
            (with-current-buffer (marker-buffer marker)
              (save-excursion
                (goto-char marker)
                (propertize
                 (concat "  " (string-trim (buffer-substring-no-properties
                                            (pos-bol) (pos-eol))))
                 'face 'completions-annotations)))))))


(defun consult-tex--find-reference ()
  "Internal function for \\='consult-tex-reference'."
  (interactive)
  (let ((refs ())
        (current-buf (current-buffer)))
    (dolist (file (consult-tex--collect-files))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char 0)
          (while (re-search-forward "\\\\label{\\(.*\\)}" nil t)
            (goto-char (match-beginning 1))
            (push (consult--location-candidate
                   (match-string 1)
                   (point-marker)
                   (line-number-at-pos)
                   (point-marker))
                  refs)
            (goto-char (match-end 0))))))
    (setq refs (seq-uniq (nreverse refs) #'string=))
    ;; Sort so the first candidate is the first one above point in the current buffer.
    (let ((head refs)
          (old nil))
      (while
          (and refs
               (eq (marker-buffer
                    (car (get-text-property 0 'consult-location (car refs))))
                   current-buf)
               (< (cdr (get-text-property 0 'consult-location (car refs)))
                  (line-number-at-pos)))
        (setq old refs)
        (setq refs (cdr refs)))
      (when old
        (setf (cdr old) nil)
        (setq refs (reverse (append refs head)))))
    (consult--read
     refs
     :prompt "References:"
     :annotate #'consult-tex--annotate
     :category 'consult-location
     :sort nil
     :require-match t
     :lookup #'consult--lookup-location
     :history '(:input consult--line-history)
     :add-history (thing-at-point 'symbol)
     :default (car refs)
     :state (consult--jump-preview))))

(provide 'consult-tex-ref)
;;; consult-tex-ref.el ends here
