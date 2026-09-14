;;; -*- lexical-binding: t; -*-
(use-package avy
  :ensure t
  :pin melpa
  :config
  (setq avy-timeout-seconds 1.0)
  )

(use-package marginalia
  ;; :disabled
  :ensure t
  :pin melpa
  :custom
  (marginalia-max-relative-age 0)
  :init
  (marginalia-mode)
  )

(use-package vertico
  :ensure t
  :config
;; Dealing with a clash between RET in vertico and TAB+RET in citar multiple candidate selection
(defvar ysb/citar-selecting-multiple nil
  "Non-nil when inside citar--select-multiple.")

(advice-add 'citar--select-multiple :around
  (lambda (orig &rest args)
    (let ((ysb/citar-selecting-multiple t))
      (apply orig args))))

(defun ysb/vertico-insert-or-dired ()
  "If candidate is a directory, insert it; otherwise exit with it."
  (interactive)
  (if ysb/citar-selecting-multiple
      (citar--multiple-exit)
    (let ((candidate (vertico--candidate)))
      (if (file-directory-p candidate)
          (vertico-insert)
        (vertico-exit)))))
(advice-add 'citar--setup-multiple-keymap :override
  (lambda ()
    (let ((keymap (make-composed-keymap nil (current-local-map)))
          (kbdselect (kbd (car citar--multiple-setup)))
          (kbdexit   (kbd (cdr citar--multiple-setup))))
      (define-key keymap kbdselect #'vertico-exit)
      (define-key keymap kbdexit #'citar--multiple-exit)
      (use-local-map keymap))))
  :custom
  (vertico-count 10)  ;; limit to a fixed size
  (vertico-resize nil) ;; Do not grow or shrink dynamically the Vertico minibuffer
  (vertico-cycle t)    ;after reaching the bottom of the list go to the top
  :bind (:map vertico-map
    ("RET" . ysb/vertico-insert-or-dired)
    ("<prior>" . vertico-scroll-down) ;; Use page-up/down to scroll vertico buffer, like ivy does by default.
    ("<next>"  . vertico-scroll-up))
  :init
  ;; Activate vertico
  (vertico-mode) ;savehist-mode is enabled in generalT490.el
  )

;; Convenient path selection
(use-package vertico-directory
  :after vertico
  :ensure nil  ;; no need to install, it comes with vertico
  :bind (:map vertico-map
    ("DEL" . vertico-directory-delete-char)))

(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package orderless
  :ensure t
  :custom
  ;; Activate orderless completion
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;; Enable partial completion for file wildcard support
  (completion-category-overrides '((file (styles partial-completion))))
  )

(use-package consult
  :ensure t
  :init
  (setq
     consult-line-start-from-top t 
     consult-line-point-placement 'match-beginning
     )
  :config
  ;; Disable preview
  (setq consult-preview-key '("S-<down>" "S-<up>"))
  (consult-customize ;https://github.com/minad/consult#live-previews
   consult-line :preview-key 'any
   )
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (add-to-list 'imenu-generic-expression
                 '("Use-package" "^(use-package \\([^[:space:]\n]+\\)" 1) t)))

(with-eval-after-load 'consult-imenu
  (setq consult-imenu-config
        '((emacs-lisp-mode
           :toplevel "Functions"
           :types ((?f "Functions"    font-lock-function-name-face)
                   (?m "Macros"       font-lock-function-name-face)
                   (?p "Packages"     font-lock-constant-face)
                   (?t "Types"        font-lock-type-face)
                   (?v "Variables"    font-lock-variable-name-face)
                   (?u "Use-package"  font-lock-keyword-face))))))
;;------------------
  (defun consult-switch-buffer-kill ()
    "Kill buffer and remove it from the current completion session."
    (interactive)
    ;; Get the candidate (removing the irregular char as you did)
    (let* ((cand (vertico--candidate))
           (name (substring cand 0 -1)))
      (when (get-buffer name)
        (kill-buffer name)
        ;; Manually filter the killed buffer out of the current Vertico list
        (setq vertico--candidates 
            (delete cand vertico--candidates))
      ;; Decrement the count so the UI stays accurate
      (setq vertico--total (1- vertico--total))
      ;; Now force the redraw
      (vertico--exhibit))))
  )
;; ---------------------- input completion for consult --------------------------------------------
(defun ysb/latex-insert-input (&optional recursive)
  "Insert \\input{FILE}, picking FILE with consult and live preview.
With a prefix argument, search subdirectories too.
Files the master already pulls in are marked."
  (interactive "P")
  (let* ((dir (ysb/latex-project-dir))
         (cands (ysb/latex-input-candidates recursive))
         (included (ignore-errors
                     (mapcar #'file-truename
                             (consult-latex-ref--collect-files
                              (consult-latex-ref--master-file)))))
         (preview (and (fboundp 'consult--file-preview)
                       (consult--file-preview)))
         (choice
          (consult--read
           cands
           :prompt "\\input: "
           :category 'file
           :require-match t
           :history 'file-name-history
           :annotate
           (lambda (cand)
             (when (member (file-truename (expand-file-name cand dir)) included)
               (propertize "  already in project"
                           'face 'consult-latex-ref-toc-file-face)))
           :state
           (when preview
             (lambda (action cand)
               (funcall preview action
                        (and cand (expand-file-name cand dir))))))))
    (insert (format "\\input{%s}" (file-name-sans-extension choice)))))
;;----------------------------------------------------------------------------------------
(use-package embark
  :ensure t
  :bind
  (("M-o"   . embark-act)         ;; Begin the embark process
   ("C-;"   . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  ;; :init
  ;; (setq embark-auto-prefix-help-delay 1.5) ; default 2.0
  ;; (embark-auto-prefix-help-mode)           ; pause after a prefix = vertico over its bindings
  :config
  (use-package embark-consult
      :ensure t
      ))

;; cape and corfu (disabled) moved to atticT490.el
;; (read-extended-command-predicate is already set in the use-package emacs block above)

(use-package company
  :ensure t
  :pin melpa
  :demand t
  :hook
  (after-init . global-company-mode)
  :bind (:map company-mode-map ;moved here from generalT490.el
              ("<tab>" . company-complete)
              ("C-<tab>" . company-dabbrev)
              :map company-active-map
              ("<escape>" . company-abort)) ;https://github.com/company-mode/company-mode/discussions/1356#discussioncomment-4469605
  :config
  (setq company-backends '((company-capf company-dabbrev-code) ; :with company-dabbrev-code
                           company-files
                           company-dabbrev))
  (defun ysb/toggle-company-auto ()
  "Switch between manual and automatic company completion."
  (interactive)
  (if company-idle-delay
      (setq-local company-idle-delay nil)
    (setq-local company-idle-delay 0.2))
  (message "Company auto-popup is now %s" 
           (if company-idle-delay "ON" "OFF")))

  (setq company-selection-wrap-around t
        company-show-numbers t
        company-format-margin-function nil ;disable icons
        company-tooltip-align-annotations t
        company-idle-delay nil;disable auto-complete
        company-require-match nil       
        company-minimum-prefix-length 2)
  ;; use numbers 0-9 to select company completion candidates :https://www.reddit.com/r/emacs/comments/5jvawj/select_the_company_completion_candidate_by/?rdt=48684
(let ((map company-active-map))
  (mapc (lambda (x)
          (define-key map (format "%d" x)
            `(lambda ()
               (interactive)
               ;; If x is 0, complete the 10th candidate; otherwise complete x
               (company-complete-number ,(if (= x 0) 10 x)))))
        (number-sequence 0 9)))
  :custom
  (company-idle-delay nil) ;; turn off auto-completion by default
  )

(provide 'completionT490)
;;; completionT490.el ends here
