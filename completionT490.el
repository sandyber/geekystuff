(provide 'completionT490)

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
;    (marginalia-align 'right)
;    (marginalia-align-offset -5)
  :init
  (marginalia-mode)
;  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  )

(use-package vertico
  :ensure t
  :custom
  (vertico-count 10)  ;; limit to a fixed size
;  (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  :bind (:map vertico-map
    ;; Use page-up/down to scroll vertico buffer, like ivy does by default.
    ("<prior>" . 'vertico-scroll-down)
    ("<next>"  . 'vertico-scroll-up))
  :init
  ;; Activate vertico
  (vertico-mode)
  (savehist-mode))

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
  (completion-styles '(orderless)) ;basic
  ;; Enable partial completion for file wildcard support
  (completion-category-overrides '((file (styles partial-completion))))
  )

(use-package consult
  :ensure t
  :config
  ;; Disable preview
  (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;(consult-preview-key nil)
  (consult-customize ;https://github.com/minad/consult#live-previews
   consult-line :preview-key 'any
   )
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
  ;; :bind
  ;; (("C-x b" . 'consult-buffer)    ;; Switch buffer, including recentf and bookmarks
  ;;  ("M-l"   . 'consult-git-grep)  ;; Search inside a project
  ;;  )
)
(use-package embark
  :ensure t
  :bind
  (("M-o"   . embark-act)         ;; Begin the embark process
   ("C-;"   . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :config
  (use-package embark-consult
      :ensure t
))

(use-package company
  ;; :disabled
  :ensure t
  :pin melpa
  :demand t
  :hook
  (after-init . global-company-mode)
  ;; :commands ; defers loading until after this command: https://www.gnu.org/software/emacs/manual/html_mono/use-package.html
  ;; (company-complete-common)
  :config
  ;; (add-hook 'prog-mode-hook 'company-mode)
  ;; (add-hook 'text-mode-hook 'company-mode)
;  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-selection-wrap-around t
        company-show-numbers t
        company-format-margin-function nil ;disable icons
        company-tooltip-align-annotations t
        company-idle-delay nil;disable auto-complete
        company-require-match nil       
        company-minimum-prefix-length 2)
  ;; use numbers 0-9 to select company completion candidates :https://www.reddit.com/r/emacs/comments/5jvawj/select_the_company_completion_candidate_by/?rdt=48684
  ;; (let ((map company-active-map))
  ;; (mapc (lambda (x) (define-key map (format "%d" x)
  ;;                `(lambda () (interactive) (company-complete-number ,x))))
  ;;       (number-sequence 0 9)))
(let ((map company-active-map))
  (mapc (lambda (x)
          (define-key map (format "%d" x)
            `(lambda ()
               (interactive)
               ;; If x is 0, complete the 10th candidate; otherwise complete x
               (company-complete-number ,(if (= x 0) 10 x)))))
        (number-sequence 0 9)))
;; (define-key company-active-map [escape] 'company-abort) ;https://github.com/company-mode/company-mode/discussions/1356#discussioncomment-4469605
  ;; (define-key company-mode-map (kbd "<tab>") 'company-complete)
  :custom
  (company-idle-delay nil) ;; turn off auto-completion
  )
