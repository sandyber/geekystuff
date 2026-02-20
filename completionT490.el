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
  (vertico-resize nil) ;; Do not grow or shrink dynamically the Vertico minibuffer
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
  (completion-styles '(orderless basic)) 
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

(use-package cape
  :disabled
  :ensure t
  :init
  ;; 1. Tell Cape where your dictionary is
  (setq cape-dict-file "/backup/Dropbox/zzz/emacs/hunspell/english-words.txt")

  ;; 2. Add the dictionary and dabbrev (buffer words) to the completion list
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  
  ;; Optional: If you want LaTeX-specific completions to always be available
  (add-to-list 'completion-at-point-functions #'cape-tex))

(use-package corfu
  :disabled
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto nil)              ;; Set to nil if you only want it when YOU trigger it
  (corfu-quit-at-boundary nil)
  ;; :bind 
  ;; ;; 1. Use this to TRIGGER the menu
  ;; ("C-x 0" . completion-at-point)
  
  ;; ;; 2. Use this to SELECT the word once the menu is open
  ;; (:map corfu-map
  ;;       ("C-x 0" . corfu-complete))
  :config
    ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil) ;use this for cape
  )
;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)
)

(use-package company
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
  (setq company-backends '((company-capf company-dabbrev-code) ; :with company-dabbrev-code
                           company-files
                           company-dabbrev))
  ;; (add-hook 'emacs-lisp-mode-hook ;'prog-mode-hook
  ;;           (lambda ()
  ;;             (setq-local company-idle-delay 0.2)))
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
  (company-idle-delay nil) ;; turn off auto-completion by default
  )
