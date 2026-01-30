(provide 'completionT490)
;---------------------------------------------
(use-package avy
  :ensure t
  :pin melpa
  :config
  (setq avy-timeout-seconds 1.0)
  )
;---------------------------------------------
(use-package ivy-avy
  ;; :disabled
  :ensure t
  :pin melpa)
;---------------------------------------------
(use-package ivy
  ;; useful keybinding: M-o in ivy-minibuffer: actions;
  ;; :disabled
  :ensure t
  :pin melpa
  :diminish ivy-mode
  :bind
  (:map ivy-minibuffer-map
        ("C-\\" . ivy-avy)
   ;; :map ivy-switch-buffer-map ;not working?
   ;;      ("C-d" . ivy-switch-buffer-kill)
  )
  (:map ivy-mode-map
        ("C-'" . ivy-avy)               ;also defined in ivy-minibuffer-map
  )
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy)
  (setq enable-recursive-minibuffers t)  ;; e.g. search in the command minibuffer  
  (setq search-default-mode #'char-fold-to-regexp)  ;; enable this if you want `swiper' to use it
  (setq ivy-use-virtual-buffers t)  ;; add 'recentf-mode' and bookmarks to 'ivy-switch-buffer'.
  (setq ivy-height 10)  ;; number of result lines to display
  (setq ivy-count-format "")  ;; do not count candidates
  (setq ivy-initial-inputs-alist nil)  ;; no regexp by default
  (setq ivy-re-builders-alist  ;; configure regexp engine.
      '((swiper-isearch . regexp-quote)  ; allow exact match in swiper, to disable press M-r in swiper before typing: https://emacs.stackexchange.com/a/41549/19901
        (t      . ivy--regex-ignore-order))) ;; allow input not in order
      ;; '((t   . ivy--regex-ignore-order)))  
)
;;---------------------------------------------
(use-package ivy-rich
  :disabled
  :ensure t
  :pin melpa
  :config
  (ivy-rich-mode)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  )
(use-package marginalia
 ;; :disabled
  :ensure t
  :pin melpa
  :config
  (marginalia-mode)
  )
;---------------------------------------------
(use-package counsel                    ;https://ladicle.com/post/config/ apparently need this to activate map binding?
  ;; :disabled
  :ensure t
  :pin melpa
    :diminish ivy-mode counsel-mode
    :after ivy
    :config
    (counsel-mode)
    :bind
    (
    :map ivy-minibuffer-map
    ("C-j" . ivy-immediate-done)
    ("RET" . ivy-alt-done)
    ("M-r" . ivy-toggle-regexp-quote)
;;     :map minibuffer-local-map; not working
;; ;    ("C-r" . counsel-minibuffer-history)
;;     ("C-d" . ivy-switch-buffer-kill); d for delete
    )
    )
;---------------------------------------------
;; (use-package swiper
;;   :ensure t
;;   :pin melpa
;;   :after (ivy avy)
;;   )
;---------------------------------------------
;; (use-package amx; allow history in counsel
;;   :ensure t)
(use-package amx; allow history in counsel
  :ensure t
  :pin melpa
  :after ivy
  ;; :custom
  ;; (amx-backend 'auto)
  ;; (amx-save-file "/backup/Dropbox/zzz/.emacs.d/amx-items")
  ;; (amx-history-length 50)
  ;; (amx-show-key-bindings nil)
  ;; :config
  ;; (amx-mode 1)
  )
;---------------------------------------------
(use-package auto-complete
  :disabled
  :ensure t
  :pin melpa
  :init
  (progn
    (ac-config-default)
    (setq ac-auto-start nil)
;    (global-set-key "\M-/" 'auto-complete)
    (ac-set-trigger-key "<tab>")
;    (setq ac-auto-show-menu 3.8)
    (global-auto-complete-mode nil)         ;nil
    (add-to-list 'ac-modes 'text-mode)
    (add-to-list 'ac-modes 'LaTeX-mode)
    ;; (setq ac-modes '(text-mode LaTeX-mode emacs-lisp-mode))
    ))
(add-hook 'auto-complete-mode-hook ;https://github.com/auto-complete/auto-complete/issues/533#issuecomment-2698199280
          (lambda ()
            (setq ac-sources (remove 'ac-source-abbrev ac-sources))))
;; ;---------------------------------------------
;; ;requires grep: on Windows, put the contents of the bin folder from grep-2.10-w32-bin.zip in /zzz/emacs into the /apps/bin folder and add the address to system PATH
;; (use-package ac-ispell
;;   :disabled
;;    :ensure t
;;    :pin melpa
;;    :requires auto-complete
;;    :after auto-complete
;;    :config
;;    (ac-ispell-setup)
;;    (add-hook 'LaTeX-mode-hook 'ac-ispell-ac-setup)
;;    (add-hook 'text-mode-hook 'ac-ispell-ac-setup)
;; )
;---------------------------------------------
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
  ;; :general
  ;; (:keymap 'company-mode-map
  ;;          "C-SPC" 'company-complete) ;; keybinding to trigger company completion  
  ;; :bind
  ;; (:map
  ;;  company-active-map ("C-x 0" . company-complete)) ; also try this if necessary: https://emacs.stackexchange.com/a/64077/19901
;  (("C-SPC" . company-complete))
  ;; (:map; not working bcs of ergoemacs? moved to the bind-key in generalT490.el
  ;;  company-mode-map ("C-x c" . company-complete))
  )
;; (with-eval-after-load 'company
;;   (define-key company-mode-map (kbd "<tab>") 'company-complete))
;; (use-package ivy-posframe ;;https://hoeltgman.gitlab.io/dotemacs/;; https://ogbe.net/emacs/minimal
;;   :disabled;; unstable?
;;   :ensure t
;;   :pin melpa
;;   :after ivy
;;   :diminish ivy-posframe-mode
;;   :custom-face
;;   (ivy-posframe ((t (list :background (face-attribute 'default :background)))))
;; ;  (ivy-posframe ((t (list :foreground (face-attribute 'default :foreground)))))
;;   (ivy-posframe-border ((t (:background "gold"))))
;;   (ivy-posframe-cursor ((t (:background "gold"))))
;;   :config
;;   ;; custom define height of post frame per function
;;   (setq ivy-posframe-height-alist '((find-file . 13)
;;             (t         . 13)))
;;   ;; display at `ivy-posframe-style'
;;   (setq ivy-posframe-display-functions-alist
;;   '((complete-symbol   . ivy-posframe-display-at-point)
;;     (counsel-M-x       . ivy-posframe-display-at-point)
;;     (counsel-find-file . ivy-posframe-display-at-window-bottom-left)
;;     (ivy-switch-buffer . ivy-posframe-display-at-window-bottom-left)
;;     (t                 . ivy-posframe-display-at-window-bottom-left)))
;;   ;; other customizations
;;   (setq ivy-posframe-hide-minibuffer t)
;;   ;; (setq ivy-posframe-min-width 120)
;;   ;; (setq ivy-posframe-width 120)
;;   (setq ivy-posframe-border-width 1)
;;   ;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;;   (set-face-attribute 'ivy-posframe nil :foreground 'unspecified :background 'unspecified)
;;   (ivy-posframe-mode 1)
;;   )
;---------------------------------------------------------------------
(use-package vertico
  :disabled
  :ensure t
  :pin melpa
  :config
  (vertico-mode)
  (vertico-mouse-mode)
  :custom
  (vertico-count 22)
  :bind (:map vertico-map
              ("C-'"       . #'vertico-quick-exit)
              ;; Have to rebind this because C-m is translated to RET.
              ("<return>"  . #'exit-minibuffer)
              ("C-m"       . #'vertico-insert)
              ("C-c SPC"   . #'vertico-quick-exit)
              ("DEL"       . #'vertico-directory-delete-char)))

(use-package consult
  :disabled
  :ensure t
  :pin melpa
  :config
  (defun pt/yank-pop ()
    "As pt/yank, but calling consult-yank-pop."
    (interactive)
    (let ((point-before (point)))
      (consult-yank-pop)
      (indent-region point-before (point))))

  :bind (("C-c i"   . #'consult-imenu)
         ("C-c b"   . #'consult-buffer)
         ("C-x b"   . #'consult-buffer)
         ("C-c r"   . #'consult-recent-file)
         ("C-c y"   . #'pt/yank-pop)
         ("C-c R"   . #'consult-bookmark)
         ("C-c `"   . #'consult-flymake)
         ("C-c h"   . #'consult-ripgrep)
         ("C-x C-f" . #'find-file)
         ("C-h a"   . #'consult-apropos)
         )
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-project-root-function #'deadgrep--project-root) ;; ensure ripgrep works
  )

;; (use-package marginalia
;;   :config (marginalia-mode))

(use-package orderless
  :disabled
  :ensure t
  :pin melpa
  :custom (completion-styles '(orderless)))

(use-package ctrlf
  :disabled
  :ensure t
  :pin melpa
  :config (ctrlf-mode))

(use-package prescient
  :disabled
  :ensure t
  :pin melpa
  :config (prescient-persist-mode))
;; ;-----------------------------------------------------------------------------------------
(use-package embark
  :disabled
  :ensure t
  :pin melpa
  :bind
  (("C-c ." . embark-act)         ;; pick some comfortable binding
   ("C-c ;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :disabled
  :ensure t
  :pin melpa
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;;--------------------------- HELM ---------------------------------------
;; (use-package helm
;;   :ensure t
;;   :demand t
;; ;  :defer 2
;;   ;; :bind
;;   ;; ("M-a" . helm-M-x)
;;   ;; ("C-x C-f" . helm-find-files)
;;   ;; ("M-y" . helm-show-kill-ring)
;;   ;; ("C-x b" . helm-mini)
;;   :config
;;   (require 'helm-config)
;;   (helm-mode 1)
;;   (setq helm-split-window-inside-p t
;;     helm-move-to-line-cycle-in-source t)
;;   (setq helm-autoresize-max-height 0)
;;   (setq helm-autoresize-min-height 20)
;;   (helm-autoresize-mode 1)
;;   (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;;   (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
;;   (define-key helm-map (kbd "M-z")  'helm-select-action) ; list actions using C-z
;;   )
