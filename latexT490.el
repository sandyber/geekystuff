;;; -*- lexical-binding: t; -*-
;;------------------------------------------------------------
(defconst ysb/ebib-checked-version "20260621.1413"
  "Newest ebib version the local overrides below were checked against.
The overrides of `ebib--save-database' and `ebib--bib-read-entries'
copy internals from an older ebib.  When ebib updates past this
version, a warning fires at load: recheck the overrides against the
upstream definitions, then update this constant to silence it.")
(use-package ebib
  :defer t
  :pin melpa
  :ensure t
  :init
  :config ;https://ogbe.net/emacs/references
  (let* ((desc (cadr (assq 'ebib package-alist)))
         (installed (and desc (package-version-join (package-desc-version desc)))))
    (when (and installed (version< ysb/ebib-checked-version installed))
      (warn "ebib %s is newer than %s: recheck the local overrides of `ebib--save-database' and `ebib--bib-read-entries', then update `ysb/ebib-checked-version'"
            installed ysb/ebib-checked-version)))
;;--------------------------------------
;; Restore pre-20260621.1413 save behaviour: write via a temp buffer
;; instead of leaving a visiting buffer behind.
(defun ebib--save-database (db &optional force)
  "Save the database DB.
The FORCE argument is used as in `ebib-save-current-database'."
  ;; See if we need to make a backup.
  (when (and (ebib-db-backup-p db)
             (file-exists-p (ebib-db-get-filename db)))
    (ebib--make-backup (ebib-db-get-filename db))
    (ebib-db-set-backup nil db))

  ;; Check if the file has changed on disk.
  (let ((db-modtime (ebib-db-get-modtime db))
        (file-modtime (ebib--get-file-modtime (ebib-db-get-filename db))))
    ;; If the file to be saved has been newly created, both modtimes are nil.
    (when (and db-modtime file-modtime
               (time-less-p db-modtime file-modtime))
      (unless (or (and (listp force)
                       (eq 16 (car force)))
                  (yes-or-no-p (format "File `%s' changed on disk.  Overwrite? " (ebib-db-get-filename db))))
        (error "[Ebib] File not saved"))))

  ;; Now save the database.
  (with-temp-buffer
    (ebib--format-database-as-bibtex db)
    (write-region (point-min) (point-max) (ebib-db-get-filename db)))
  (ebib--set-modified nil db))
;;--------------------------------------------------------------------------------------------------------  
;; Restore pre-20260621.1413 behaviour: read bib files into a temp
;; buffer instead of leaving a visiting buffer behind for every file.
  ;; If ebib is updated, recheck this against the upstream definition.
  (defun ebib--bib-read-entries (file db &optional ignore-modtime not-as-dependent)
    "Load BibTeX entries from FILE into DB. If FILE specifies a BibTeX dialect and no dialect is set for DB,
also set DB's dialect.  FILE's modification time is stored in DB,
unless IGNORE-MODTIME is non-nil.  If NOT-AS-DEPENDENT is
non-nil, load FILE as a normal database, even if it is a
dependent database."
  (with-temp-buffer
    (insert-file-contents file)
    (unless ignore-modtime
      (ebib-db-set-modtime (ebib--get-file-modtime file) db))
    (if (and (not not-as-dependent)
             (ebib--bib-find-main db))
        (let ((result (ebib--bib-find-bibtex-entries db nil)))
          (ebib--log 'message "Loaded %d entries into dependent database." (car result)))
      ;; Opening a non-dependent database.
      (unless (ebib-db-get-dialect db)
        (ebib-db-set-dialect (parsebib-find-bibtex-dialect) db))
      (let ((result (ebib--bib-find-bibtex-entries db nil)))
        (ebib--log 'message "%d entries, %d @Strings and %s @Preamble found in file."
                   (car result)
                   (cadr result)
                   (if (nth 2 result) "a" "no"))))
    (when ebib--log-error
      (message "%s found! Press `l' to check Ebib log buffer." (nth ebib--log-error '("Warnings" "Errors"))))))
;;--------------------------------------------------------------------------------------------------------  
  (setq
   ebib-default-directory ysb/bibdir
   ebib-bibtex-dialect 'BibTeX
   )
  (setq ebib-index-columns '(("Entry Key" 20 t)("Author/Editor" 20 nil)("Year" 6 t)("Title" 40 t)))
;; `ebib' uses `bibtex.el' to auto-generate keys for us
  (setq bibtex-autokey-year-length 4)
  (setq bibtex-autokey-name-case-convert-function 'capitalize)
  (setq bibtex-autokey-titleword-separator "")
  (setq bibtex-autokey-name-year-separator "")
  (setq bibtex-autokey-year-title-separator "")
  (setq bibtex-autokey-titleword-length 0)
  (setq bibtex-autokey-titlewords 0)
  ;; make ebib window easier to deal with
  (setq ebib-index-window-size 30)
  (require 'ebib-biblio)
  (define-key ebib-index-mode-map (kbd "Y") #'ebib-copy-entry-as-kill)
  (define-key ebib-index-mode-map (kbd "B") #'ebib-biblio-import-doi)
  (define-key biblio-selection-mode-map (kbd "e") #'ebib-biblio-selection-import)
  (setq ebib-citation-commands
        (quote ((LaTeX-mode
                 (("cite" "~\\cite[%A]{%K}")("citet" "~\\citet[%A]{%K}")
                  ("citep" "~\\citep[%A]{%K}")("citeyearpar" "~\\citeyearpar[%A]{%K}"))))))
  :custom
  (ebib-preload-bib-files '("progress.bib" "evolnat.bib" "hypocrisy.bib"))
  (ebib-bib-search-dirs (list ysb/bibdir))
  (ebib-field-edit-functions ;the original is in ebib-utils.el
   '((("abstract" "addendum" "note" "annotation")
      . ebib--edit-field-as-multiline)
     (("afterword" "annotator" "author" "bookauthor" "commentator" "editor" "editora" "editorb" "editorc" "foreword" "holder" "introduction" "sortname" "translator")
      . ebib--edit-list-field)
     (("title" "booktitle") ; to auto-complete InCollection items
      . ebib--edit-list-field)
     (("crossref" "xref")
      . ebib--edit-ref-field)
     (("file")
      . ebib--edit-file-field)
     (("institution")
      . ebib--edit-list-field)
     (("journal" "journaltitle")
      . ebib--edit-literal-field)
     (("keywords")
      . ebib--edit-separated-values-field)
     (("language" "origlanguage")
      . ebib--edit-language-field)
     (("location" "origlocation" "address")
      . ebib--edit-list-field)
     (("publisher" "origpublisher")
      . ebib--edit-list-field)))
  )
(use-package citar
  :custom
  (citar-bibliography (mapcar (lambda (f) (expand-file-name f ysb/bibdir))
                              '("evolnat.bib" "respect.bib" "hypocrisy.bib" "progress.bib")))
  :hook
  (LaTeX-mode . citar-capf-setup)
  :config
  (with-eval-after-load 'citar-latex
  (setq citar-latex-cite-commands
        '((("citet" "citep" "citeyearpar" "citeyear" "citeauthor") . (["Argument 1"] ["Argument 2"] t))
          )))
  (setq citar-latex-default-cite-command "citet")
  (advice-add 'citar-latex-insert-citation :around
  (lambda (orig keys &optional invert-prompt command)
    (if (citar-latex--macro-bounds)
        ;; Already inside a cite command, just add the key normally
        (funcall orig keys invert-prompt command)
      ;; New citation, insert tilde before
      (insert "~")
      (funcall orig keys invert-prompt command))))
(defun ysb/citar-append-bib-basename (orig-fun bib)
  "After pre-formatting, append the bib basename in italics and smaller font."
  (funcall orig-fun bib)
  (let* ((filename (citar-cache--bibliography-filename bib))
         ;; Propertize the name with italic and a subtle/small face
         (bibname (propertize (concat "  " (file-name-nondirectory filename))
                             'face '(:slant italic :height 0.8 :inherit shadow)))
         (preformatted (citar-cache--bibliography-preformatted bib)))
    (maphash (lambda (_key preform)
               (when (and preform (listp (cdr preform)))
                 (setf (cdr preform)
                       (nconc (cdr preform) (list bibname)))))
             preformatted)))
(with-eval-after-load 'citar
  (setq citar-templates
        '((main . "${author editor:25%sn}   ${date year issued:8}   ${title:30}")
          ;; We increase the width for =source= to 30 and put it at the start of the suffix
          (suffix . " ${=source=:30} ${=key= id:15}")
          (preview . "${author editor:%etal} (${year issued date}) ${title}.\n")
          (note . "Notes on ${author editor:%etal}, ${title}"))))
(advice-add 'citar-cache--preformat-bibliography :around #'ysb/citar-append-bib-basename)
)

(use-package citar-embark
  :after (citar embark)
  :no-require
  :config (citar-embark-mode)
)

(use-package consult-latex-ref
  ;; :vc (consult-latex-ref :url "https://github.com/sandyber/consult-latex-ref" 
  ;;               :rev :newest
  ;;               :branch "main"
  ;;               )
 :load-path "c:/backup/Dropbox/zzz/.emacs.d" ;use-package needs a literal string here, keep in sync with ysb/dotdir
  :init
  (let* ((src (expand-file-name "consult-latex-ref.el" ysb/dotdir))
         (elc (expand-file-name "consult-latex-ref.elc" ysb/dotdir)))
    (when (file-newer-than-file-p src elc)
      (byte-compile-file src)))
  :config
  (consult-customize consult-latex-toc :preview-key 'any)
  (setq consult-latex-ref-prompt-for-command t)
  (setq consult-latex-ref-label-command-alist
        '(("eq:" . "eqref")))
  (setq consult-latex-ref-commands
      '("ref" "eqref" "pageref"))
)

;; consult-tex-ref (disabled) moved to atticT490.el

(use-package biblio
  :ensure t
  :pin melpa
  :config
  (remove-hook 'biblio-init-hook #'biblio-dblp-backend)
  (remove-hook 'biblio-init-hook #'biblio-arxiv-backend)
  (remove-hook 'biblio-init-hook #'biblio-ieee-backend)
  (remove-hook 'biblio-init-hook #'biblio-hal-backend)
  (setq-default
   biblio-bibtex-use-autokey t
   bibtex-autokey-name-year-separator ""
   bibtex-autokey-year-title-separator ""
   bibtex-autokey-year-length 4
   bibtex-autokey-titlewords 0
   bibtex-autokey-titleword-length 0 ;; -1 means exactly one
   bibtex-autokey-titlewords-stretch 0
   bibtex-autokey-titleword-separator ""
   bibtex-autokey-titleword-case-convert 'upcase)
  )
;;---------------------- LaTeX custom -------------------------
(defun call-reftex-label-directly ()
  (interactive)
  (let ((current-prefix-arg 1)) ;; emulate C-u --- rescan the document when labels are messed up
    (call-interactively 'reftex-label) ;; invoke reftex-reference
    )
  )
(use-package reftex
  :commands turn-on-reftex
  :bind (("C-c l" . call-reftex-label-directly)
         :map reftex-mode-map ;override reftex default bindings (moved here from generalT490.el)
         ("C-c [" . citar-insert-citation)
         ("C-c (" . consult-latex-create-label)
         ("C-c )" . consult-latex-insert-ref)
         ("C-c =" . consult-latex-toc)
         ("C-c &" . consult-latex-view-crossref))
  :config
  (setq reftex-auto-view-crossref nil) ;disable the now irrelevant message when inside \ref{blah}
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-ref-macro-prompt nil)
  (setq reftex-cite-prompt-optional-args t); to prompt for pages in \citet
  (setq reftex-label-alist
        '(("eqclaim" ?e "eq:" "~\\eqref{%s}" t  ("eqclaim" "eq." )) ("exe" ?e "eq:" "~\\eqref{%s}" t  ("exe" "exe." )) ("eqtext" ?e nil nil t)) reftex-insert-label-flags '("s" "ft"))
  (setq font-latex-match-reference-keywords
        '(("hiddenref" "{")
          ))
  (setq LaTeX-reftex-cite-format-auto-activate nil); required for customising reftex-cite-format
  (setq reftex-cite-format
        '((?\C-m. "~\\citet[]{%l}")
          (?t    . "~\\citet[]{%l}")
          (?T    . "~\\citet*[]{%l}")
          (?c    . "~\\cite[]{%l}")
          (?p    . "~\\citep[]{%l}")
          (?P    . "~\\citep*[]{%l}")
          (?a    . "~\\citeauthor{%l}")
          (?A    . "~\\citeauthor*{%l}")
          (?n    . "~\\nocite{%l}")
          (?y. "~\\citeyearpar[]{%l}")
          (?Y. "~\\citeyear[]{%l}")
          ))
  )

(use-package pdf-tools ;if install within emacs fails, try installing this first in msys2
  :ensure t
  :pin melpa
  :init
  (pdf-loader-install)
  :config
  (define-key pdf-view-mode-map (kbd "C-<f11>") 'toggle-frame-fullscreen)
  (setq mouse-wheel-follow-mouse t)
  (setq pdf-view-resize-factor 1.10)
  (setq pdf-view-use-scaling t)
  (setq auto-revert-interval 0.5)
  (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12"))
  :hook
  (pdf-view-mode . auto-revert-mode)
;  (LaTeX-mode . pdf-tools-install)
  )
;;
(use-package pdf-sync
  :ensure nil                          ; part of the pdf-tools package
  :commands (pdf-sync-forward-search)  ; autoload from pdf-sync.el
  :init
  (with-eval-after-load 'latex         ; latex.el defines LaTeX-mode-map
    (define-key LaTeX-mode-map (kbd "C-c C-g") #'pdf-sync-forward-search)))
;; 
(use-package pdf-view-restore
  :ensure t
  :pin melpa
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  )
;; 
(use-package pdffontetc
  ;; :vc (pdffontetc :url "https://github.com/emacsomancer/pdffontetc"
  ;;                   :branch "main")
  :ensure t
  :pin melpa
  :config
  (defun pdffontetc-extra-keys ()
    "Set some additional keybindings in PDF-Tools for pdffontetc info functions."
    ;; `O' for `Org-style' Info, = pdf metadata in orgish display:
    (local-set-key (kbd "O") #'pdffontetc-display-metadata-org-style)
    ;; `T' for `Typeface', i.e., Font info [since `F' is already taken]:
    (local-set-key (kbd "T") #'pdffontetc-display-font-information)
    ;; `U' for `Unified' info, i.e., both Metadata and Font info:
    (local-set-key (kbd "U") #'pdffontetc-display-combined-metadata-and-font-info))
  ;; 
  (add-hook 'pdf-view-mode-hook #'pdffontetc-extra-keys)
  )
;;-------------------------------------------------------------
;; electric-ospl (disabled; less useful than twauctex) moved to atticT490.el
(use-package visual-fill-column
 ;; :disabled
  :ensure t
  :pin melpa
  :after twauctex
  :config
  (global-visual-fill-column-mode 0)
  (visual-fill-column-mode 0)
  )
(use-package twauctex ; also https://www.reddit.com/r/emacs/comments/agufyc/text_mode_automatic_newline_insertion_after/ and https://emacs.stackexchange.com/a/64364/19901
  :vc (twauctex :url "https://github.com/jeeger/twauctex" 
                :rev :newest                              ;comment this and reinstall if troubles with ebib!
                :branch "main"
                )
  ;; :disabled
  :ensure t ; nil required for the :load-path !!! 
  ;; :load-path "c:/backup/Dropbox/zzz/.emacs.d" ;; this version can't be used becauce it screws up ebib
  :hook
  (LaTeX-mode . twauctex-mode)
  (markdown-mode . twauctex-mode)
  :config
  ;; (with-eval-after-load 'twauctex ;need this to disable visual-fill-column-mode for the earlier version of twauctex: https://chatgpt.com/share/69738ad8-f644-800b-8f0e-bb478c96d365
  ;; (advice-add 'visual-fill-column-mode :around
  ;;             (lambda (orig-fun &optional arg)
  ;;               (unless twauctex-mode
  ;;                 (funcall orig-fun arg)))))
   (setq twauctex-use-visual-fill-column nil) ;; comment this and go with the chatgpt solution above if troubles with ebib!
;;  (add-hook 'twauctex-mode-hook (lambda () (visual-fill-column-mode -1)))
  (setq-local sentence-end-double-space nil)
  ;; 1. The Toggle Variable
  (defvar ysb/tw-pulse-enabled nil "Whether the twauctex sentence pulse is active by default.")
  ;; 2. The Toggle Command
  (defun ysb/tw-pulse-toggle ()
    "Toggle the sentence pulse highlight on and off."
    (interactive)
    (setq ysb/tw-pulse-enabled (not ysb/tw-pulse-enabled))
    (message "Sentence pulse %s" (if ysb/tw-pulse-enabled "enabled" "disabled")))
  ;; 3. The Timer-Safe Pulse Logic (with Toggle Check)
  (defun ysb/tw-simple-pulse (&rest _args)
    "Pulses the previous line if enabled and at the start of a new line."
    (when (and ysb/tw-pulse-enabled (bolp))
      (save-excursion
        (forward-line -1)
        (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
          (overlay-put ov 'priority 100)
          (overlay-put ov 'face '(:background "#fffacd" :foreground "black"))
          (run-with-timer 0.4 nil (lambda (timer-ov) 
                                    (when (overlayp timer-ov)
                                      (delete-overlay timer-ov))) 
                          ov)))))
  ;; 4. Attach to the function 
  (with-eval-after-load 'twauctex
    (advice-add 'twauctex-electric-sentence-end-space :after #'ysb/tw-simple-pulse))
 )
(defun ysb/latex-buffer-face-mode-variable ()
  "Adjust buffer-local face for LaTeX based on monitor width."
  (if (> ysb-monitor-width ysb-large-display-threshold);; ysb-is-large-display
    (progn
        (message "Large display detected (%dmm): setting default font Consolas-18" ysb-monitor-width)
        (setq buffer-face-mode-face '(:height 181 :family "Consolas")))
    (progn
        (message "Small display detected (%dmm): setting default font Consolas-14" ysb-monitor-width)      
        (setq buffer-face-mode-face '(:height 141 :family "Consolas"))))
  
  (buffer-face-mode)
  )
  ; Consolas 18=181,14=141,20=203 (Samsung 27"); SF Mono-139; 108,102. 120(Samsung),114(T400)

;; local configuration for TeX modes
(defun ysb/TeX-remove-macro () ;https://emacs.stackexchange.com/a/7997/19901
  "Remove current macro and return `t'.  If no macro at point,
return `nil'."
  (interactive)
  (when (TeX-current-macro)
    (let ((bounds (TeX-find-macro-boundaries))
          (brace  (save-excursion
                    (goto-char (1- (TeX-find-macro-end)))
                    (TeX-find-opening-brace))))
      (delete-region (1- (cdr bounds)) (cdr bounds))
      (delete-region (car bounds) (1+ brace)))
    t)
  )
(defun ysb/LaTeX-setup ()
  "Buffer-local LaTeX-mode setup (collected from former lambda hooks).
The `LaTeX-item-list' and `LaTeX-label-alist' additions must stay here,
per buffer, not in :config: AUCTeX styles may rebuild these lists
buffer-locally, which loses entries added only once globally."
  (setq global-hl-line-mode t) ;buffer-local here: see make-variable-buffer-local in generalT490.el
  (LaTeX-add-environments '("xlist" LaTeX-env-item) '("xlisti" LaTeX-env-item)
                          '("xlistA" LaTeX-env-item) '("exe" LaTeX-env-item)
                          '("eqtext" LaTeX-env-label) '("eqclaim" LaTeX-env-label))
  (add-to-list 'LaTeX-label-alist '("eqclaim" . "eq:"))
  (dolist (env '("exe" "xlist" "xlisti" "xlistA"))
    (add-to-list 'LaTeX-item-list
                 `(,env lambda () (let (TeX-insert-braces) (TeX-insert-macro "ex"))))))
;; (use-package auctex
;;   :ensure t
;;   :pin gnu-devel)
(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :bind
  ([f4] . TeX-engine-set)
  ("C-x m" . ysb/TeX-remove-macro)
  ("C-c i" . ysb/latex-insert-input) ;insert input file in consult
  :custom
  (TeX-default-mode 'LaTeX-mode)
  (TeX-command-list ;https://gitlab.com/jabranham/emacs/blob/master/init.el; auctex 14 needs capital letters in modes...
   '(("LaTeX" "%`%l%(mode)%' %T" TeX-run-TeX nil
      (LaTeX-mode docTeX-mode)
      :help "Run LaTeX")
     ("BibTeX" "bibtex %(O?aux)" TeX-run-BibTeX nil
      (plain-TeX-mode LaTeX-mode docTeX-mode conTeXt-mode Texinfo-mode AmSTeX-mode)
      :help "Run BibTeX")
     ("Biber" "biber %(output-dir) %s" TeX-run-Biber nil
      (plain-TeX-mode LaTeX-mode docTeX-mode Texinfo-mode AmSTeX-mode)
      :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function nil nil
      :help "Run Viewer")
     ))
  :config
  (with-eval-after-load 'tex ;dealing with syntax-propertize warnings
  (define-advice TeX-find-macro-boundaries
      (:before (&optional lower-bound _signature) ysb/propertize-first)
    (when (and lower-bound
               (>= lower-bound (point-min))
               (<= lower-bound (point-max)))
      (syntax-propertize lower-bound))))
  (setq TeX-kpathsea-path-delimiter ;ensures lookup for \usepackage
      (if (string-match-p "miktex" (or (executable-find "kpsewhich") "")) nil ":"))
  (defun ysb/suppress-synctex-supersession (fn &rest args) ;get rid of the useless confirmations to update .synctex.gz when using pdf-tools
    (unless (string-match-p "\\.synctex\\.gz\\'" (or buffer-file-name ""))
      (apply fn args)))
  (advice-add 'ask-user-about-supersession-threat :around #'ysb/suppress-synctex-supersession)
  (with-eval-after-load 'recentf ;recentf is deferred: don't let a void recentf-exclude abort this :config
    (add-to-list 'recentf-exclude "\\.synctex\\.gz\\'")) ;don't litter my recentf list
  (setq-default TeX-PDF-mode t)
  (add-hook 'TeX-update-style-hook ;to avoid weird issues with bclogo/pstricks triggering dvi production
          (lambda () (setq TeX-PDF-from-DVI nil)))
   (setq TeX-parse-self t
         TeX-save-query nil ;https://tex.stackexchange.com/questions/291045/how-to-make-tex-command-run-all-save-automatically-without-asking
         font-latex-user-keyword-classes
         ;; Make \scq display with the same face as \mbox: https://emacs.stackexchange.com/questions/35802/fontification-of-custom-latex-macro
         ;; \mbox is classified as "function" in font-latex.el and as such it uses the face font-lock-function-name-face.
         '(("my-function"  (("change" "{") ("scq" "{") ("word" "{")) font-lock-constant-face command)
           ("my-string"  (("sn" "[{") ("ex" "[{")) font-lock-string-face command)
           ("my-citation"  (("citeyearc" "*[[{")) font-lock-constant-face command))
         font-latex-match-bold-command-keywords
         '(("mn" "{") ("marg" "{") ("mnshort" "{") ("mnlong" "{"))
         LaTeX-csquotes-close-quote "}"
         LaTeX-csquotes-open-quote "\\enquote{"
         LaTeX-command-style '(("" "%(PDF)%(latex) %(output-dir) -synctex=1 -file-line-error %(extraopts) %S%(PDFout)"))
         TeX-view-program-selection '((output-pdf "pdf-tools"))
         TeX-source-correlate-start-server t
         TeX-source-correlate-method 'synctex ;moved out of a per-buffer hook: these are global
         TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))
         )
   ;; item and label tables are per-buffer business: see ysb/LaTeX-setup
   (define-key LaTeX-mode-map (kbd "$") 'self-insert-command); for electric-pair-mode
   :hook
   (LaTeX-mode . ysb/LaTeX-setup)
   (LaTeX-mode . ysb/latex-buffer-face-mode-variable)
   (LaTeX-mode . turn-on-reftex)
   (LaTeX-mode . visual-line-mode)
   (LaTeX-mode . flyspell-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . electric-pair-mode)
   )
(use-package latex-labeler
  :ensure t
  :config
  (dolist (env '("exe" "xlist" "xlisti" "xlistA" "xlista"))
    (add-to-list 'latex-labeler-math-envs env t))
  )

;;----------------------- LaTeX word count -----------------------------
;; Elisp equivalent of latexpcnt.bat, plus a count excluding footnotes.
;; Runs latexpand and texcount with `call-process', so no shell and no
;; console window are involved.

(defvar ysb/latexpcnt-log "*latexpcnt*"
  "Buffer holding latexpand and texcount output.")

(defvar ysb/latexpcnt-nofootnote-rules
  "%macro \\footnote [option:ignore,ignore]\n\
%macro \\footnotetext [option:ignore,ignore]\n"
  "TeXcount instructions that make it skip footnote text.
Written to a temporary option file and passed with -opt=.  The
option:ignore entry covers the \\footnote[3]{...} form.  Captions and
headers are unaffected.")

(defun ysb/latexpcnt--opt-file ()
  "Write `ysb/latexpcnt-nofootnote-rules' to a temp file, return its name."
  (let ((f (make-temp-file "texcount" nil ".opt")))
    (with-temp-file f (insert ysb/latexpcnt-nofootnote-rules))
    f))

(defun ysb/latexpcnt--count (file &rest flags)
  "Return the texcount total for FILE, run with FLAGS.
Return 0 when FILE is missing or texcount prints nothing usable.
Stderr is discarded, matching the 2>nul redirections in the bat file."
  (if (not (file-exists-p file))
      0
    (with-temp-buffer
      (apply #'call-process "texcount" nil (list t nil) nil
             (append '("-1" "-sum") flags (list file)))
      (string-to-number (buffer-string)))))

(defun ysb/latex-word-count ()
  "Count the words of the current LaTeX project, comments excluded.
Flatten the master file with latexpand (expanding the .bbl when one
exists), run texcount on the result, subtract the count of abstract.tex,
write countdef.tex for \\input, and report the numbers in the echo area.

Four macros are defined in countdef.tex: \\wcbody, \\wcbib, \\wcabs and
\\wcnofn, the last being the body count with footnote text excluded.

The master file is resolved as elsewhere in the config, so the command
works from a subfile buffer too."
  (interactive)
  (unless (buffer-file-name)
    (user-error "This buffer isn't visiting a file"))
  (save-buffer)
  (let* ((master (if (fboundp 'consult-latex-ref--master-file)
                     (consult-latex-ref--master-file)
                   (buffer-file-name)))
         (default-directory (file-name-directory master))
         (stem (file-name-base master))
         (bbl  (concat stem ".bbl"))
         (all  (concat stem "-all.tex"))
         (log  (get-buffer-create ysb/latexpcnt-log)))
    (with-current-buffer log (erase-buffer))
    (unless (zerop (apply #'call-process "latexpand" nil log nil
                          (append '("--empty-comments")
                                  (when (file-exists-p bbl)
                                    (list "--expand-bbl" bbl))
                                  (list (file-name-nondirectory master)
                                        "-o" all))))
      (pop-to-buffer log)
      (user-error "latexpand failed: see the %s buffer" ysb/latexpcnt-log))
    (let ((opt (ysb/latexpcnt--opt-file)))
      (unwind-protect
          (let* ((nofn-flag (concat "-opt=" opt))
                 (full      (ysb/latexpcnt--count all))
                 (fullbib   (ysb/latexpcnt--count all "-incbib"))
                 (fullnofn  (ysb/latexpcnt--count all nofn-flag))
                 (abs       (ysb/latexpcnt--count "abstract.tex"))
                 (absnofn   (ysb/latexpcnt--count "abstract.tex" nofn-flag))
                 (body      (- full abs))
                 (bib       (- fullbib abs))
                 (nofn      (- fullnofn absnofn)))
            (unless (file-exists-p "abstract.tex")
              (message "Warning: abstract.tex not found, so the fallback text counts in the body"))
            (with-temp-file "countdef.tex"
              (insert (format "\\def\\wcbody{%d}\n\\def\\wcbib{%d}\n\\def\\wcabs{%d}\n\\def\\wcnofn{%d}\n"
                              body bib abs nofn)))
            (with-current-buffer log
              (goto-char (point-max))
              (insert (format "\nabstract:              %d\nbody:                  %d\nbody, no footnotes:    %d\nfootnotes:             %d\nbody + bibliography:   %d\nWrote countdef.tex in %s\n"
                              abs body nofn (- body nofn) bib default-directory)))
            (message "abstract %d | body %d | no footnotes %d | with bib %d -> countdef.tex"
                     abs body nofn bib))
        (delete-file opt)))))

;; Optional binding.
(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-c w") #'ysb/latex-word-count))
;;
(defvar ysb/pdf2txt-script "c:/git/geekystuff/pdfgeek/pdf2txt-ysb.py"
  "Full path to the pdf2txt-ysb.py script.
Named pdf2txt-ysb, not pdf2txt: pdfminer.six installs its own
pdf2txt.py in c:/python313/Scripts/, which comes earlier on PATH
and would otherwise shadow this one.")

(defun ysb/pdf2txt (&optional arg)
  "Run `ysb/pdf2txt-script' on the master PDF of the current TeX project.
The txt output lands next to the PDF.  Warns if the buffer has
unsaved changes or the source is newer than the PDF.  In dired,
uses the file at point.  With prefix ARG, edit the command."
  (interactive "P")
  (let* ((pdf (cond ((derived-mode-p 'dired-mode)
                     (dired-get-filename))
                    ((and buffer-file-name
                          (string-suffix-p ".tex" buffer-file-name)
                          (fboundp 'TeX-master-file))
                     (expand-file-name (TeX-master-file "pdf")
                                       (TeX-master-directory)))
                    (buffer-file-name
                     (concat (file-name-sans-extension buffer-file-name)
                             ".pdf"))
                    (t (read-file-name "PDF: " nil nil t nil
                                       (lambda (f) (string-suffix-p ".pdf" f))))))
         (stale
          (cond ((not (file-exists-p pdf))
                 (user-error "No PDF: %s" pdf))
                ((buffer-modified-p)
                 "buffer has unsaved changes")
                ((and buffer-file-name
                      (file-newer-than-file-p buffer-file-name pdf))
                 "source is newer than the PDF"))))
    (when (and stale
               (not (y-or-n-p (format "%s -- run pdf2txt anyway? "
                                      (capitalize stale)))))
      (user-error "Aborted; recompile first"))
    (let ((default-directory (file-name-directory pdf))
          (coding-system-for-read 'utf-8)
          (cmd (format "python \"%s\" \"%s\""
                       ysb/pdf2txt-script
                       (file-name-nondirectory pdf))))
      (compile (if arg (read-string "Command: " cmd) cmd)))))
;;
(provide 'latexT490)
;;; latexT490.el ends here
