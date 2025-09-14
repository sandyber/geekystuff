(provide 'latexT)
;;------------------------------------------------------------
;(define-key minibuffer-local-completion-map " " 'self-insert-command); make SPC normal in completion, otherwise use C-q to insert SPC: https://emacs.stackexchange.com/questions/68423/remove-space-autocomplete-from-m-x
(use-package ebib
  :defer t
  :pin melpa
  :ensure t  
  :config ;https://ogbe.net/emacs/references
  (setq
   ebib-default-directory "/backup/Dropbox/bib"
   ebib-bibtex-dialect 'BibTeX
;   ebib-file-search-dirs (quote ("/backup/Dropbox/bib"))
   ebib-index-display-fields (quote ("title")))
;   ebib-popup-entry-window nil ;; no effect unless ebib-layout set to 'index-only
;   ebib-layout 'index-only
;   ebib-notes-directory "/backup/Dropbox/bib/notes"
;      ebib-notes-storage 'one-file-per-note
;      ebib-notes-locations '("/backup/Dropbox/bib/notes")
;      ebib-notes-default-file nil
;; `ebib' uses `bibtex.el' to auto-generate keys for us
  (setq bibtex-autokey-year-length 4)
  (setq bibtex-autokey-name-case-convert-function 'capitalize)
  (setq bibtex-autokey-titleword-separator "")
  (setq bibtex-autokey-name-year-separator "")
  (setq bibtex-autokey-year-title-separator "")
  (setq bibtex-autokey-titleword-length 0)
  (setq bibtex-autokey-titlewords 0)
;  (setq bibtex-autokey-titleword-ignore ;; I took "On" out of this
;        '("A" "An" "The" "Eine?" "Der" "Die" "Das" "[^[:upper:]].*" ".*[^[:upper:][:lower:]0-9].*"))
  ;; make ebib window easier to deal with
  (setq ebib-index-window-size 30)
  (require 'ebib-biblio)
  (define-key ebib-index-mode-map (kbd "Y") #'ebib-copy-entry-as-kill)
  (define-key ebib-index-mode-map (kbd "B") #'ebib-biblio-import-doi)
  (define-key biblio-selection-mode-map (kbd "e") #'ebib-biblio-selection-import)
  :custom
  (ebib-preload-bib-files '("evolnat.bib" "hypocrisy.bib" "progress.bib"));"respect.bib" ))
  (ebib-bib-search-dirs '("/backup/Dropbox/bib"))
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
;; (use-package tex-site
;;   :custom
;;   (TeX-command-list
;;    '(("LaTeX" "%`%l%(mode)%' %T" TeX-run-TeX nil
;;       (latex-mode doctex-mode)
;;       :help "Run LaTeX")
;;      ("BibTeX" "bibtex %(O?aux)" TeX-run-BibTeX nil
;;       (plain-tex-mode latex-mode doctex-mode context-mode texinfo-mode ams-tex-mode)
;;       :help "Run BibTeX")
;;      ("Biber" "biber %(output-dir) %s" TeX-run-Biber nil
;;       (plain-tex-mode latex-mode doctex-mode texinfo-mode ams-tex-mode)
;;       :help "Run Biber")
;;      ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
;;      ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files"))))
;;    (setq TeX-PDF-mode t)
;; ;;   (setq TeX-engine 'xetex)
;; ;; (setq TeX-auto-save t)
;;    (setq TeX-parse-self t)
;;    (setq-default TeX-master nil)
;(global-set-key (kbd "<f4>") 'TeX-engine-set)
(defun call-reftex-label-directly ()
  (interactive)
  (let ((current-prefix-arg 1)) ;; emulate C-u --- rescan the document when labels are messed up
    (call-interactively 'reftex-label) ;; invoke reftex-reference
    )
  )
(use-package reftex
  :commands turn-on-reftex
  :bind ("C-c l" . call-reftex-label-directly)
  :config
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
;; (add-hook 'LaTeX-mode-hook
;;           #'(lambda ()
;;             (define-key LaTeX-mode-map (kbd "$") 'self-insert-command))); for electric-pair-mode
    ;(setq reftex-label-alist '(AMSTeX))
;-----------------------------------------
;; Automatically add labels to eqclaim
;; (add-hook
;;  'LaTeX-mode-hook
;;  (lambda ()
;;    (LaTeX-add-environments
;;     '("xlist" LaTeX-env-item)
;;     '("exe" LaTeX-env-item)
;;     '("eqtext" LaTeX-env-label)
;;     '("eqclaim" LaTeX-env-label))
;;    (add-to-list 'LaTeX-label-alist '("eqclaim" . "eq:"))))
;(add-hook 'LaTeX-mode-hook 'add-my-latex-environments);; non-automatically add labels
;(defun add-my-latex-environments ()
;  (LaTeX-add-environments
;   '("eqclaim" LaTeX-env-label)
;   '("eqtext" LaTeX-env-label)))
;
;(setq reftex-label-alist
;   '(("axiom"   ?e "ax:"  "~\\ref{%s}" nil ("axiom"   "ax.") nil)
;     ("theorem" ?h "thr:" "~\\ref{%s}" t   ("theorem" "th."))))
;    
;; (setq reftex-label-alist
;;   ;'(("eqclaim" ?e nil nil t)
;;   '(("eqclaim" ?e "eq:" "~\\eqref{%s}" t  ("eqclaim" "eq." ))
;;   ("exe" ?e "eq:" "~\\eqref{%s}" t  ("exe" "exe." ))
;;    ("eqtext" ?e nil nil t))
;;   reftex-insert-label-flags '("s" "ft"))
;;;
;(global-set-key (kbd "C-c l") 'call-reftex-label-directly)
;;;
;(setq reftex-label-alist
;      '(
;        ("eq" ?e "eq:%f" "~\ref{%s}" nil ("eqclaim") nil)
;        ))
;
;; (add-hook 'LaTeX-mode-hook #'(lambda ()
;;      (add-to-list 'LaTeX-item-list
;; 		  '("exe" lambda () (let (TeX-insert-braces) (TeX-insert-macro "ex"))))))
;; (add-hook 'LaTeX-mode-hook #'(lambda ()
;;      (add-to-list 'LaTeX-item-list
;; 		  '("xlist" lambda () (let (TeX-insert-braces) (TeX-insert-macro "ex"))))))
;(setq TeX-engine 'xetex)
;
;
;(add-hook 'LaTeX-mode-hook 'my-latex-buffer-face-mode-variable)
;
;(setq LaTeX-verbatim-environments-local '("quote" "quotation")) ;; to highlight quote environments
;----------------PDF Tools: requires synctex=1 -----------------------
;; (setq LaTeX-command-style '(("" "%(PDF)%(latex) -synctex=1 -file-line-error %S%(PDFout)")))
;; (add-hook 'LaTeX-mode-hook
;;             (lambda ()
;; 	      (setq TeX-source-correlate-method 'synctex)
;;               (setq TeX-source-correlate-start-server t)))
;
;; to use pdfview with auctex
;(add-hook 'LaTeX-mode-hook 'pdf-tools-install)

;; to use pdfview with auctex
;; (setq TeX-view-program-selection '((output-pdf "pdf-tools"))
;;        TeX-source-correlate-start-server t)
;; (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

(use-package pdf-tools ;if install within emacs fails, try installing this first in msys2
  :ensure t
  :pin melpa
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :bind ("C-c C-g" . pdf-sync-forward-search)
  :defer t
  :config
  (define-key pdf-view-mode-map (kbd "C-<f11>") 'toggle-frame-fullscreen)
  (setq mouse-wheel-follow-mouse t)
  (setq pdf-view-resize-factor 1.10)
  (setq auto-revert-interval 0.5)
  (setq pdf-view-midnight-colors '("#ff9900" . "#0a0a12" ));
  :hook
  (pdf-view-mode . auto-revert-mode)
  (LaTeX-mode . pdf-tools-install)
  )
(use-package pdf-view-restore
  :ensure t
  :pin melpa
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  )
;--------------------------------------------------------------
(defun ysb/latex-buffer-face-mode-variable ()
  (interactive)
;; (if (string= system-name "T490S")
;;      (setq buffer-face-mode-face '(:overline nil :underline nil :slant normal :weight normal :height 141 :width normal :family "Consolas")) 
;;   (setq buffer-face-mode-face '(:overline nil :underline nil :slant normal :weight normal :height 181 :width normal :family "Consolas"))
;;   )
(cond ((string= system-name ysb/system);"T490S")
       (progn
         (message "%s" (concat "Running on "  system-name ", setting setting LaTeX font Consolas-141..."))
 ;        (message "Running on T490S, setting LaTeX font Consolas-141...")
         (setq buffer-face-mode-face '(:height 141 :family "Consolas"))))
      ((string= system-name ysb/systemalt);"T490s")
       (progn
         (message "%s" (concat "Running on "  system-name ", setting setting LaTeX font Consolas-141..."))
;         (message "Running on T490s, setting LaTeX font Consolas-141...") ;;due to weird behaviour mixing T490s and T490S
         (setq buffer-face-mode-face '(:height 141 :family "Consolas"))))
      (t                                ;default setting
       (progn
         (message "%s" (concat "Running on a larger monitor on "  system-name " machine, setting LaTeX font Consolas-181..."))
;         (message "Running on a large monitor, setting LaTeX font Consolas-181...")
(setq buffer-face-mode-face '(:height 181 :family "Consolas"))))
  )
  (buffer-face-mode))
; Consolas 18=181,14=141,20=203 (Samsung 27"); SF Mono-139; 108,102. 120(Samsung),114(T400)

;; local configuration for TeX modes
;; (defun ysb/latex-mode-setup ()
;;   (setq-local company-backends
;;               (append '((company-math-symbols-latex company-latex-commands))
;;                       company-backends)))
(use-package latex
  :ensure auctex
  :bind ([f4] . TeX-engine-set)
  :custom
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
     ;("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("View" "%V" TeX-run-discard-or-function nil nil
      :help "Run Viewer")
;     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ;("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ))
  :config
   (setq-default TeX-PDF-mode t)
   (setq TeX-parse-self t
         TeX-save-query nil ;https://tex.stackexchange.com/questions/291045/how-to-make-tex-command-run-all-save-automatically-without-asking
         ;; font-latex-match-reference-keywords
         ;; '(("ex" "[{") ("sn" "[{"))
         font-latex-user-keyword-classes
         ;; Make \scq display with the same face as \mbox: https://emacs.stackexchange.com/questions/35802/fontification-of-custom-latex-macro
         ;; \mbox is classified as "function" in font-latex.el and as such it uses the face font-lock-function-name-face.
         '(("my-function"  (("change" "{") ("scq" "{") ("word" "{")) font-lock-constant-face command)
           ("my-string"  (("sn" "[{") ("ex" "[{")) font-lock-string-face command))
         font-latex-match-bold-command-keywords
         '(("mn" "{") ("marg" "{") ("mnshort" "{") ("mnlong" "{"))
         LaTeX-csquotes-close-quote "}"
         LaTeX-csquotes-open-quote "\\enquote{"
         LaTeX-command-style '(("" "%(PDF)%(latex) -synctex=1 -file-line-error %S%(PDFout)"))
         TeX-view-program-selection '((output-pdf "pdf-tools"))
         TeX-source-correlate-start-server t
         TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))
         )
   :hook
   ;; (LaTeX-mode . ysb/latex-mode-setup)
   (LaTeX-mode . (lambda () (setq global-hl-line-mode nil)))
   (LaTeX-mode . ysb/latex-buffer-face-mode-variable)
   (LaTeX-mode . turn-on-reftex)
   (LaTeX-mode . visual-line-mode)
   (LaTeX-mode . flyspell-mode)
   (LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . (lambda ()(setq TeX-source-correlate-method 'synctex) (setq TeX-source-correlate-start-server t)))
   (LaTeX-mode . (lambda ()(LaTeX-add-environments '("xlist" LaTeX-env-item) '("xlisti" LaTeX-env-item) '("exe" LaTeX-env-item) '("eqtext" LaTeX-env-label) '("eqclaim" LaTeX-env-label))(add-to-list 'LaTeX-label-alist '("eqclaim" . "eq:"))))
   (LaTeX-mode . (lambda ()(add-to-list 'LaTeX-item-list '("exe" lambda () (let (TeX-insert-braces) (TeX-insert-macro "ex"))))))
   (LaTeX-mode . (lambda ()(add-to-list 'LaTeX-item-list '("xlist" lambda () (let (TeX-insert-braces) (TeX-insert-macro "ex"))))))
   (LaTeX-mode . (lambda ()(add-to-list 'LaTeX-item-list '("xlisti" lambda () (let (TeX-insert-braces) (TeX-insert-macro "ex"))))))
   (LaTeX-mode . (lambda ()(define-key LaTeX-mode-map (kbd "$") 'self-insert-command))); for electric-pair-mode
  ;; (add-to-list 'LaTeX-font-list
  ;;              '(?\C-q "\\scq{" "}"))
  ;; (add-to-list 'LaTeX-font-list
  ;;              '(?\C-w "\\word{" "}"))
   )
;--------------------------------------------------------------
;
;; (defun wrap-footnote ()
;;   (interactive)
;;     (goto-char (region-end)) (insert "}")
;;     (goto-char (region-beginning)) (insert "\\footnote{")
;;     (goto-char (region-end))
;; )
;; (global-set-key (kbd "M-F") 'wrap-footnote)
;
;(eval-after-load "tex"
;  '(setcdr (assoc "BibTeX" TeX-command-list)
;          '("bibtex --min-crossrefs=1 %s"
;            TeX-run-BibTeX nil t :help "Run BibTeX with ...")))
; -------------------------------------
; Inserting and wrapping single quotes
; -------------------------------------
;; (defun mg-TeX-insert-single-quote (force)
;;   "Insert the appropriate quotation marks for TeX.
;; Inserts ` or ' depending on the context.  With prefix argument
;; FORCE, always inserts ' characters."
;;   (interactive "*P")
;;   (if (or force
;;       ;; Do not insert TeX quotes in verbatim, math or comment constructs.
;;       (and (fboundp 'font-latex-faces-present-p)
;;            (font-latex-faces-present-p '(font-latex-verbatim-face
;;                          font-latex-math-face
;;                          font-lock-comment-face))
;;            (font-latex-faces-present-p '(font-latex-verbatim-face
;;                          font-latex-math-face
;;                          font-lock-comment-face)
;;                        (1- (point))))
;;       (texmathp)
;;       (and (TeX-in-comment) (not (eq major-mode 'doctex-mode))))
;;       (self-insert-command (prefix-numeric-value force))
;;     (TeX-update-style)
;;     (let* ((open-quote "`")
;;        (close-quote "'"))
;;       (insert (cond ((bobp)
;;              open-quote)
;;             ((= (preceding-char) (string-to-char TeX-esc))
;;              ?\')
;;             ((= (preceding-char) ?\")
;;              ?')
;;             ((save-excursion
;;                (forward-char (- (length open-quote)))
;;                (looking-at (regexp-quote open-quote)))
;;              (delete-char (- (length open-quote)))
;;              ?')
;;             ((save-excursion
;;                (forward-char (- (length close-quote)))
;;                (looking-at (regexp-quote close-quote)))
;;              (delete-char (- (length close-quote)))
;;              ?')
;;             ((save-excursion
;;                (forward-char -1)
;;                (looking-at "[ \t\n]\\|\\s("))
;;              open-quote)
;;             (t
;;              close-quote))))))

;;   (add-hook 'LaTeX-mode-hook
;;       (lambda ()
;;         (define-key LaTeX-mode-map (kbd "'") 'mg-TeX-insert-single-quote)))
;
