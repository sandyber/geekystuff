;;; markdownT490.el --- Markdown and pandoc setup  -*- lexical-binding: t; -*-
;;
;; Split out of generalT490.el.  Loaded after latexT490.el, which defines
;; ysb/latex-buffer-face-mode-variable and sets up twauctex and citar.
;;
(defvar ysb/cite2link-script (expand-file-name "cite2link.py" ysb/bibdir)
  "Path to cite2link.py.")

(defvar ysb/cite2link-python "python"
  "Python executable used to run cite2link.py.")

;; ------- helpers shared by the pandoc commands below -------
(defun ysb/markdown--run (log program &rest args)
  "Run PROGRAM with ARGS synchronously, logging output into LOG.
Show LOG and signal a user-error if PROGRAM exits nonzero."
  (unless (zerop (apply #'call-process program nil log nil args))
    (pop-to-buffer log)
    (user-error "%s failed; see the %s buffer" program log)))

(defun ysb/markdown--reset-log (log)
  "Erase the LOG buffer if it exists."
  (when (get-buffer log)
    (with-current-buffer log (erase-buffer))))

(defun ysb/markdown--file ()
  "Save the current buffer and return its file name.
Signal a user-error when the buffer isn't visiting a file."
  (unless (buffer-file-name)
    (user-error "This buffer isn't visiting a file"))
  (save-buffer)
  (buffer-file-name))

(defun ysb/markdown--yaml-bibliography ()
  "Return the bibliography path from the buffer's YAML block, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "---\n")
      (let ((end (save-excursion (forward-line 1)
                                 (re-search-forward "^---$" nil t))))
        (when (and end
                   (re-search-forward
                    "^bibliography:[ \t]*\"?\\([^\"\n]+\\)\"?" end t))
          (match-string 1))))))

(defun ysb/markdown--bib ()
  "Return the bibliography for the current buffer, asking if needed."
  (or (ysb/markdown--yaml-bibliography)
      (read-file-name "Bibliography: " ysb/bibdir)))
;; -------------------------------- COMMANDS --------------------------------------------
(defun ysb/markdown--lo-locked-p (file)
  "Return LibreOffice's lock file for FILE, if it exists."
  (let ((lock (expand-file-name
               (concat ".~lock." (file-name-nondirectory file) "#")
               (file-name-directory file))))
    (and (file-exists-p lock) lock)))
(defun ysb/markdown--check-free (file)
  "Signal a user-error if FILE is open in LibreOffice."
  (when-let* ((lock (ysb/markdown--lo-locked-p file)))
    (user-error "%s is open in LibreOffice -- close it first (lock file: %s)"
                (file-name-nondirectory file) lock)))
;; -------------------------------- LINKED DOCX -----------------------------------------
(defun ysb/markdown-publish-linked-docx ()
  "Embed cited refs, turn citations into links, render Word, open it.
Pipeline: FILE.md -> FILE-all.md -> FILE-all-linked.md -> .docx."
  (interactive)
  (let* ((md     (ysb/markdown--file))
         (stem   (file-name-sans-extension md))
         (all    (concat stem "-all.md"))
         (linked (concat stem "-all-linked.md"))
         (docx   (concat stem "-all-linked.docx"))
         (bib    (ysb/markdown--bib))
         (log    "*md-pipeline*"))
    (ysb/markdown--reset-log log)
    (ysb/markdown--check-free docx)
    ;; 1. embed only the cited entries
    (ysb/markdown--run log "pandoc" "lua"
                       (expand-file-name "embed-cited-refs.lua" ysb/bibdir)
                       md (expand-file-name bib))
    ;; 2. citations -> markdown links
    (ysb/markdown--run log ysb/cite2link-python ysb/cite2link-script all)
    ;; 3. render the linked file
    (ysb/markdown--run log "pandoc" linked "-o" docx)
    (message "Wrote %s" docx)
    (if (fboundp 'w32-shell-execute)
        (w32-shell-execute "open" docx)
      (browse-url-of-file docx))))
;; -------------------------------- LINKED HTML -----------------------------------------
(defun ysb/markdown-publish-linked-html ()
  "Embed cited refs, turn citations into links, render HTML, show it.
Pipeline: FILE.md -> FILE-all.md -> FILE-all-linked.md -> .html."
  (interactive)
  (let* ((md     (ysb/markdown--file))
         (stem   (file-name-sans-extension md))
         (all    (concat stem "-all.md"))
         (linked (concat stem "-all-linked.md"))
         (html   (concat stem "-all-linked.html"))
         (bib    (ysb/markdown--bib))
         (log    "*md-pipeline*"))
    (ysb/markdown--reset-log log)
    ;; 1. embed only the cited entries
    (ysb/markdown--run log "pandoc" "lua"
                       (expand-file-name "embed-cited-refs.lua" ysb/bibdir)
                       md (expand-file-name bib))
    ;; 2. citations -> markdown links
    (ysb/markdown--run log ysb/cite2link-python ysb/cite2link-script all)
    ;; 3. render the linked file
    (ysb/markdown--run log "pandoc" linked "-s" "--mathjax" "-o" html)
    (message "Wrote %s" html)
    (browse-url-of-file html)))
;; ********************** MD => DOCX *********************************
(defun ysb/markdown-to-docx ()
  "Render the current Markdown buffer to Word with pandoc and open it."
  (interactive)
  (let* ((md   (ysb/markdown--file))
         (docx (concat (file-name-sans-extension md) ".docx")))
    (ysb/markdown--check-free docx)
    (ysb/markdown--reset-log "*pandoc*")
    (ysb/markdown--run "*pandoc*" "pandoc"
                       md "--citeproc" "-M" "link-citations=true" "-o" docx)
    (if (fboundp 'w32-shell-execute)
        (w32-shell-execute "open" docx)
      (browse-url-of-file docx))))
;; ********************** MD => HTML *********************************
(defun ysb/markdown-to-html ()
  "Render the current Markdown buffer to linked HTML with pandoc and show it."
  (interactive)
  (let* ((md   (ysb/markdown--file))
         (html (concat (file-name-sans-extension md) ".html")))
    (ysb/markdown--reset-log "*pandoc*")
    (ysb/markdown--run "*pandoc*" "pandoc"
                       md "--citeproc"
;;                     "--lua-filter=c:/backup/Dropbox/bib/link-cites-to-url.lua"
;;                     "--bibliography=c:/backup/Dropbox/bib/refs.bib"
                       "-M" "link-citations=true"
                       "-s" "--mathjax"
                       "-o" html)
    (browse-url-of-file html)))
;; *** PDF ***
(defun ysb/markdown-to-pdf ()
  "Render the current Markdown buffer to PDF with pandoc and show it."
  (interactive)
  (let* ((md  (ysb/markdown--file))
         (pdf (concat (file-name-sans-extension md) ".pdf")))
    (ysb/markdown--reset-log "*pandoc*")
    (ysb/markdown--run "*pandoc*" "pandoc"
                       md "--citeproc"
                       "-M" "link-citations=true"
                       "-V" "geometry:margin=1in"
                       "-V" "colorlinks=true"
                       "-V" "urlcolor=NavyBlue"
                       "-V" "linkcolor=NavyBlue"
                       "-V" "citecolor=NavyBlue"
                       "-o" pdf)
    (let ((buf (find-buffer-visiting pdf)))
      (if buf
          (with-current-buffer buf (revert-buffer t t t))
        (find-file-other-window pdf)))))
;; *** view-pdf ***
(defun ysb/markdown-view-pdf ()
  "Open the PDF matching the current Markdown buffer in pdf-tools."
  (interactive)
  (unless (buffer-file-name)
    (user-error "This buffer isn't visiting a file"))
  (let ((pdf (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
    (if (file-exists-p pdf)
        (find-file-other-window pdf)
      (user-error "No PDF yet: %s -- render it first (<f5>)" pdf))))
;;
(defun ysb/twauctex-markdown-tweaks ()
  "In Markdown buffers, keep twauctex's OSPL but drop its LaTeX-only keys."
  (when (boundp 'twauctex-mode-map)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map twauctex-mode-map)
      (define-key map (kbd "\"") #'self-insert-command)
      (define-key map (kbd "_")  #'self-insert-command)
      (define-key map (kbd "&")  #'self-insert-command)
      (setq minor-mode-overriding-map-alist
            (cons (cons 'twauctex-mode map)
                  (assq-delete-all 'twauctex-mode
                                   minor-mode-overriding-map-alist))))))
;; *** embed-bib ***
(defun ysb/markdown-embed-bib ()
  "Write FILE-all.md with only the cited references embedded."
  (interactive)
  (let ((md  (ysb/markdown--file))
        (bib (ysb/markdown--bib)))
    (ysb/markdown--reset-log "*pandoc*")
    (ysb/markdown--run "*pandoc*" "pandoc" "lua"
                       (expand-file-name "embed-cited-refs.lua" ysb/bibdir)
                       md (expand-file-name bib))
    (message "Wrote %s-all.md" (file-name-sans-extension md))))
;;
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (add-hook 'markdown-mode-hook #'ysb/twauctex-markdown-tweaks 90)
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode)
         (markdown-mode . ysb/latex-buffer-face-mode-variable))
  :bind (:map markdown-mode-map
              ([f5] . ysb/markdown-to-pdf)
              ([C-f5] . ysb/markdown-to-docx)
              ("C-x <f5>" . ysb/markdown-publish-linked-docx)
              ([C-f7] . ysb/markdown-to-html)
              ("C-x <f7>" . ysb/markdown-publish-linked-html)
              ("C-c C-v" . ysb/markdown-view-pdf)
              ("C-c C-b" . ysb/markdown-embed-bib)            
              ("C-c [" . citar-insert-citation))
  :init
  (setq markdown-command "pandoc -f gfm")
  (setq markdown-fontify-code-blocks-natively t))
;; ********************** MD => TXT *********************************
(require 'ucs-normalize)

(defvar ysb/markdown-txt-pandoc-args
  '("--citeproc" "-t" "plain" "--wrap=none")
  "Pandoc arguments used by `ysb/markdown-to-txt'.
\"--wrap=none\" puts each paragraph on one line, which suits the
`visual-line-mode' of `txt-mode'.  Use \"--columns=72\" with a wrap
setting of \"auto\" instead if you want hard-wrapped text.")

(defvar ysb/markdown-txt-ascii t
  "Whether `ysb/markdown-to-txt' folds its output to ASCII.
A prefix argument to the command inverts this.")

(defvar ysb/markdown-warn-stale t
  "Whether `ysb/markdown--check-fresh' asks before rendering over a target
that is newer than its source.  Set to nil if the question gets in the way.")

(defvar ysb/ascii-replacements
  '(("\u2018" . "'") ("\u2019" . "'") ("\u201a" . "'") ("\u201b" . "'")
    ("\u2032" . "'") ("\u2039" . "'") ("\u203a" . "'")
    ("\u201c" . "\"") ("\u201d" . "\"") ("\u201e" . "\"") ("\u201f" . "\"")
    ("\u2033" . "\"") ("\u00ab" . "\"") ("\u00bb" . "\"")
    ("\u2010" . "-") ("\u2011" . "-") ("\u2012" . "-") ("\u2013" . "-")
    ("\u2014" . "--") ("\u2015" . "--") ("\u2212" . "-")
    ("\u2026" . "...") ("\u2022" . "*") ("\u00b7" . "*")
    ("\u00a0" . " ") ("\u2007" . " ") ("\u2009" . " ") ("\u202f" . " ")
    ("\u200b" . "") ("\ufeff" . "")
    ("\u00e6" . "ae") ("\u00c6" . "AE") ("\u0153" . "oe") ("\u0152" . "OE")
    ("\u00df" . "ss") ("\u00f8" . "o") ("\u00d8" . "O")
    ("\u0142" . "l") ("\u0141" . "L") ("\u0111" . "d") ("\u0110" . "D")
    ("\u00f0" . "d") ("\u00d0" . "D") ("\u00fe" . "th") ("\u00de" . "Th")
    ("\u2192" . "->") ("\u2190" . "<-") ("\u2194" . "<->")
    ("\u21d2" . "=>") ("\u21d0" . "<=") ("\u21d4" . "<=>")
    ("\u2264" . "<=") ("\u2265" . ">=") ("\u2260" . "!=") ("\u2248" . "~=")
    ("\u00d7" . "x") ("\u00f7" . "/") ("\u00b1" . "+/-")
    ("\u00a9" . "(c)") ("\u00ae" . "(R)") ("\u2122" . "(TM)")
    ("\u00b0" . " deg") ("\u00a7" . "sec. ") ("\u00b6" . "par. ")
    ("\u2020" . "+") ("\u2021" . "++")
    ("\u00bc" . " 1/4") ("\u00bd" . " 1/2") ("\u00be" . " 3/4"))
  "ASCII spellings for the non-ASCII characters that have an obvious one.
Used by `ysb/asciify-buffer' before it strips accents.")

(defun ysb/asciify-buffer ()
  "Fold the current buffer to ASCII as far as that can be done sensibly.
First the substitutions in `ysb/ascii-replacements', then accented letters
are decomposed and stripped of their marks, so \"cafe'\" spelt with an
acute comes out as \"cafe\".  Characters with no ASCII spelling -- Greek,
mathematical operators, CJK -- are left alone.  Returns an alist of those
leftovers and their counts."
  (dolist (pair ysb/ascii-replacements)
    (goto-char (point-min))
    (while (search-forward (car pair) nil t)
      (replace-match (cdr pair) t t)))
  (goto-char (point-min))
  (while (re-search-forward "[^[:ascii:]]" nil t)
    (let ((folded (save-match-data
                    (replace-regexp-in-string
                     "[\u0300-\u036f]" ""
                     (ucs-normalize-NFKD-string (match-string 0))))))
      (when (string-match-p "\\`[[:ascii:]]+\\'" folded)
        (replace-match folded t t))))
  (let ((left ()))
    (goto-char (point-min))
    (while (re-search-forward "[^[:ascii:]]" nil t)
      (let* ((ch (char-before))
             (cell (assq ch left)))
        (if cell
            (setcdr cell (1+ (cdr cell)))
          (push (cons ch 1) left))))
    (nreverse left)))

(defun ysb/ascii--report (left)
  "Describe LEFT, the leftover alist returned by `ysb/asciify-buffer'."
  (mapconcat (lambda (c)
               (format "%c (U+%04X)%s" (car c) (car c)
                       (if (> (cdr c) 1) (format " x%d" (cdr c)) "")))
             left ", "))

(defun ysb/markdown--strip-comments ()
  "Delete every HTML comment in the current buffer.
The whitespace before a comment goes with it, so \"word. <!-- @kant -->\"
comes out as \"word.\".  A line holding nothing but a comment is deleted
whole, newline included, so no blank line stays behind; the same goes for
a comment spanning several lines.  Runs of blank lines left over are
collapsed to one.  An unterminated comment is left untouched."
  (goto-char (point-min))
  (let (beg end at-bol)
    (while (search-forward "<!--" nil t)
      (setq beg (match-beginning 0))
      (if (not (search-forward "-->" nil t))
          (goto-char (point-max))       ; unterminated: leave it alone
        (setq end (point))
        (goto-char beg)
        (skip-chars-backward " \t")
        (setq beg (point)
              at-bol (bolp))
        (goto-char end)
        (cond
         ;; nothing but the comment on its line(s): take the newline too
         ((and at-bol (looking-at "[ \t]*\\(?:\n\\|\\'\\)"))
          (setq end (match-end 0)))
         ;; the comment opens a line: drop the whitespace after it as well
         (at-bol
          (skip-chars-forward " \t")
          (setq end (point))))
        (delete-region beg end)
        (goto-char beg))))
  (goto-char (point-min))
  (while (re-search-forward "\n\\{3,\\}" nil t)
    (replace-match "\n\n")))

(defun ysb/markdown--check-fresh (target)
  "Ask before writing TARGET when doing so may lose work.
Three cases: the source buffer has unsaved changes; TARGET is open in a
buffer with unsaved changes of its own; TARGET is newer on disk than the
source, so the render either repeats the last one or overwrites a file
edited by hand.  Answering no aborts.  The third question is skipped when
the source buffer is dirty, since those edits postdate TARGET whatever the
timestamps say.

Call this before the buffer is saved: the save makes the source newer
than TARGET and so hides the third case."
  (let ((name  (file-name-nondirectory target))
        (buf   (find-buffer-visiting target))
        (dirty (buffer-modified-p)))
    (when dirty
      (unless (y-or-n-p (format "%s has unsaved changes -- save and render? "
                                (buffer-name)))
        (user-error "Nothing written")))
    (when (and buf (buffer-modified-p buf))
      (unless (y-or-n-p
               (format "%s has unsaved changes of its own -- overwrite? " name))
        (user-error "Kept %s" name)))
    (when (and ysb/markdown-warn-stale
               (not dirty)              ; unsaved edits postdate TARGET anyway
               (file-exists-p target)
               (file-newer-than-file-p target (buffer-file-name)))
      (unless (y-or-n-p
               (format "%s is newer than the Markdown source -- render anyway? "
                       name))
        (user-error "Kept %s" name)))))

(defun ysb/markdown-to-txt (&optional invert-ascii)
  "Render the current Markdown buffer to plain text with pandoc and open it.
HTML comments are removed first, the cite2link markers among them, because
pandoc drops the comment but keeps the space before it, leaving \"word .\".
The stripped copy is written beside the source, so a relative bibliography
path in the YAML block still resolves, and is deleted afterwards.

The result is then folded to ASCII if `ysb/markdown-txt-ascii' says so.
INVERT-ASCII, the prefix argument, inverts that choice.  Whatever the
folding cannot spell in ASCII is reported in the echo area.

`ysb/markdown--check-fresh' asks first if the buffer has unsaved changes,
or if the txt is newer than the Markdown source."
  (interactive "P")
  (unless (buffer-file-name)
    (user-error "This buffer isn't visiting a file"))
  (unless (member (downcase (or (file-name-extension (buffer-file-name)) ""))
                  '("md" "markdown" "mdown" "mkd"))
    (user-error "%s isn't a Markdown file"
                (file-name-nondirectory (buffer-file-name))))
  (ysb/markdown--check-fresh
   (concat (file-name-sans-extension (buffer-file-name)) ".txt"))
  (let* ((md    (ysb/markdown--file))
         (txt   (concat (file-name-sans-extension md) ".txt"))
         (tmp   (concat (make-temp-name (concat (file-name-sans-extension md)
                                                "-tmp"))
                        ".md"))
         (raw   (concat (file-name-sans-extension tmp) ".txt"))
         (ascii (xor invert-ascii ysb/markdown-txt-ascii))
         left)
    (ysb/markdown--reset-log "*pandoc*")
    (unwind-protect
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file tmp
            (insert-file-contents md)
            (ysb/markdown--strip-comments))
          ;; pandoc writes to a scratch file, and the target is written once,
          ;; here.  Rewriting a file that a buffer visits twice over makes
          ;; Emacs ask whether the buffer should really be edited
          (apply #'ysb/markdown--run "*pandoc*" "pandoc"
                 (append ysb/markdown-txt-pandoc-args
                         (list tmp "-o" raw)))
          (setq left (with-temp-file txt
                       (insert-file-contents raw)
                       ;; folded after pandoc, never before: the markdown
                       ;; reader's smart quotes would put curly punctuation
                       ;; back into a file folded on the way in
                       (when ascii (ysb/asciify-buffer)))))
      (ignore-errors (delete-file tmp))
      (ignore-errors (delete-file raw)))
    ;; shown in another window, but the point stays in the Markdown buffer,
    ;; so hitting the key twice cannot feed the txt back to pandoc
    (let ((buf (find-buffer-visiting txt)))
      (if (null buf)
          (setq buf (find-file-noselect txt))
        (with-current-buffer buf
          ;; the file was rewritten under the buffer's feet, so record the new
          ;; timestamp before reverting.  Without this Emacs sees the revert as
          ;; an edit to a buffer whose file changed behind its back and asks
          ;; "really edit the buffer?", which has nothing to do with anything
          ;; the user did
          (set-visited-file-modtime)
          (revert-buffer t t t)))
      (display-buffer buf))
    (message "Wrote %s%s" txt
             (if left (concat " -- not ASCII: " (ysb/ascii--report left)) ""))))

(provide 'markdownT490)
;;; markdownT490.el ends here
