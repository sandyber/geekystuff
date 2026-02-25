;;; consult-latex-ref.el --- Consult-powered reference navigation for LaTeX -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; A standalone consult-powered reference management package for LaTeX.
;;
;; It provides consult-powered label search across multi-file projects,
;; with context-aware ref command selection (\\ref, \\eqref, \\autoref etc.)
;; and label-prefix heuristics (fig:, eq:, tab:, sec: etc.).
;;
;; A parse cache avoids re-scanning unchanged files on every invocation.
;; Cache entries are invalidated when a file is saved, or when its
;; modification time has changed since the last scan.  Files open in
;; modified buffers are always scanned live.
;;
;; Commands:
;;  `consult-latex-find-label'        - Jump to a \\label definition
;;  `consult-latex-insert-ref'        - Insert a \\ref (or variant) at point
;;  `consult-latex-view-crossref'     - Show the \\label for the \\ref at point
;;  `consult-latex-create-label'      - Create a context-aware \\label at point
;;  `consult-latex-list-orphan-labels'- List labels never referenced in the project
;;  `consult-latex-toc'               - Browse project table of contents
;;  `consult-latex-ref-reset-cache'   - Manually clear the parse cache

;;; Code:

(require 'consult)

(require 'tex nil t)

(declare-function TeX-parse-macro "ext:tex")

(defgroup consult-latex-ref nil
  "Consult-powered reference navigation for LaTeX."
  :group 'consult)

;;; Customization: ref insertion

(defcustom consult-latex-ref-commands
  '("ref" "eqref" "autoref" "pageref" "nameref")
  "List of reference commands available for insertion."
  :group 'consult-latex-ref
  :type '(repeat string))

(defcustom consult-latex-ref-default-command "ref"
  "Default command for reference insertion.
Must be a member of `consult-latex-ref-commands'."
  :group 'consult-latex-ref
  :type 'string
  :safe 'always)

(defcustom consult-latex-ref-prompt-for-command nil
  "Whether to prompt for a ref command when inserting.
When nil, use `consult-latex-ref-default-command' or the
heuristically determined command."
  :group 'consult-latex-ref
  :type 'boolean
  :safe 'always)

(defcustom consult-latex-ref-prefix "~"
  "String to insert before the ref command.
Set to \"~\" for a non-breaking space (recommended),
or \"\" for no prefix."
  :group 'consult-latex-ref
  :type 'string
  :safe 'always)

(defcustom consult-latex-ref-label-command-alist
  '(("eq:"  . "eqref")
    ("fig:" . "autoref")
    ("tab:" . "autoref")
    ("sec:" . "autoref")
    ("ch:"  . "autoref")
    ("app:" . "autoref"))
  "Alist mapping label prefixes to default ref commands.
When a label matches a prefix, the associated command is used
as the default instead of `consult-latex-ref-default-command'.
Only used when `consult-latex-ref-prompt-for-command' is nil."
  :group 'consult-latex-ref
  :type '(alist :key-type string :value-type string))

;;; Customization: label creation

(defcustom consult-latex-ref-equation-environments
  '("equation" "equation*" "align" "align*" "alignat" "alignat*"
    "gather" "gather*" "multline" "multline*" "flalign" "flalign*"
    "eqnarray" "eqnarray*" "exe" "xlist" "xlisti")
  "Environments treated as equation-like for label creation.
Labels get the prefix from `consult-latex-ref-equation-prefix' and
a sequential number."
  :group 'consult-latex-ref
  :type '(repeat string))

(defcustom consult-latex-ref-figure-environments
  '("figure" "figure*" "subfigure" "wrapfigure")
  "Environments treated as figures for label creation.
Labels get the prefix from `consult-latex-ref-figure-prefix' and
a slug from the \\\\caption."
  :group 'consult-latex-ref
  :type '(repeat string))

(defcustom consult-latex-ref-table-environments
  '("table" "table*" "longtable" "supertabular")
  "Environments treated as tables for label creation.
Labels get the prefix from `consult-latex-ref-table-prefix' and
a slug from the \\\\caption."
  :group 'consult-latex-ref
  :type '(repeat string))

(defcustom consult-latex-ref-theorem-environments
  '("theorem" "lemma" "proposition" "corollary" "definition"
    "remark" "example" "proof" "claim" "conjecture")
  "Environments treated as theorem-like for label creation.
Labels get the prefix \"thm:\", \"lem:\", \"prop:\", etc.
based on the environment name."
  :group 'consult-latex-ref
  :type '(repeat string))

(defcustom consult-latex-ref-equation-prefix "eq:"
  "Label prefix for equation-like environments."
  :group 'consult-latex-ref
  :type 'string)

(defcustom consult-latex-ref-figure-prefix "fig:"
  "Label prefix for figure environments."
  :group 'consult-latex-ref
  :type 'string)

(defcustom consult-latex-ref-table-prefix "tab:"
  "Label prefix for table environments."
  :group 'consult-latex-ref
  :type 'string)

(defcustom consult-latex-ref-section-prefix "sec:"
  "Label prefix for section commands."
  :group 'consult-latex-ref
  :type 'string)

(defcustom consult-latex-ref-chapter-prefix "ch:"
  "Label prefix for chapter commands."
  :group 'consult-latex-ref
  :type 'string)

(defcustom consult-latex-ref-theorem-prefix-alist
  '(("theorem"     . "thm:")
    ("lemma"       . "lem:")
    ("proposition" . "prop:")
    ("corollary"   . "cor:")
    ("definition"  . "def:")
    ("remark"      . "rem:")
    ("example"     . "ex:")
    ("proof"       . "prf:")
    ("claim"       . "clm:")
    ("conjecture"  . "conj:"))
  "Alist mapping theorem-like environment names to label prefixes."
  :group 'consult-latex-ref
  :type '(alist :key-type string :value-type string))

(defcustom consult-latex-ref-slug-max-length 40
  "Maximum length of the slug portion of a generated label."
  :group 'consult-latex-ref
  :type 'integer)

;;; Customization: TOC

(defcustom consult-latex-ref-toc-commands
  '(("\\part"           . 0)
    ("\\chapter"        . 1)
    ("\\section"        . 2)
    ("\\subsection"     . 3)
    ("\\subsubsection"  . 4)
    ("\\paragraph"      . 5)
    ("\\subparagraph"   . 6))
  "Alist of sectioning commands and their indent levels for TOC display."
  :group 'consult-latex-ref
  :type '(alist :key-type string :value-type integer))

;;; Faces

(defface consult-latex-ref-label-face
  '((t :inherit font-lock-constant-face))
  "Face for label names in the consult completion list."
  :group 'consult-latex-ref)

(defface consult-latex-ref-toc-section-face
  '((t :inherit font-lock-keyword-face))
  "Face for section titles in the TOC consult list."
  :group 'consult-latex-ref)

(defface consult-latex-ref-toc-file-face
  '((t :inherit font-lock-comment-face))
  "Face for file names in the TOC consult list."
  :group 'consult-latex-ref)

(defface consult-latex-ref-orphan-face
  '((t :inherit font-lock-warning-face))
  "Face for orphaned label names in the orphan list."
  :group 'consult-latex-ref)

;;; History variables

(defvar consult-latex-ref-command-history nil
  "History variable for ref command selection.")

(defvar consult-latex-ref-label-history nil
  "History variable for label creation.")

;;; Cache data structures
;;
;; consult-latex-ref--cache:
;;   hash: master-path ->
;;     (:mtimes MTIMES :ticks TICKS :labels LABELS :toc TOC)
;;     MTIMES: hash: file-path -> mtime
;;     TICKS:  hash: file-path -> buffer-chars-modified-tick at scan time
;;               (nil if file was scanned from disk, not from a live buffer)
;;     LABELS: list of (label-string . (file-path . char-position))
;;     TOC:    list of (title level file-path char-position)
;;
;; consult-latex-ref--file-to-masters:
;;   reverse index hash: file-path -> list of master-paths

(defvar consult-latex-ref--cache (make-hash-table :test 'equal)
  "Parse cache: master-path -> plist of mtimes, labels, and toc.")

(defvar consult-latex-ref--file-to-masters (make-hash-table :test 'equal)
  "Reverse index: file-path -> list of master paths that include it.")

;;; Internal: file collection

(defun consult-latex-ref--master-file ()
  "Return the absolute path to the project master file."
  (let* ((master-name (or (and (boundp 'TeX-master)
                               (stringp TeX-master)
                               TeX-master)
                          (file-name-sans-extension (buffer-file-name))))
         (master (expand-file-name
                  (concat master-name
                          (unless (string-suffix-p ".tex" master-name) ".tex"))
                  (file-name-directory (buffer-file-name)))))
    master))

(defun consult-latex-ref--collect-files (master)
  "Return list of all tex files in the project with MASTER as root."
  (let* ((master-dir (file-name-directory master))
         (files (list master)))
    (with-temp-buffer
      (insert-file-contents master)
      (goto-char (point-min))
      (while (re-search-forward
              "\\\\\\(?:input\\|include\\){\\([^}]+\\)}" nil t)
        (let ((included (expand-file-name
                         (concat (match-string 1)
                                 (unless (string-suffix-p ".tex" (match-string 1))
                                   ".tex"))
                         master-dir)))
          (when (file-exists-p included)
            (push included files)))))
    (delete-dups (nreverse files))))

;;; Internal: cache management

(defun consult-latex-ref--file-mtime (file)
  "Return the modification time of FILE."
  (file-attribute-modification-time (file-attributes file)))

(defun consult-latex-ref--buffer-modified-p (file)
  "Return non-nil if FILE is open in a modified buffer."
  (when-let* ((buf (find-buffer-visiting file)))
    (buffer-modified-p buf)))

(defun consult-latex-ref--buffer-tick (file)
  "Return the buffer-chars-modified-tick for FILE's buffer, or nil."
  (when-let* ((buf (find-buffer-visiting file)))
    (buffer-chars-modified-tick buf)))

(defun consult-latex-ref--file-stale-p (file old-mtimes old-ticks)
  "Return non-nil if FILE needs rescanning.
A file is stale if:
- It has no cache entry yet
- Its mtime has changed (saved externally)
- It is open in a modified buffer
- It was previously scanned from a live buffer whose tick has changed
  (catches undo-back-to-save-point: buffer-modified-p becomes nil but
   the cached data was from a different buffer state)"
  (let ((old-mtime (gethash file old-mtimes))
        (new-mtime (consult-latex-ref--file-mtime file))
        (old-tick  (gethash file old-ticks))
        (new-tick  (consult-latex-ref--buffer-tick file)))
    (or (null old-mtime)
        (not (equal old-mtime new-mtime))
        (buffer-modified-p (or (find-buffer-visiting file) (current-buffer)))
        ;; If the cache was built from a live buffer scan (old-tick non-nil),
        ;; rescan whenever the current tick differs — this catches undo to
        ;; save point (tick changes even though buffer-modified-p is nil)
        (and old-tick new-tick (not (= old-tick new-tick)))
        ;; If cache was from a live buffer but buffer is now unmodified and
        ;; tick matches disk, still rescan once to sync with disk state
        (and old-tick (null new-tick)))))

(defun consult-latex-ref--toc-regexp ()
  "Return a regexp matching all sectioning commands."
  (concat "\\\\\\("
          (mapconcat (lambda (e) (regexp-quote (substring (car e) 1)))
                     consult-latex-ref-toc-commands "\\|")
          "\\)\\*?[[:space:]]*{\\([^}]*\\)}"))

(defun consult-latex-ref--scan-file (file)
  "Scan FILE for \\\\label and sectioning commands.
Returns a plist (:labels LABELS :toc TOC :tick TICK).
TICK is the buffer-chars-modified-tick if scanned from a live buffer,
or nil if scanned from disk.
If FILE is open in a modified buffer, scans the live buffer instead."
  (let ((labels ())
        (toc ())
        (tick nil))
    (cl-flet ((scan ()
                (goto-char (point-min))
                (while (re-search-forward "\\\\label{\\([^}]*\\)}" nil t)
                  (push (cons (match-string-no-properties 1)
                              (cons file (match-beginning 1)))
                        labels))
                (goto-char (point-min))
                (while (re-search-forward (consult-latex-ref--toc-regexp) nil t)
                  (let* ((cmd (concat "\\" (match-string-no-properties 1)))
                         (title (match-string-no-properties 2))
                         (level (or (cdr (assoc cmd consult-latex-ref-toc-commands)) 2))
                         (pos (match-beginning 0)))
                    (push (list title level file pos) toc)))))
      (if-let* ((buf (find-buffer-visiting file))
                (_ (buffer-modified-p buf)))
          (with-current-buffer buf
            (setq tick (buffer-chars-modified-tick))
            (save-excursion (scan)))
        (with-temp-buffer
          (insert-file-contents file)
          (scan))))
    (list :labels (nreverse labels) :toc (nreverse toc) :tick tick)))

(defun consult-latex-ref--update-cache (master)
  "Update the cache for project rooted at MASTER.
Re-scans only stale or modified files.
Returns a plist (:labels LABELS :toc TOC)."
  (let* ((files (consult-latex-ref--collect-files master))
         (entry (gethash master consult-latex-ref--cache))
         (old-mtimes (if entry (plist-get entry :mtimes)
                       (make-hash-table :test 'equal)))
         (old-ticks  (if entry (plist-get entry :ticks)
                       (make-hash-table :test 'equal)))
         (new-mtimes (make-hash-table :test 'equal))
         (new-ticks  (make-hash-table :test 'equal))
         (old-labels (if entry (plist-get entry :labels) nil))
         (old-toc    (if entry (plist-get entry :toc) nil))
         (stale-files (seq-filter
                       (lambda (file)
                         (consult-latex-ref--file-stale-p
                          file old-mtimes old-ticks))
                       files))
         (stale-set (make-hash-table :test 'equal)))
    (dolist (f stale-files) (puthash f t stale-set))
    (let* ((kept-labels (seq-filter
                         (lambda (lab)
                           (let ((file (cadr lab)))
                             (and (member file files)
                                  (not (gethash file stale-set)))))
                         old-labels))
           (kept-toc (seq-filter
                      (lambda (e)
                        (let ((file (nth 2 e)))
                          (and (member file files)
                               (not (gethash file stale-set)))))
                      old-toc))
           (new-data   (mapcar #'consult-latex-ref--scan-file stale-files))
           (new-labels (mapcan (lambda (d) (plist-get d :labels)) new-data))
           (new-toc    (mapcan (lambda (d) (plist-get d :toc))    new-data))
           (all-labels (append kept-labels new-labels))
           (all-toc    (append kept-toc new-toc)))
      ;; Record mtimes
      (dolist (file files)
        (puthash file (consult-latex-ref--file-mtime file) new-mtimes))
      ;; Record ticks for freshly scanned files
      (cl-mapcar (lambda (file data)
                   (when-let* ((tick (plist-get data :tick)))
                     (puthash file tick new-ticks)))
                 stale-files new-data)
      ;; Carry over ticks for non-stale files
      (dolist (file files)
        (unless (gethash file stale-set)
          (when-let* ((tick (gethash file old-ticks)))
            (puthash file tick new-ticks))))
      ;; Store cache
      (puthash master (list :mtimes new-mtimes
                            :ticks  new-ticks
                            :labels all-labels
                            :toc    all-toc)
               consult-latex-ref--cache)
      ;; Update reverse index
      (dolist (file files)
        (let ((masters (gethash file consult-latex-ref--file-to-masters)))
          (unless (member master masters)
            (puthash file (cons master masters)
                     consult-latex-ref--file-to-masters))))
      (list :labels all-labels :toc all-toc))))

(defun consult-latex-ref--invalidate-file (file)
  "Invalidate cache entries for FILE in all projects that include it."
  (when-let* ((masters (gethash file consult-latex-ref--file-to-masters)))
    (dolist (master masters)
      (when-let* ((entry (gethash master consult-latex-ref--cache)))
        (remhash file (plist-get entry :mtimes))
        (remhash file (plist-get entry :ticks))))))

(defun consult-latex-ref--after-save ()
  "Hook: invalidate cache for the saved file."
  (when (and buffer-file-name
             (string-suffix-p ".tex" buffer-file-name))
    (consult-latex-ref--invalidate-file buffer-file-name)))

(add-hook 'after-save-hook #'consult-latex-ref--after-save)

;;;###autoload
(defun consult-latex-ref-reset-cache ()
  "Clear the entire label and TOC parse cache."
  (interactive)
  (clrhash consult-latex-ref--cache)
  (clrhash consult-latex-ref--file-to-masters)
  (message "consult-latex-ref: cache cleared"))

;;; Internal: consult candidate building

(defun consult-latex-ref--make-candidates (labels)
  "Build consult location candidates from LABELS.
LABELS is a list of (label-string . (file-path . char-position))."
  (let ((candidates ()))
    (dolist (lab labels)
      (let* ((label-str (car lab))
             (file (cadr lab))
             (pos (cddr lab))
             (buf (or (find-buffer-visiting file)
                      (find-file-noselect file t))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (save-excursion
              (goto-char pos)
              (push (consult--location-candidate
                     (concat (propertize label-str
                                         'face 'consult-latex-ref-label-face)
                             "  ")
                     (point-marker)
                     (line-number-at-pos)
                     (point-marker))
                    candidates))))))
    (nreverse candidates)))

;;; Internal: annotation

(defun consult-latex-ref--annotate (cand)
  "Show line number and source line for CAND as annotation."
  (when-let* ((loc (get-text-property 0 'consult-location cand))
              (marker (car loc))
              (line (cdr loc)))
    (list cand
          (propertize (format "  %d  " line) 'face 'consult-line-number-prefix)
          (when (marker-buffer marker)
            (with-current-buffer (marker-buffer marker)
              (save-excursion
                (goto-char marker)
                (propertize
                 (string-trim (buffer-substring-no-properties
                               (pos-bol) (pos-eol)))
                 'face 'completions-annotations)))))))

(defun consult-latex-ref--toc-annotate (cand)
  "Show fixed-width line number and filename for TOC CAND as annotation."
  (when-let* ((loc (get-text-property 0 'consult-location cand))
              (marker (car loc))
              (line (cdr loc)))
    (list cand
          (propertize (format "  %5d  " line) 'face 'consult-line-number-prefix)
          (when (marker-buffer marker)
            (propertize (buffer-name (marker-buffer marker))
                        'face 'consult-latex-ref-toc-file-face)))))

;;; Internal: label search

(defun consult-latex-ref--find-label (&optional initial)
  "Search for \\\\label definitions across project files.
If INITIAL is a string, pre-fill the consult input with it.
Returns the marker position of the selected label."
  (let* ((master (consult-latex-ref--master-file))
         (current-buf (current-buffer))
         (data (consult-latex-ref--update-cache master))
         (refs (seq-uniq
                (consult-latex-ref--make-candidates (plist-get data :labels))
                #'string=)))
    (let ((head refs) (old nil))
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
     :prompt "Labels: "
     :annotate #'consult-latex-ref--annotate
     :category 'consult-location
     :sort nil
     :require-match t
     :lookup #'consult--lookup-location
     :history '(:input consult--line-history)
     :initial initial
     :add-history (thing-at-point 'symbol)
     :default (car refs)
     :state (consult--jump-preview))))

;;; Internal: command selection

(defun consult-latex-ref--command-for-label (label)
  "Return the appropriate ref command for LABEL."
  (or (cdr (seq-find (lambda (entry)
                       (string-prefix-p (car entry) label))
                     consult-latex-ref-label-command-alist))
      consult-latex-ref-default-command))

(defun consult-latex-ref--select-command (label)
  "Prompt for a ref command, with a smart default for LABEL."
  (completing-read "Ref command: "
                   consult-latex-ref-commands
                   nil t nil
                   'consult-latex-ref-command-history
                   (consult-latex-ref--command-for-label label)))

;;; Internal: label creation helpers

(defun consult-latex-ref--slugify (text)
  "Convert TEXT to a label slug.
Strips LaTeX commands, lowercases, replaces spaces/punctuation with -."
  (let* (;; Remove LaTeX commands like \textbf{foo} -> foo
         (s (replace-regexp-in-string "\\\\[a-zA-Z]+{\\([^}]*\\)}" "\\1" text))
         ;; Remove remaining backslash commands
         (s (replace-regexp-in-string "\\\\[a-zA-Z]+" "" s))
         ;; Remove remaining braces and special chars
         (s (replace-regexp-in-string "[{}$%&_^]" "" s))
         ;; Lowercase
         (s (downcase s))
         ;; Replace whitespace and punctuation runs with -
         (s (replace-regexp-in-string "[[:space:][:punct:]]+" "-" s))
         ;; Strip leading/trailing -
         (s (replace-regexp-in-string "\\(^-+\\|-+$\\)" "" s))
         ;; Truncate
         (s (if (> (length s) consult-latex-ref-slug-max-length)
                (substring s 0 consult-latex-ref-slug-max-length)
              s))
         ;; Strip trailing - again after truncation
         (s (replace-regexp-in-string "-+$" "" s)))
    s))

(defun consult-latex-ref--next-sequential (prefix labels)
  "Return the next sequential number for PREFIX among LABELS.
E.g. if eq:1 and eq:2 exist, returns 3."
  (let ((max 0))
    (dolist (lab labels)
      (let ((label-str (car lab)))
        (when (string-prefix-p prefix label-str)
          (let ((rest (substring label-str (length prefix))))
            (when (string-match "^[0-9]+$" rest)
              (setq max (max max (string-to-number rest))))))))
    (1+ max)))

(defun consult-latex-ref--enclosing-environment ()
  "Return the name of the innermost enclosing LaTeX environment, or nil."
  (save-excursion
    (let ((depth 0)
          (found nil))
      (while (and (not found)
                  (re-search-backward
                   "\\\\\\(begin\\|end\\){\\([^}]+\\)}" nil t))
        (if (string= (match-string 1) "end")
            (setq depth (1+ depth))
          (if (= depth 0)
              (setq found (match-string-no-properties 2))
            (setq depth (1- depth)))))
      found)))

(defun consult-latex-ref--enclosing-section ()
  "Return the title of the nearest preceding section command, or nil."
  (save-excursion
    (when (re-search-backward
           "\\\\\\(?:chapter\\|section\\|subsection\\|subsubsection\\|paragraph\\)\\*?{\\([^}]*\\)}"
           nil t)
      (match-string-no-properties 1))))

(defun consult-latex-ref--caption-text ()
  "Return the text of the nearest \\\\caption in the enclosing environment."
  (save-excursion
    ;; Search forward for caption within a reasonable range
    (let ((bound (save-excursion
                   (or (re-search-forward "\\\\end{" nil t) (point-max)))))
      (when (re-search-forward "\\\\caption\\(?:\\[[^]]*\\]\\)?{\\([^}]*\\)}"
                               bound t)
        (match-string-no-properties 1)))))

(defun consult-latex-ref--context-label (labels)
  "Determine a suggested label string based on point context.
LABELS is the current project label list for sequential numbering."
  (let ((env (consult-latex-ref--enclosing-environment)))
    (cond
     ;; Equation-like environment
     ((and env (member env consult-latex-ref-equation-environments))
      (let* ((prefix consult-latex-ref-equation-prefix)
             (n (consult-latex-ref--next-sequential prefix labels)))
        (format "%s%d" prefix n)))
     ;; Figure environment
     ((and env (member env consult-latex-ref-figure-environments))
      (let* ((prefix consult-latex-ref-figure-prefix)
             (caption (consult-latex-ref--caption-text))
             (slug (if caption (consult-latex-ref--slugify caption) "")))
        (if (string-empty-p slug)
            (format "%s%d" prefix
                    (consult-latex-ref--next-sequential prefix labels))
          (format "%s%s" prefix slug))))
     ;; Table environment
     ((and env (member env consult-latex-ref-table-environments))
      (let* ((prefix consult-latex-ref-table-prefix)
             (caption (consult-latex-ref--caption-text))
             (slug (if caption (consult-latex-ref--slugify caption) "")))
        (if (string-empty-p slug)
            (format "%s%d" prefix
                    (consult-latex-ref--next-sequential prefix labels))
          (format "%s%s" prefix slug))))
     ;; Theorem-like environment
     ((and env (member env consult-latex-ref-theorem-environments))
      (let* ((prefix (or (cdr (assoc env consult-latex-ref-theorem-prefix-alist))
                         "thm:"))
             (n (consult-latex-ref--next-sequential prefix labels)))
        (format "%s%d" prefix n)))
     ;; Section context (no enclosing float/math env)
     (t
      (let* ((title (consult-latex-ref--enclosing-section))
             (prefix consult-latex-ref-section-prefix)
             (slug (if title (consult-latex-ref--slugify title) "")))
        (if (string-empty-p slug)
            (format "%s%d" prefix
                    (consult-latex-ref--next-sequential prefix labels))
          (format "%s%s" prefix slug)))))))

;;; Internal: TOC candidate building

(defun consult-latex-ref--toc-candidates (toc-entries)
  "Build consult candidates from TOC-ENTRIES.
Each entry is (title level file-path char-position)."
  (let ((candidates ()))
    (dolist (entry toc-entries)
      (let* ((title (nth 0 entry))
             (level (nth 1 entry))
             (file  (nth 2 entry))
             (pos   (nth 3 entry))
             (indent (make-string (* level 2) ?\s))
             (buf (or (find-buffer-visiting file)
                      (find-file-noselect file t))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (save-excursion
              (goto-char pos)
              (push (consult--location-candidate
                     (concat indent
                             (propertize title
                                         'face 'consult-latex-ref-toc-section-face)
                             "  ")
                     (point-marker)
                     (line-number-at-pos)
                     (point-marker))
                    candidates))))))
    (nreverse candidates)))

;;; Internal: ref key at point

(defun consult-latex-ref--key-at-point ()
  "Return the label key inside a \\\\ref-like command at point, or nil.
Recognises any command in `consult-latex-ref-commands'."
  (save-excursion
    (let ((pt (point))
          (regexp (concat "\\\\\\(?:"
                          (mapconcat #'regexp-quote consult-latex-ref-commands "\\|")
                          "\\){\\([^}]*\\)}")))
      ;; Search forward from beginning of line so we catch all matches on
      ;; the current line and can check if pt falls inside one of them.
      (goto-char (pos-bol))
      (let ((result nil))
        (while (and (not result)
                    (re-search-forward regexp (pos-eol) t))
          (when (and (>= pt (match-beginning 0))
                     (< pt (match-end 0)))
            (setq result (match-string-no-properties 1))))
        result))))

;;; Public commands

;;;###autoload
(defun consult-latex-find-label ()
  "Jump to a \\\\label definition in the project."
  (interactive)
  (push-mark (point) t)
  (when (fboundp 'evil--jumps-push) (evil--jumps-push))
  (let ((marker (consult-latex-ref--find-label nil)))
    (when (markerp marker)
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker))))

;;;###autoload
(defun consult-latex-view-crossref ()
  "Show the \\\\label definition for the ref command at point.
If point is inside a \\\\ref{key}, opens a consult session pre-filtered
to that label with live preview — press RET to jump there, C-g to cancel
and stay in place.  If point is not inside a ref command, falls back to
the full interactive label search."
  (interactive)
  (let ((marker (consult-latex-ref--find-label
                 (consult-latex-ref--key-at-point))))
    (when (markerp marker)
      (push-mark (point) t)
      (when (fboundp 'evil--jumps-push) (evil--jumps-push))
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker))))

;;;###autoload
(defun consult-latex-insert-ref (&optional invert-prompt)
  "Insert a ref command at point.
Selects a label using consult, then inserts
`consult-latex-ref-prefix' followed by the ref command and label.
INVERT-PROMPT inverts the prompt-for-command behaviour."
  (interactive "P")
  (let* ((marker (consult-latex-ref--find-label))
         (label (when (markerp marker)
                  (with-current-buffer (marker-buffer marker)
                    (save-excursion
                      (goto-char marker)
                      (when (looking-at "[^}]+")
                        (match-string-no-properties 0))))))
         (command (if (xor invert-prompt consult-latex-ref-prompt-for-command)
                      (consult-latex-ref--select-command (or label ""))
                    (consult-latex-ref--command-for-label (or label "")))))
    (when label
      (insert consult-latex-ref-prefix)
      (if (fboundp 'TeX-parse-macro)
          (TeX-parse-macro command nil)
        (insert (format "\\%s{}" command))
        (backward-char))
      (insert label)
      (forward-char 1))))

;;;###autoload
(defun consult-latex-create-label ()
  "Create a context-aware \\\\label at point.
Detects the enclosing environment or section, generates a
suggested label string, lets the user confirm or edit it,
then inserts \\\\label{...} and invalidates the cache."
  (interactive)
  (let* ((master (consult-latex-ref--master-file))
         (data (consult-latex-ref--update-cache master))
         (labels (plist-get data :labels))
         (suggestion (consult-latex-ref--context-label labels))
         (label (read-string "Label: " suggestion 'consult-latex-ref-label-history)))
    (unless (string-empty-p label)
      (insert (format "\\label{%s}" label))
      ;; Invalidate cache for current file so the new label is picked up
      (when buffer-file-name
        (consult-latex-ref--invalidate-file buffer-file-name)))))

;;;###autoload
(defun consult-latex-list-orphan-labels ()
  "List \\\\label definitions that are never referenced in the project.
Presents results in a consult buffer for navigation."
  (interactive)
  (let* ((master (consult-latex-ref--master-file))
         (files (consult-latex-ref--collect-files master))
         (data (consult-latex-ref--update-cache master))
         (labels (plist-get data :labels))
         ;; Collect all \ref-like usages across all project files
         (ref-regexp
          (concat "\\\\\\(?:"
                  (mapconcat #'regexp-quote consult-latex-ref-commands "\\|")
                  "\\){\\([^}]+\\)}"))
         (used-labels (make-hash-table :test 'equal)))
    (dolist (file files)
      (let ((content
             (if-let* ((buf (find-buffer-visiting file)))
                 (with-current-buffer buf
                   (buffer-substring-no-properties (point-min) (point-max)))
               (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))))
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          (while (re-search-forward ref-regexp nil t)
            (puthash (match-string-no-properties 1) t used-labels)))))
    ;; Find labels not in used-labels
    (let ((orphans (seq-filter
                    (lambda (lab)
                      (not (gethash (car lab) used-labels)))
                    labels)))
      (if (null orphans)
          (message "consult-latex-ref: no orphaned labels found")
        (let ((candidates (consult-latex-ref--make-orphan-candidates orphans)))
          (let ((marker (consult--read
                         candidates
                         :prompt "Orphaned labels: "
                         :annotate #'consult-latex-ref--annotate
                         :category 'consult-location
                         :sort nil
                         :require-match t
                         :lookup #'consult--lookup-location
                         :history '(:input consult--line-history)
                         :state (consult--jump-preview))))
            (when (markerp marker)
              (switch-to-buffer (marker-buffer marker))
              (goto-char marker))))))))

(defun consult-latex-ref--make-orphan-candidates (labels)
  "Build consult candidates for orphaned LABELS using orphan face."
  (let ((candidates ()))
    (dolist (lab labels)
      (let* ((label-str (car lab))
             (file (cadr lab))
             (pos (cddr lab))
             (buf (or (find-buffer-visiting file)
                      (find-file-noselect file t))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (save-excursion
              (goto-char pos)
              (push (consult--location-candidate
                     (concat (propertize label-str
                                         'face 'consult-latex-ref-orphan-face)
                             "  ")
                     (point-marker)
                     (line-number-at-pos)
                     (point-marker))
                    candidates))))))
    (nreverse candidates)))

;;;###autoload
(defun consult-latex-toc ()
  "Browse the table of contents of the current LaTeX project.
Presents all sections across all project files via consult,
with live preview."
  (interactive)
  (let* ((master (consult-latex-ref--master-file))
         (data (consult-latex-ref--update-cache master))
         (toc (plist-get data :toc))
         (candidates (consult-latex-ref--toc-candidates toc)))
    (if (null candidates)
        (message "consult-latex-ref: no sections found in project")
      (let ((marker (consult--read
                     candidates
                     :prompt "TOC: "
                     :annotate #'consult-latex-ref--toc-annotate
                     :category 'consult-location
                     :sort nil
                     :require-match t
                     :lookup #'consult--lookup-location
                     :history '(:input consult--line-history)
                     :state (consult--jump-preview))))
        (when (markerp marker)
          (switch-to-buffer (marker-buffer marker))
          (goto-char marker))))))

(provide 'consult-latex-ref)
;;; consult-latex-ref.el ends here
