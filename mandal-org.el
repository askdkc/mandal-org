;;; mandal-org.el --- Text mandala view for Org headings -*- lexical-binding: t; -*-

;; Author: askdkc
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: outlines, org, notes

;;; Commentary:

;; mandal-org displays one Org file as a hierarchical 3x3 mandala.
;; The current heading is shown in the center cell, and its direct
;; children with MANDALA_POS properties are shown around it.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'seq)
(require 'subr-x)

(defgroup mandal-org nil
  "Text mandala view for Org headings."
  :group 'org)

(defcustom mandal-org-cell-preview-lines 4
  "Maximum number of body preview lines shown in one mandala cell.
Each cell also shows a label line and a title line above these."
  :type 'integer
  :group 'mandal-org)

(defcustom mandal-org-cell-preview-width 18
  "Maximum display width of one preview line."
  :type 'integer
  :group 'mandal-org)

(defcustom mandal-org-cell-width 'fit
  "Width of one mandala cell, in display columns.
- an integer fixes the width;
- `auto' derives a near-square width from the cell's row count;
- `fit' (default) sizes the whole grid to the current window using
  `window-width'/`window-height', keeping cells near square.

`fit' reads the window size once per redraw (no resize hook, no
feedback loop): the grid refits on open, `g', and navigation, but a
live frame resize only takes effect on the next redraw."
  :type '(choice (const :tag "Fit to window" fit)
                 (const :tag "Auto (near square)" auto)
                 integer)
  :group 'mandal-org)

(defcustom mandal-org-cell-aspect 1.05
  "Target cell height/width ratio used when `mandal-org-cell-width' is `auto'.
A value just above 1 makes the cell a little taller than wide; 1.0 is
square; larger is taller and narrower."
  :type 'number
  :group 'mandal-org)

(defcustom mandal-org-frame-style 'xpm
  "How to draw the mandala grid frame.

`xpm' (default) draws the grid lines as char-cell-sized XPM tiles
(goban style), for crisp lines that do not depend on the box-drawing
font.  Used only on graphical displays; terminals use `text'.

`text' draws the lines with the `│' character and a strike-through
rule (no image).

Both styles pin columns with `:align-to', so the grid stays aligned
with wide CJK text, and cell content is always plain buffer text.  A
translucent frame (e.g. `(set-frame-parameter nil \\='alpha 80)')
shows the desktop through BOTH styles equally — that is the frame
setting, not a rendering bug."
  :type '(choice (const :tag "XPM goban-style tiles" xpm)
                 (const :tag "Plain text lines" text))
  :group 'mandal-org)

(defcustom mandal-org-edit-popup-height 0.4
  "Height of the cell edit popup window.
A float is a fraction of the frame height; an integer is a number
of lines.  See `split-window' / `display-buffer' height specs."
  :type 'number
  :group 'mandal-org)

(defcustom mandal-org-directory "~/mandala/"
  "Directory where new mandala Org files are created and searched.
New files are named with a Denote-like identifier, for example
\"20240115T143022--central-keyword.org\"."
  :type 'directory
  :group 'mandal-org)

(defconst mandal-org--positions '(nw n ne w e sw s se))
(defconst mandal-org--grid '((nw n ne) (w center e) (sw s se)))

(defface mandal-org-label
  '((t :inherit shadow))
  "Face for a cell's position label (nw, n, …)."
  :group 'mandal-org)

(defface mandal-org-title
  '((t :inherit bold :height 1.2))
  "Face for a cell's heading title (one step larger than body text).
The title sits on the same row in every cell, so a uniform height
keeps the grid rows aligned."
  :group 'mandal-org)

(defface mandal-org-h1
  '((t :inherit bold :height 1.15))
  "Face for a `*' heading line inside a cell body (slightly larger bold).
A height other than 1.0 makes that line taller, which can nudge the
grid out of alignment; set the height to 1.0 to avoid that."
  :group 'mandal-org)

(defface mandal-org-h2
  '((t :inherit bold))
  "Face for a `**' heading line inside a cell body (bold, body size)."
  :group 'mandal-org)

(defface mandal-org-body
  '((t :inherit default))
  "Face for a cell's body preview text."
  :group 'mandal-org)

(defface mandal-org-empty
  '((t :inherit shadow :height 1.2))
  "Face for an empty (creatable) cell placeholder.
Matches `mandal-org-title' height so the title row stays the same
height in empty and filled cells."
  :group 'mandal-org)

(defface mandal-org-center
  '((t :inherit highlight :extend t))
  "Background face for the center cell."
  :group 'mandal-org)

(defface mandal-org-selected
  '((t :inherit region :extend t))
  "Background face for the currently selected cell."
  :group 'mandal-org)

(defface mandal-org-border
  '((((background dark)) :foreground "gray60")
    (((background light)) :foreground "gray40")
    (t :inherit shadow))
  "Face for the grid border lines."
  :group 'mandal-org)

(defface mandal-org-rule
  '((((background dark)) :foreground "gray60" :strike-through "gray60")
    (((background light)) :foreground "gray40" :strike-through "gray40")
    (t :inherit shadow :strike-through t))
  "Face for the horizontal grid rules (drawn as a strike-through space)."
  :group 'mandal-org)


(defvar-local mandal-org-source-buffer nil)
(defvar-local mandal-org-source-file nil)
(defvar-local mandal-org-current-marker nil)
(defvar-local mandal-org-current-position nil)
(defvar-local mandal-org-selected-position 'center)
(defvar-local mandal-org-selected-marker nil)
(defvar-local mandal-org-edit-return-marker nil)
(defvar-local mandal-org-edit-return-position nil)
(defvar-local mandal-org--cell-width nil
  "Buffer-local override of `mandal-org-cell-width' for zooming.")

(defvar-local mandal-org--last-view-buffer nil)
(defvar-local mandal-org--last-view-current-marker nil)
(defvar-local mandal-org--last-view-selected-position 'center)
(defvar-local mandal-org--last-edit-return-marker nil)

(defun mandal-org--package-root ()
  "Return the directory that contains `mandal-org.el'."
  (file-name-directory
   (or load-file-name
       (locate-library "mandal-org.el")
       (buffer-file-name))))

(defun mandal-org--tutorial-file ()
  "Return the tutorial Org file path."
  (expand-file-name "examples/mandala-example.org"
                    (mandal-org--package-root)))

(defun mandal-org--marker-at-point ()
  "Return a marker at point in the current buffer."
  (let ((marker (make-marker)))
    (set-marker marker (point) (current-buffer))
    marker))

(defun mandal-org--copy-marker (marker)
  "Return a copy of MARKER."
  (let ((copy (make-marker)))
    (set-marker copy (marker-position marker) (marker-buffer marker))
    copy))

(defun mandal-org--marker-position (marker)
  "Return MARKER's position, or nil."
  (and (markerp marker)
       (marker-position marker)))

(defun mandal-org--source-file-name (buffer)
  "Return BUFFER's file name, or nil."
  (and (buffer-live-p buffer)
       (buffer-file-name buffer)))

(defun mandal-org--marker-from-position (buffer position)
  "Return a heading marker in BUFFER at POSITION.

If POSITION is unavailable or no longer points into BUFFER, use the
first Org heading."
  (with-current-buffer buffer
    (save-excursion
      (if (and (integerp position)
               (<= (point-min) position)
               (<= position (point-max)))
          (goto-char position)
        (goto-char (point-min)))
      (if (org-before-first-heading-p)
          (mandal-org--first-heading-marker)
        (org-back-to-heading t)
        (mandal-org--marker-at-point)))))

(defun mandal-org--ensure-source-buffer ()
  "Return a live source Org buffer for the current mandala view.

If the original source buffer was killed but the view still knows
the file name, reopen the file and restore the buffer reference."
  (cond
   ((buffer-live-p mandal-org-source-buffer)
    mandal-org-source-buffer)
   ((and mandal-org-source-file
         (file-readable-p mandal-org-source-file))
    (setq mandal-org-source-buffer
          (find-file-noselect mandal-org-source-file))
    (with-current-buffer mandal-org-source-buffer
      (org-mode))
    mandal-org-source-buffer)
   (t
    (user-error "Source Org buffer is no longer live and cannot be reopened"))))

(defun mandal-org--ensure-view-state ()
  "Ensure the current view has a live source buffer and markers."
  (let ((source (mandal-org--ensure-source-buffer)))
    (unless (and (markerp mandal-org-current-marker)
                 (marker-buffer mandal-org-current-marker))
      (setq mandal-org-current-marker
            (mandal-org--marker-from-position source mandal-org-current-position)))
    (when (and mandal-org-edit-return-position
               (not (and (markerp mandal-org-edit-return-marker)
                         (marker-buffer mandal-org-edit-return-marker))))
      (setq mandal-org-edit-return-marker
            (mandal-org--marker-from-position source
                                               mandal-org-edit-return-position)))
    source))

(defun mandal-org--valid-position-p (position)
  "Return non-nil when POSITION is a valid mandala position symbol."
  (memq position mandal-org--positions))

(defun mandal-org--position-string (position)
  "Return POSITION as an Org property string."
  (symbol-name position))

(defun mandal-org--property-position ()
  "Return MANDALA_POS at point as a symbol, or nil if absent."
  (let ((value (org-entry-get (point) "MANDALA_POS")))
    (when (and value (not (string-empty-p value)))
      (intern value))))

(defun mandal-org--front-matter-end (heading-start subtree-end)
  "Return end of HEADING-START's own lines, before its first sub-heading."
  (save-excursion
    (goto-char heading-start)
    (forward-line 1)
    (if (re-search-forward org-heading-regexp subtree-end t)
        (match-beginning 0)
      subtree-end)))

(defun mandal-org--subtree-body-end (level subtree-end)
  "Return the end of LEVEL heading's editable body before SUBTREE-END.

The body runs until the first direct child heading that is an actual
mandala cell — a direct child (level LEVEL+1) carrying a MANDALA_POS
property (well-formed OR in a misplaced drawer, detected like the grid
via `mandal-org--cell-position').  Headings WITHOUT MANDALA_POS (at
any depth) are treated as ordinary cell content, so you can use Org
structure (`**' headings, lists, etc.) inside a cell's body without it
being read as mandala structure or truncating the cell."
  (save-excursion
    (forward-line 1)
    (let ((found nil))
      (while (and (not found)
                  (re-search-forward org-heading-regexp subtree-end t))
        (let ((heading-start (match-beginning 0)))
          (goto-char heading-start)
          (when (= (org-outline-level) (1+ level))
            (let* ((child-end (save-excursion (org-end-of-subtree t t)))
                   (front-end (mandal-org--front-matter-end heading-start
                                                             child-end)))
              (when (save-excursion
                      (goto-char heading-start)
                      (mandal-org--cell-position front-end))
                (setq found heading-start))))
          (forward-line 1)))
      (or found subtree-end))))

(defun mandal-org--fallback-property-position (body-end)
  "Return a misplaced MANDALA_POS before BODY-END, or nil.

This handles cells edited before the property drawer, where the
drawer is no longer recognized by Org because body text was placed
between the heading and :PROPERTIES:."
  (save-excursion
    (forward-line 1)
    (when (re-search-forward
           "^[ \t]*:MANDALA_POS:[ \t]*\\([^ \t\n]+\\)[ \t]*$"
           body-end t)
      (intern (match-string-no-properties 1)))))

(defun mandal-org--cell-position (body-end)
  "Return the mandala position at point, checking malformed drawers to BODY-END."
  (or (mandal-org--property-position)
      (mandal-org--fallback-property-position body-end)))

(defun mandal-org--scan-property (beg end key)
  "Return the value of the first \":KEY: value\" line in [BEG, END), else nil."
  (save-excursion
    (goto-char beg)
    (when (re-search-forward
           (format "^[ \t]*:%s:[ \t]*\\(.*?\\)[ \t]*$" (regexp-quote key))
           end t)
      (let ((v (match-string-no-properties 1)))
        (unless (string-empty-p v) v)))))

(defun mandal-org--repair-heading ()
  "Move a misplaced MANDALA_POS drawer for the heading at point.
Point must be on the heading line.  If the heading has a MANDALA_POS
that Org does not see as a proper property drawer (because body text
precedes it), relocate the drawer — or rebuild it from loose lines —
to immediately after the heading.  Return non-nil when repaired."
  (let* ((hstart (line-beginning-position))
         (subtree-end (save-excursion (org-end-of-subtree t t)))
         (front-end (mandal-org--front-matter-end hstart subtree-end)))
    (when (and (mandal-org--cell-position front-end)
               (not (org-entry-get (point) "MANDALA_POS")))
      (let ((fmark (copy-marker front-end))
            (smark (copy-marker subtree-end))
            (insert-at (copy-marker
                        (save-excursion
                          (goto-char hstart)
                          (forward-line 1)
                          (when (and (looking-at-p org-planning-line-re)
                                     (< (point) front-end))
                            (forward-line 1))
                          (point)))))
        (save-excursion
          (goto-char hstart)
          (forward-line 1)
          (if (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" fmark t)
              ;; Relocate the whole misplaced drawer block, preserving it.
              (let ((dbeg (match-beginning 0)))
                (when (re-search-forward "^[ \t]*:END:[ \t]*$" smark t)
                  (let* ((dend (min (point-max) (1+ (line-end-position))))
                         (block (concat (string-trim-right
                                         (buffer-substring-no-properties dbeg dend)
                                         "[ \t\n]+")
                                        "\n")))
                    (delete-region dbeg dend)
                    (goto-char insert-at)
                    (insert block))))
            ;; No drawer block: rebuild from loose ID/MANDALA_POS lines.
            (let ((pos (mandal-org--cell-position fmark))
                  (id (mandal-org--scan-property hstart fmark "ID")))
              (goto-char hstart)
              (forward-line 1)
              (while (re-search-forward
                      "^[ \t]*:\\(?:ID\\|MANDALA_POS\\):.*\n" fmark t)
                (replace-match ""))
              (goto-char hstart)
              (org-entry-put (point) "MANDALA_POS"
                             (mandal-org--position-string pos))
              (when id (org-entry-put (point) "ID" id)))))
        (set-marker fmark nil)
        (set-marker smark nil)
        (set-marker insert-at nil)
        t))))

(defun mandal-org--heading-title (marker)
  "Return the Org heading title at MARKER."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (org-get-heading t t t t))))

(defun mandal-org--first-heading-marker ()
  "Return a marker for the first heading in the current Org buffer."
  (save-excursion
    (goto-char (point-min))
    (unless (org-next-visible-heading 1)
      (user-error "No Org heading found"))
    (mandal-org--marker-at-point)))

(defun mandal-org--direct-children-by-position (marker)
  "Return direct children of MARKER keyed by MANDALA_POS.

Only direct children are considered.  Invalid or duplicate
MANDALA_POS values signal `user-error'."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (org-back-to-heading t)
      (let* ((parent-level (org-outline-level))
             (end (save-excursion (org-end-of-subtree t t)))
             (children nil))
        (forward-line 1)
        (while (re-search-forward org-heading-regexp end t)
          (goto-char (match-beginning 0))
          (let ((level (org-outline-level)))
            (cond
             ((<= level parent-level)
              (goto-char end))
             ((= level (1+ parent-level))
              (let* ((child-end (save-excursion (org-end-of-subtree t t)))
                     (body-end (save-excursion
                                 (mandal-org--subtree-body-end level child-end)))
                     (pos (mandal-org--cell-position body-end)))
                (when pos
                  (unless (mandal-org--valid-position-p pos)
                    (user-error "Invalid MANDALA_POS: %s" pos))
                  (when (alist-get pos children)
                    (user-error "Duplicate MANDALA_POS under %s: %s"
                                (mandal-org--heading-title marker) pos))
                  (push (cons pos (mandal-org--marker-at-point)) children))))))
          (forward-line 1))
        children))))

(defun mandal-org--goto-parent (marker)
  "Return a marker for MARKER's parent heading, or nil at top level."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (org-back-to-heading t)
      (when (org-up-heading-safe)
        (mandal-org--marker-at-point)))))

(defun mandal-org--insert-property-drawer (id position)
  "Insert a property drawer for ID and POSITION at point."
  (insert ":PROPERTIES:\n")
  (insert (format ":ID:          %s\n" id))
  (when position
    (insert (format ":MANDALA_POS: %s\n" (mandal-org--position-string position))))
  (insert ":END:\n\n"))

(defun mandal-org--create-cell (parent-marker position)
  "Create a child heading below PARENT-MARKER at POSITION.

Return a marker pointing to the new heading.  The new title is
initially empty, and point is left where the title can be typed."
  (unless (mandal-org--valid-position-p position)
    (user-error "Invalid mandala position: %s" position))
  (with-current-buffer (marker-buffer parent-marker)
    (save-excursion
      (goto-char parent-marker)
      (org-back-to-heading t)
      (let* ((parent-level (org-outline-level))
             (children (mandal-org--direct-children-by-position parent-marker)))
        (when (alist-get position children)
          (user-error "Cell already exists at %s" position))
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (unless (or (bobp) (looking-back "\n\n" nil)) (insert "\n"))
        (let ((heading-start (point)))
          (insert (make-string (1+ parent-level) ?*) " \n")
          (mandal-org--insert-property-drawer (org-id-new) position)
          (copy-marker heading-start))))))

(defun mandal-org--drawer-line-p (line)
  "Return non-nil if LINE opens or closes an Org drawer."
  (string-match-p "\\`[ \t]*:\\([[:alnum:]_@#%]+\\|END\\):[ \t]*\\'" line))

(defun mandal-org--source-block-line-p (line)
  "Return non-nil if LINE starts or ends an Org source/example block."
  (string-match-p "\\`[ \t]*#\\+\\(begin\\|end\\)_\\(src\\|example\\)\\b" (downcase line)))

(defun mandal-org--body-lines (marker)
  "Return direct body lines for the heading at MARKER."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (org-back-to-heading t)
      (let* ((level (org-outline-level))
             (subtree-end (save-excursion (org-end-of-subtree t t)))
             (body-start (save-excursion
                           (forward-line 1)
                           (point)))
             (body-end (save-excursion
                         (mandal-org--subtree-body-end level subtree-end)))
             (raw (buffer-substring-no-properties body-start body-end))
             (lines (split-string raw "\n"))
             (in-drawer nil)
             (in-block nil)
             kept)
        (dolist (line lines)
          (cond
           ((and in-drawer (string-match-p "\\`[ \t]*:END:[ \t]*\\'" line))
            (setq in-drawer nil))
           (in-drawer)
           ((and in-block (mandal-org--source-block-line-p line))
            (setq in-block nil))
           (in-block)
           ((mandal-org--drawer-line-p line)
            (setq in-drawer t))
           ((mandal-org--source-block-line-p line)
            (setq in-block t))
           ;; Org heading lines inside the body: style by level, strip stars,
           ;; and hide level 3+ (`***' and deeper).
           ((string-match "\\`\\(\\*+\\)[ \t]+\\(.*\\)\\'" line)
            (let ((level (length (match-string 1 line)))
                  (text (string-trim (match-string 2 line))))
              (cond
               ((>= level 3))           ; hidden
               ((= level 1)
                (push (propertize text 'face 'mandal-org-h1) kept))
               (t
                (push (propertize text 'face 'mandal-org-h2) kept)))))
           ((string-blank-p line))
           (t (push (string-trim line) kept))))
        (nreverse kept)))))

(defun mandal-org--wrap-line (line width)
  "Wrap LINE by display WIDTH using `string-width'."
  (let ((rest line)
        lines)
    (while (> (string-width rest) width)
      (let ((part (truncate-string-to-width rest width nil nil "")))
        (when (string-empty-p part)
          (setq part (substring rest 0 1)))
        (push part lines)
        (setq rest (string-trim-left
                    (substring rest (length part))))))
    (unless (string-empty-p rest)
      (push rest lines))
    (nreverse lines)))

(defun mandal-org--fit-lines (lines width max-lines)
  "Fit LINES to WIDTH and MAX-LINES, adding ellipsis when truncated."
  (let ((wrapped nil))
    (dolist (line lines)
      (setq wrapped (append wrapped (mandal-org--wrap-line line width))))
    (let ((truncated (> (length wrapped) max-lines))
          (result (seq-take wrapped max-lines)))
      (when (and truncated result)
        (let* ((last (car (last result)))
               (room (max 0 (1- width)))
               (short (truncate-string-to-width last room nil nil "")))
          (setq result (append (butlast result) (list (concat short "…"))))))
      result)))

(defun mandal-org--clamp (value min-value max-value)
  "Clamp VALUE between MIN-VALUE and MAX-VALUE."
  (min max-value (max min-value value)))

(defun mandal-org--cell-preview (marker)
  "Return display preview lines for the heading at MARKER."
  (let* ((title (mandal-org--heading-title marker))
         (body (mandal-org--body-lines marker)))
    (mandal-org--fit-lines
     (cons (if (string-empty-p title) "(untitled)" title) body)
     mandal-org-cell-preview-width
     mandal-org-cell-preview-lines)))

(defun mandal-org--cell-preview-parts-for-size (marker width max-lines)
  "Return title/body preview parts for MARKER fitted to WIDTH and MAX-LINES.

Each returned element is (KIND . TEXT), where KIND is `title' or
`body'."
  (let* ((title (mandal-org--heading-title marker))
         (title-lines (mandal-org--fit-lines
                       (list (if (string-empty-p title) "(untitled)" title))
                       width
                       2))
         (body-lines (mandal-org--fit-lines
                      (mandal-org--body-lines marker)
                      width
                      (max 0 (- max-lines (length title-lines)))))
         parts)
    (dolist (line title-lines)
      (push (cons 'title line) parts))
    (dolist (line body-lines)
      (push (cons 'body line) parts))
    (nreverse parts)))

(defun mandal-org--selected-marker ()
  "Return marker for the selected cell in the current view buffer."
  (if (eq mandal-org-selected-position 'center)
      mandal-org-current-marker
    (alist-get mandal-org-selected-position
               (mandal-org--direct-children-by-position mandal-org-current-marker))))

(defun mandal-org--cell-lines (position children width lines)
  "Return preview lines for POSITION using CHILDREN alist."
  (cond
   ((eq position 'center)
    (mandal-org--cell-preview-parts-for-size mandal-org-current-marker width lines))
   ((alist-get position children)
    (mandal-org--cell-preview-parts-for-size (alist-get position children) width lines))
   (t (list (cons 'empty "+")))))

(defun mandal-org--char-ratio ()
  "Return the canonical character height/width ratio (≈2.0 in a terminal).
Uses `frame-char-height'/`frame-char-width' (the same units as
`window-width' and `:align-to') so layout math stays consistent."
  (if (display-graphic-p)
      (/ (float (frame-char-height)) (max 1 (frame-char-width)))
    2.0))

(defun mandal-org--square-width (rows)
  "Return the near-square cell width in columns for ROWS text rows."
  (max 8 (round (/ (* rows (mandal-org--char-ratio))
                   (max 0.1 mandal-org-cell-aspect)))))

(defun mandal-org--fit-dims ()
  "Return (CELL-WIDTH . PREVIEW-LINES) fitted to the current window.
Reads `window-width'/`window-height' once per call (no resize hook),
so it cannot feed back into a loop."
  (let* ((win (get-buffer-window (current-buffer)))
         (ww (window-width win))
         (wh (window-height win))
         ;; Grid width in columns is 3*cellw + 4 separators.  Leave a couple
         ;; of columns of margin so the line never reaches the window edge
         ;; (which would clip or show a truncation glyph).
         (max-cellw (max 8 (/ (- ww 7) 3)))
         ;; Fixed vertical overhead: title (2) + all grid rules (4) + blank
         ;; before the footer (1) + safety (2).
         (fixed 9)
         ;; Pass 1: estimate rows assuming the worst-case (narrowest) cell so
         ;; the footer's wrapped line count is not under-reserved.
         (est-rows (max 2 (/ (- wh (+ fixed 4)) 3)))
         (est-cellw (min max-cellw (mandal-org--square-width est-rows)))
         ;; Help wraps to the actual grid width; the narrower est-cellw gives
         ;; an upper bound on the line count, so we never reserve too little.
         (help-n (length (mandal-org--help-lines (+ (* 3 est-cellw) 4))))
         (rows-per (max 2 (/ (- wh (+ fixed help-n)) 3)))
         (square (mandal-org--square-width rows-per)))
    (cons (min max-cellw square) (max 1 (1- rows-per)))))

(defun mandal-org--effective-cell-width ()
  "Return the current cell width in columns.
A zoom override (`mandal-org--cell-width') wins; otherwise follow
`mandal-org-cell-width': an integer as-is, `fit' to the window, or
`auto' for a near-square width from `mandal-org-cell-preview-lines'."
  (cond
   (mandal-org--cell-width mandal-org--cell-width)
   ((integerp mandal-org-cell-width) (max 8 mandal-org-cell-width))
   ((eq mandal-org-cell-width 'fit) (car (mandal-org--fit-dims)))
   (t (mandal-org--square-width (1+ mandal-org-cell-preview-lines)))))

(defun mandal-org--effective-preview-lines ()
  "Return the number of body preview lines per cell.
With `fit' width (and no zoom override) the row count is derived from
the window height; otherwise use `mandal-org-cell-preview-lines'."
  (if (and (eq mandal-org-cell-width 'fit)
           (not mandal-org--cell-width))
      (cdr (mandal-org--fit-dims))
    mandal-org-cell-preview-lines))

(defun mandal-org--cell-rows (pos children cols text-lines)
  "Return content rows for cell POS, truncated to COLS display columns.
The list has 1 label row plus TEXT-LINES content rows, with faces
applied but no padding or background (the grid layer adds those via
`:align-to', so wide CJK glyphs cannot shift the column borders)."
  (let* ((selected (eq pos mandal-org-selected-position))
         (center (eq pos 'center))
         (parts (mandal-org--cell-lines pos children (1- cols) text-lines))
         (label (format "%s%s"
                        (if selected "▸ " " ")
                        (if center "● center" (symbol-name pos))))
         (rows (cons (propertize label 'face 'mandal-org-label)
                     (mapcar (lambda (part)
                               (pcase (car part)
                                 ('title (propertize (concat " " (cdr part))
                                                     'face 'mandal-org-title))
                                 ('empty (propertize (concat " " (cdr part))
                                                     'face 'mandal-org-empty))
                                 ;; Body: add the body face as a low-priority
                                 ;; base so any per-line heading face baked in
                                 ;; by `mandal-org--body-lines' still wins.
                                 (_ (let ((s (concat " " (cdr part))))
                                      (add-face-text-property
                                       0 (length s) 'mandal-org-body t s)
                                      s))))
                             parts))))
    (while (< (length rows) (1+ text-lines))
      (setq rows (append rows (list ""))))
    (mapcar (lambda (r) (truncate-string-to-width r cols nil nil "…")) rows)))

(defun mandal-org--grid-line (contents faces cols)
  "Assemble one text row of the grid from CONTENTS using `:align-to'.
CONTENTS is a list of three column strings; FACES a list of three
cell background faces (or nil).  Each column boundary is pinned to a
fixed canonical column, so the vertical separators line up no matter
how wide the (possibly CJK) cell text renders."
  (let ((vbar (propertize "│" 'face 'mandal-org-border))
        (s ""))
    (dotimes (k 3)
      (let* ((nx (* (1+ k) (1+ cols)))
             (bg (nth k faces))
             (txt (or (nth k contents) "")))
        (when (and bg (> (length txt) 0))
          (setq txt (copy-sequence txt))
          (add-face-text-property 0 (length txt) bg t txt))
        (setq s (concat s vbar txt
                        (propertize " " 'display `(space :align-to ,nx)
                                    'face (or bg 'default))))))
    (concat s vbar "\n")))

(defun mandal-org--rule-row (cols)
  "Return a horizontal grid rule row, pinned to COLS with `:align-to'."
  (let ((vbar (propertize "│" 'face 'mandal-org-border))
        (s ""))
    (dotimes (k 3)
      (let ((nx (* (1+ k) (1+ cols))))
        (setq s (concat s vbar
                        (propertize " " 'display `(space :align-to ,nx)
                                    'face 'mandal-org-rule)))))
    (concat s vbar "\n")))

(defconst mandal-org--help-units
  '("←h ↓j ↑k →l ↖u ↗i ↙n ↘m move"
    "c center" "C-u parent" "e edit"
    "C-n/M-n cycle" "C-c m +/- zoom" "g refresh" "q quit")
  "Footer hint groups, wrapped to fit the grid width.")

(defun mandal-org--help-lines (width)
  "Return the footer hint groups wrapped to WIDTH columns, as a list."
  (let ((sep "   ") (lines nil) (cur ""))
    (dolist (u mandal-org--help-units)
      (cond ((string-empty-p cur) (setq cur u))
            ((<= (+ (string-width cur) (string-width sep) (string-width u)) width)
             (setq cur (concat cur sep u)))
            (t (push cur lines) (setq cur u))))
    (unless (string-empty-p cur) (push cur lines))
    (nreverse lines)))

(defun mandal-org--help-text (width)
  "Return the footer hint wrapped to WIDTH display columns."
  (concat "\n"
          (propertize (mapconcat #'identity (mandal-org--help-lines width) "\n")
                      'face 'mandal-org-label)
          "\n"))

(defun mandal-org--render-grid-text ()
  "Return the view as a propertized text mandala (no image).
Columns are pinned with `:align-to', so the grid never breaks even
with wide CJK glyphs or odd box-drawing font metrics."
  (let* ((cols (mandal-org--effective-cell-width))
         (text-lines (mandal-org--effective-preview-lines))
         (children (mandal-org--direct-children-by-position
                    mandal-org-current-marker))
         (out nil))
    (push (propertize
           (format "Org Mandala: %s\n\n"
                   (mandal-org--heading-title mandal-org-current-marker))
           'face 'mandal-org-title)
          out)
    (push (mandal-org--rule-row cols) out)
    (cl-loop for grid-row in mandal-org--grid do
             (let ((cells (mapcar (lambda (pos)
                                    (mandal-org--cell-rows pos children cols text-lines))
                                  grid-row))
                   (faces (mapcar (lambda (pos)
                                    (cond ((eq pos mandal-org-selected-position)
                                           'mandal-org-selected)
                                          ((eq pos 'center) 'mandal-org-center)))
                                  grid-row)))
               (dotimes (line (1+ text-lines))
                 (push (mandal-org--grid-line
                        (mapcar (lambda (c) (nth line c)) cells) faces cols)
                       out))
               (push (mandal-org--rule-row cols) out)))
    (push (mandal-org--help-text (+ (* 3 cols) 4)) out)
    (apply #'concat (nreverse out))))

(defcustom mandal-org-line-thickness 0
  "Grid line thickness in pixels for the XPM frame.
0 means auto (about 1/9 of the line height, min 2px)."
  :type 'integer
  :group 'mandal-org)

(defun mandal-org--tile-xpm (w h dirs color)
  "Return a WxH XPM image tile drawing line segments toward DIRS in COLOR.
DIRS is a list of `up' `down' `left' `right'.  Lines are
`mandal-org-line-thickness' px thick and cross at the tile centre,
so adjacent tiles connect into a continuous board grid."
  (let* ((th (if (> mandal-org-line-thickness 0)
                 mandal-org-line-thickness
               (max 2 (round (/ h 9.0)))))
         (cy (/ h 2)) (cx (/ w 2))
         (r0 (max 0 (- cy (/ th 2)))) (r1 (min (1- h) (+ r0 (1- th))))
         (c0 (max 0 (- cx (/ th 2)))) (c1 (min (1- w) (+ c0 (1- th))))
         (grid (cl-loop repeat h collect (make-string w ?\s))))
    (cl-flet ((seti (r c) (when (and (>= r 0) (< r h) (>= c 0) (< c w))
                            (aset (nth r grid) c ?x))))
      ;; Horizontal segments fill the band rows R0..R1; vertical segments fill
      ;; the band columns C0..C1, meeting in a solid centre block.
      (when (memq 'left dirs)
        (cl-loop for r from r0 to r1 do (cl-loop for c from 0 to c1 do (seti r c))))
      (when (memq 'right dirs)
        (cl-loop for r from r0 to r1 do (cl-loop for c from c0 below w do (seti r c))))
      (when (memq 'up dirs)
        (cl-loop for c from c0 to c1 do (cl-loop for r from 0 to r1 do (seti r c))))
      (when (memq 'down dirs)
        (cl-loop for c from c0 to c1 do (cl-loop for r from r0 below h do (seti r c)))))
    (create-image
     (concat "/* XPM */\nstatic char *t[]={\n"
             (format "\"%d %d 2 1\",\n\"x c %s\",\n\"  c None\",\n" w h color)
             (mapconcat (lambda (row) (format "\"%s\"" row)) grid ",\n")
             "};")
     'xpm t :ascent 'center)))

(defun mandal-org--render-grid-xpm ()
  "Return the view with a goban-style XPM tile frame (graphical only).
Cell text is laid out with `:align-to' exactly as in the text
renderer, but the grid lines are char-cell-sized XPM tiles so they
render as a clean board grid independent of the box-drawing font."
  (let* ((cols (mandal-org--effective-cell-width))
         (text-lines (mandal-org--effective-preview-lines))
         (children (mandal-org--direct-children-by-position
                    mandal-org-current-marker))
         ;; Use canonical char metrics so one tile == one `:align-to' column.
         (fw (frame-char-width))
         (lh (frame-char-height))
         (color (or (face-foreground 'mandal-org-border nil t) "#888888"))
         (segpx (* cols fw))
         (v  (mandal-org--tile-xpm fw lh '(up down) color))
         (hr (mandal-org--tile-xpm segpx lh '(left right) color))
         (tl (mandal-org--tile-xpm fw lh '(down right) color))
         (tr (mandal-org--tile-xpm fw lh '(down left) color))
         (bl (mandal-org--tile-xpm fw lh '(up right) color))
         (br (mandal-org--tile-xpm fw lh '(up left) color))
         (td (mandal-org--tile-xpm fw lh '(down left right) color))   ; ┬
         (bu (mandal-org--tile-xpm fw lh '(up left right) color))     ; ┴
         (lt (mandal-org--tile-xpm fw lh '(up down right) color))     ; ├
         (rt (mandal-org--tile-xpm fw lh '(up down left) color))      ; ┤
         (cx (mandal-org--tile-xpm fw lh '(up down left right) color)); ┼
         (out nil))
    (cl-flet ((im (i) (propertize " " 'display i))
              (rule (left mid right)
                (concat (propertize " " 'display left)
                        (propertize " " 'display hr) (propertize " " 'display mid)
                        (propertize " " 'display hr) (propertize " " 'display mid)
                        (propertize " " 'display hr) (propertize " " 'display right)
                        "\n")))
      (push (propertize (format "Org Mandala: %s\n\n"
                                (mandal-org--heading-title mandal-org-current-marker))
                        'face 'mandal-org-title)
            out)
      (push (rule tl td tr) out)
      (cl-loop for grid-row in mandal-org--grid
               for ri from 0 do
               (let ((cells (mapcar (lambda (pos)
                                      (mandal-org--cell-rows pos children cols text-lines))
                                    grid-row))
                     (faces (mapcar (lambda (pos)
                                      (cond ((eq pos mandal-org-selected-position)
                                             'mandal-org-selected)
                                            ((eq pos 'center) 'mandal-org-center)))
                                    grid-row)))
                 (dotimes (line (1+ text-lines))
                   (let ((s "") (lc (mapcar (lambda (c) (nth line c)) cells)))
                     (dotimes (k 3)
                       (let* ((nx (* (1+ k) (1+ cols)))
                              (bg (nth k faces))
                              (txt (or (nth k lc) "")))
                         (when (and bg (> (length txt) 0))
                           (setq txt (copy-sequence txt))
                           (add-face-text-property 0 (length txt) bg t txt))
                         (setq s (concat s (im v) txt
                                         (propertize " " 'display `(space :align-to ,nx)
                                                     'face (or bg 'default))))))
                     (push (concat s (im v) "\n") out)))
                 (push (if (= ri 2) (rule bl bu br) (rule lt cx rt)) out)))
      (push (mandal-org--help-text (+ (* 3 cols) 4)) out)
      (apply #'concat (nreverse out)))))

(defun mandal-org--render-grid ()
  "Return the current view as a propertized text mandala.
Uses the XPM goban-style frame on graphical displays when
`mandal-org-frame-style' is `xpm', otherwise the plain text frame.
Neither uses a window/frame-size measurement or a resize hook."
  (if (and (eq mandal-org-frame-style 'xpm)
           (display-graphic-p)
           (image-type-available-p 'xpm))
      (mandal-org--render-grid-xpm)
    (mandal-org--render-grid-text)))

(defun mandal-org--view-buffer (source-buffer)
  "Return the mandala view buffer for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (or (and (buffer-live-p mandal-org--last-view-buffer)
             mandal-org--last-view-buffer)
        (let ((buffer (generate-new-buffer
                       (format "*Org Mandala: %s*" (buffer-name source-buffer)))))
          (setq mandal-org--last-view-buffer buffer)
          buffer))))

(defun mandal-org--refresh-selected-marker ()
  "Refresh `mandal-org-selected-marker' from the current selection."
  (setq mandal-org-selected-marker (mandal-org--selected-marker)))

(defun mandal-org-refresh ()
  "Redraw the current mandala view from the source Org buffer.
The grid is plain text (no image), so this never measures a window
or frame and can never trigger a resize loop or freeze."
  (interactive)
  (unless (eq major-mode 'mandal-org-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (let* ((inhibit-read-only t)
         (source (mandal-org--ensure-view-state))
         (current mandal-org-current-marker)
         (selected mandal-org-selected-position)
         (return mandal-org-edit-return-marker))
    (setq header-line-format
          (format " %s   selected: %s"
                  (mandal-org--heading-title current)
                  selected))
    (erase-buffer)
    (insert (mandal-org--render-grid))
    (setq mandal-org-source-buffer source
          mandal-org-source-file (mandal-org--source-file-name source)
          mandal-org-current-marker current
          mandal-org-current-position (mandal-org--marker-position current)
          mandal-org-selected-position selected
          mandal-org-edit-return-marker return
          mandal-org-edit-return-position (mandal-org--marker-position return))
    (mandal-org--refresh-selected-marker)
    (goto-char (point-min))))

(defun mandal-org--display-view (source-buffer current-marker &optional selected-position return-marker)
  "Display SOURCE-BUFFER's mandala view centered on CURRENT-MARKER."
  (let ((view (mandal-org--view-buffer source-buffer)))
    (with-current-buffer view
      (mandal-org-view-mode)
      (setq mandal-org-source-buffer source-buffer
            mandal-org-source-file (mandal-org--source-file-name source-buffer)
            mandal-org-current-marker (mandal-org--copy-marker current-marker)
            mandal-org-current-position (mandal-org--marker-position current-marker)
            mandal-org-selected-position (or selected-position 'center)
            mandal-org-edit-return-marker (when return-marker
                                             (mandal-org--copy-marker return-marker))
            mandal-org-edit-return-position (mandal-org--marker-position return-marker)))
    (with-current-buffer source-buffer
      (setq mandal-org--last-view-buffer view
            mandal-org--last-view-current-marker (mandal-org--copy-marker current-marker)
            mandal-org--last-view-selected-position (or selected-position 'center)
            mandal-org--last-edit-return-marker (when return-marker
                                                   (mandal-org--copy-marker return-marker))))
    (pop-to-buffer-same-window view)
    (with-current-buffer view
      (mandal-org-refresh))))

(defun mandal-org--select-position (position)
  "Select POSITION in the current mandala view."
  (unless (eq major-mode 'mandal-org-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (mandal-org--ensure-view-state)
  (setq mandal-org-selected-position position)
  (mandal-org-refresh))

(defun mandal-org-select-nw () (interactive) (mandal-org--select-position 'nw))
(defun mandal-org-select-n () (interactive) (mandal-org--select-position 'n))
(defun mandal-org-select-ne () (interactive) (mandal-org--select-position 'ne))
(defun mandal-org-select-w () (interactive) (mandal-org--select-position 'w))
(defun mandal-org-select-center () (interactive) (mandal-org--select-position 'center))
(defun mandal-org-select-e () (interactive) (mandal-org--select-position 'e))
(defun mandal-org-select-sw () (interactive) (mandal-org--select-position 'sw))
(defun mandal-org-select-s () (interactive) (mandal-org--select-position 's))
(defun mandal-org-select-se () (interactive) (mandal-org--select-position 'se))

(defconst mandal-org--cycle-order '(center s sw w nw n ne e se)
  "Cell order for `mandal-org-select-next'/`-previous'.
Starting at the center, stepping forward walks the ring clockwise:
center -> s -> sw -> w -> nw -> n -> ne -> e -> se -> center.")

(defun mandal-org--cycle-position (position delta)
  "Return the cell DELTA steps from POSITION in `mandal-org--cycle-order'."
  (let* ((order mandal-org--cycle-order)
         (idx (or (cl-position position order) 0)))
    (nth (mod (+ idx delta) (length order)) order)))

(defun mandal-org-select-next ()
  "Select the next mandala cell clockwise, wrapping around."
  (interactive)
  (mandal-org--select-position
   (mandal-org--cycle-position mandal-org-selected-position 1)))

(defun mandal-org-select-previous ()
  "Select the previous mandala cell counterclockwise, wrapping around."
  (interactive)
  (mandal-org--select-position
   (mandal-org--cycle-position mandal-org-selected-position -1)))

(defun mandal-org-enter-selected-cell ()
  "Enter the selected existing surrounding cell as the next center."
  (interactive)
  (unless (eq major-mode 'mandal-org-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (mandal-org--ensure-view-state)
  (if (eq mandal-org-selected-position 'center)
      (message "Already at center")
    (let ((marker (mandal-org--selected-marker)))
      (if marker
          (progn
            (setq mandal-org-current-marker marker
                  mandal-org-current-position (mandal-org--marker-position marker)
                  mandal-org-selected-position 'center
                  mandal-org-edit-return-marker nil
                  mandal-org-edit-return-position nil)
            (with-current-buffer mandal-org-source-buffer
              (setq mandal-org--last-view-current-marker
                    (mandal-org--copy-marker marker)
                    mandal-org--last-view-selected-position 'center
                    mandal-org--last-edit-return-marker nil))
            (mandal-org-refresh))
        (message "No cell at %s" mandal-org-selected-position)))))

(defun mandal-org-up ()
  "Move to the parent mandala of the current center heading."
  (interactive)
  (unless (eq major-mode 'mandal-org-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (mandal-org--ensure-view-state)
  (let ((parent (mandal-org--goto-parent mandal-org-current-marker)))
    (if parent
        (progn
          (setq mandal-org-current-marker parent
                mandal-org-current-position (mandal-org--marker-position parent)
                mandal-org-selected-position 'center
                mandal-org-edit-return-marker nil
                mandal-org-edit-return-position nil)
          (with-current-buffer mandal-org-source-buffer
            (setq mandal-org--last-view-current-marker
                  (mandal-org--copy-marker parent)
                  mandal-org--last-view-selected-position 'center
                  mandal-org--last-edit-return-marker nil))
          (mandal-org-refresh))
      (message "Already at the top mandala"))))

(defvar-local mandal-org-edit--view-buffer nil
  "Mandala view buffer to refresh when this cell edit finishes.")

(defun mandal-org-edit-no-star ()
  "Refuse to self-insert `*' while editing a mandala cell.
A `*' can start an Org heading and break the cell/mandala structure,
so it is disabled here.  Use \\[quoted-insert] * to force one if you
really must."
  (interactive)
  (message "`*' is disabled while editing a mandala cell (it can create a heading)"))

(defvar mandal-org-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'mandal-org-edit-finish)
    (define-key map (kbd "C-c C-k") #'mandal-org-edit-cancel)
    (define-key map (kbd "*") #'mandal-org-edit-no-star)
    map)
  "Keymap for `mandal-org-edit-mode'.")

(define-minor-mode mandal-org-edit-mode
  "Minor mode for editing one mandala cell in a popup buffer.

\\<mandal-org-edit-mode-map>\\[mandal-org-edit-finish] saves the \
file, closes the popup, and refreshes the mandala view.
\\[mandal-org-edit-cancel] closes the popup without writing the file."
  :lighter " Mandala-Edit"
  (when mandal-org-edit-mode
    (setq header-line-format
          (substitute-command-keys
           (concat " Edit cell — "
                   "\\[mandal-org-edit-finish] save & close   "
                   "\\[mandal-org-edit-cancel] close without saving")))))

(defun mandal-org-edit-selected-cell ()
  "Edit the selected cell as Org text in a popup window.

The popup shows the cell's heading and its direct body, narrowed
from the source Org buffer.  If the selected surrounding cell does
not exist, create it first.  Save and close with
\\<mandal-org-edit-mode-map>\\[mandal-org-edit-finish]."
  (interactive)
  (unless (eq major-mode 'mandal-org-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (mandal-org--ensure-view-state)
  (let* ((source mandal-org-source-buffer)
         (view (current-buffer))
         (selected mandal-org-selected-position)
         (marker (if (eq selected 'center)
                     mandal-org-current-marker
                   (or (mandal-org--selected-marker)
                       (mandal-org--create-cell mandal-org-current-marker selected))))
         (pos (marker-position marker))
         (edit (make-indirect-buffer
                source
                (generate-new-buffer-name
                 (format "*Mandala edit: %s*"
                         (mandal-org--heading-title marker)))
                t)))
    (with-current-buffer edit
      (org-mode)
      (goto-char pos)
      (org-back-to-heading t)
      (let* ((beg (point))
             (level (org-outline-level))
             (subtree-end (save-excursion (org-end-of-subtree t t)))
             (end (save-excursion
                    (mandal-org--subtree-body-end level subtree-end))))
        (narrow-to-region beg end))
      (when (fboundp 'org-fold-hide-drawer-all)
        (ignore-errors (org-fold-hide-drawer-all)))
      (setq mandal-org-edit--view-buffer view)
      (mandal-org-edit-mode 1)
      ;; Make the popup look like a normal Org buffer: enable Org font-lock
      ;; and fontify the narrowed region now (indirect + narrowed buffers do
      ;; not always get fontified on their own).
      (unless font-lock-mode (font-lock-mode 1))
      (ignore-errors (font-lock-ensure (point-min) (point-max)))
      (goto-char (point-min))
      (end-of-line))
    (select-window
     (display-buffer-in-side-window
      edit `((side . bottom)
             (slot . 0)
             (window-height . ,mandal-org-edit-popup-height))))
    (message "Edit the cell, then press C-c C-c to save and close")))

(defun mandal-org-edit--quit ()
  "Close the current cell edit popup and kill its buffer."
  (let ((win (get-buffer-window (current-buffer))))
    (if (window-live-p win)
        (quit-window t win)
      (kill-buffer (current-buffer)))))

(defun mandal-org-edit--return-to-view (view)
  "Select VIEW's window if shown and refresh the mandala."
  (when (buffer-live-p view)
    (let ((win (get-buffer-window view)))
      (when (window-live-p win)
        (select-window win)))
    (with-current-buffer view
      (when (eq major-mode 'mandal-org-view-mode)
        (mandal-org-refresh)))))

(defun mandal-org-edit-finish ()
  "Save the edited cell to its file, close the popup, refresh the view."
  (interactive)
  (unless mandal-org-edit-mode
    (user-error "Not in a mandala cell edit buffer"))
  (let ((view mandal-org-edit--view-buffer)
        (base (buffer-base-buffer)))
    (when (buffer-live-p base)
      (with-current-buffer base
        (save-buffer)))
    (mandal-org-edit--quit)
    (mandal-org-edit--return-to-view view)))

(defun mandal-org-edit-cancel ()
  "Close the cell edit popup without writing the file, then refresh."
  (interactive)
  (unless mandal-org-edit-mode
    (user-error "Not in a mandala cell edit buffer"))
  (let ((view mandal-org-edit--view-buffer))
    (mandal-org-edit--quit)
    (mandal-org-edit--return-to-view view)))

(defun mandal-org-view ()
  "Show the mandala view for the current Org buffer.

When returning from an edit started in a mandala view, restore the
same center heading that was visible before editing."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "mandal-org-view must be called from an Org buffer"))
  (let* ((source (current-buffer))
         (current (or mandal-org--last-edit-return-marker
                      mandal-org--last-view-current-marker
                      (save-excursion
                        (if (org-before-first-heading-p)
                            (mandal-org--first-heading-marker)
                          (org-back-to-heading t)
                          (mandal-org--marker-at-point)))))
         (selected (or mandal-org--last-view-selected-position 'center)))
    (setq mandal-org--last-edit-return-marker nil)
    (mandal-org--display-view source current selected nil)))

(defun mandal-org--slug (title)
  "Return a Denote-like slug for TITLE.
Lowercase, with each run of non-alphanumeric characters collapsed
to a single hyphen.  Non-Latin letters (e.g. Japanese) are kept."
  (let ((slug (string-trim
               (replace-regexp-in-string "[^[:alnum:]]+" "-" (downcase title))
               "-+" "-+")))
    (if (string-empty-p slug) "mandala" slug)))

(defun mandal-org--new-file-path (keyword)
  "Return a Denote-like file path for KEYWORD in `mandal-org-directory'."
  (expand-file-name
   (format "%s--%s.org"
           (format-time-string "%Y%m%dT%H%M%S")
           (mandal-org--slug keyword))
   mandal-org-directory))

(defun mandal-org--files ()
  "Return existing mandala Org files in `mandal-org-directory'."
  (when (file-directory-p mandal-org-directory)
    (directory-files mandal-org-directory t "\\.org\\'")))

(defun mandal-org--open-file (file)
  "Open FILE and display its first heading as a mandala."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (org-mode)
      (goto-char (point-min))
      (if (org-next-visible-heading 1)
          (mandal-org--display-view buffer (mandal-org--marker-at-point) 'center)
        (user-error "No Org heading in %s" file)))))

(defun mandal-org-new (file title)
  "Create a new Org mandala FILE with top-level TITLE and show its view."
  (interactive
   (list (read-file-name "New mandala Org file: " nil nil nil nil
                         (lambda (name) (string-suffix-p ".org" name)))
         (read-string "Central theme: ")))
  (when (file-exists-p file)
    (user-error "Refusing to overwrite existing file: %s" file))
  (make-directory (file-name-directory (expand-file-name file)) t)
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert (format "#+TITLE: %s\n#+MANDALA: t\n\n" title))
      (insert "* " title "\n")
      (mandal-org--insert-property-drawer (org-id-new) nil)
      (save-buffer)
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (mandal-org--display-view buffer (mandal-org--marker-at-point) 'center))))

;;;###autoload
(defun mandal-org-open-or-create (input)
  "Open an existing mandala file, or create a new one from INPUT.

Completion offers the existing mandala files in
`mandal-org-directory'.  Selecting one opens it.  Typing a new
string treats it as the central keyword for a brand new mandala,
whose file is named with a Denote-like identifier in
`mandal-org-directory' (see \\[mandal-org-new])."
  (interactive
   (list (completing-read "Mandala (keyword or existing file): "
                          (mapcar #'file-name-nondirectory (mandal-org--files))
                          nil nil)))
  (let ((match (seq-find (lambda (f)
                           (string= (file-name-nondirectory f) input))
                         (mandal-org--files))))
    (if match
        (mandal-org--open-file match)
      (mandal-org-new (mandal-org--new-file-path input) input))))

;;;###autoload
(defun mandal-org-tutorial ()
  "Open the bundled tutorial sample and display it as a mandala."
  (interactive)
  (let ((file (mandal-org--tutorial-file)))
    (unless (file-readable-p file)
      (user-error "Tutorial file not found: %s" file))
    (mandal-org--open-file file)))

(defun mandal-org-repair-drawers ()
  "Move misplaced MANDALA_POS property drawers to just after their heading.

Org only recognizes a property drawer placed immediately after the
heading (and any planning line).  When a cell's drawer ends up after
body text it is detected only by a fallback scan, which can leak that
cell's text into its parent.  This scans the source Org buffer and
relocates (or rebuilds) every such drawer, then saves and refreshes.

Run it from a mandala view or directly in the source Org buffer."
  (interactive)
  (let ((buf (if (eq major-mode 'mandal-org-view-mode)
                 (mandal-org--ensure-source-buffer)
               (current-buffer)))
        (n 0))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (user-error "Not an Org buffer"))
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-heading-regexp nil t)
         (goto-char (match-beginning 0))
         (when (mandal-org--repair-heading)
           (cl-incf n))
         (goto-char (line-end-position))))
      (when (and (> n 0) buffer-file-name)
        (save-buffer)))
    (when (eq major-mode 'mandal-org-view-mode)
      (mandal-org-refresh))
    (message "Repaired %d MANDALA_POS drawer%s" n (if (= n 1) "" "s"))
    n))

(defun mandal-org-quit ()
  "Quit the mandala view, killing the view buffer and its source Org buffer.
A modified source buffer prompts to save (standard `kill-buffer')."
  (interactive)
  (unless (eq major-mode 'mandal-org-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (let ((source mandal-org-source-buffer)
        (view (current-buffer)))
    (kill-buffer view)
    (when (buffer-live-p source)
      (kill-buffer source))))

(defun mandal-org-show-source ()
  "Switch from the mandala view to the source Org file at the current center."
  (interactive)
  (unless (eq major-mode 'mandal-org-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (mandal-org--ensure-view-state)
  (let ((source (mandal-org--ensure-source-buffer))
        (pos (mandal-org--marker-position mandal-org-current-marker)))
    (pop-to-buffer-same-window source)
    (when (and pos (<= pos (point-max)))
      (goto-char pos)
      (org-back-to-heading t)
      (org-fold-show-entry)
      (org-fold-show-children))))

(defun mandal-org--zoom (delta)
  "Change the mandala cell width by DELTA columns and redraw."
  (unless (eq major-mode 'mandal-org-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (setq mandal-org--cell-width
        (round (mandal-org--clamp
                (+ (mandal-org--effective-cell-width) delta) 10 60)))
  (mandal-org-refresh)
  (message "Mandala cell width: %d cols" mandal-org--cell-width))

(defun mandal-org-zoom-in ()
  "Widen mandala cells."
  (interactive)
  (mandal-org--zoom 2))

(defun mandal-org-zoom-out ()
  "Narrow mandala cells."
  (interactive)
  (mandal-org--zoom -2))

(defvar mandal-org-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") #'mandal-org-select-w)
    (define-key map (kbd "j") #'mandal-org-select-s)
    (define-key map (kbd "k") #'mandal-org-select-n)
    (define-key map (kbd "l") #'mandal-org-select-e)
    (define-key map (kbd "u") #'mandal-org-select-nw)
    (define-key map (kbd "i") #'mandal-org-select-ne)
    (define-key map (kbd "n") #'mandal-org-select-sw)
    (define-key map (kbd "m") #'mandal-org-select-se)
    (define-key map (kbd "c") #'mandal-org-select-center)
    (define-key map (kbd ".") #'mandal-org-select-center)
    (define-key map (kbd "C-n") #'mandal-org-select-next)
    (define-key map (kbd "M-n") #'mandal-org-select-previous)
    (define-key map (kbd "C-p") #'mandal-org-select-previous)
    (define-key map (kbd "e") #'mandal-org-edit-selected-cell)
    (define-key map (kbd "RET") #'mandal-org-enter-selected-cell)
    (define-key map (kbd "C-d") #'mandal-org-enter-selected-cell)
    (define-key map (kbd "DEL") #'mandal-org-up)
    (define-key map (kbd "^") #'mandal-org-up)
    (define-key map (kbd "C-u") #'mandal-org-up)
    (define-key map (kbd "g") #'mandal-org-refresh)
    (define-key map (kbd "C-c m o") #'mandal-org-show-source)
    (define-key map (kbd "C-c m r") #'mandal-org-repair-drawers)
    (define-key map (kbd "C-c m +") #'mandal-org-zoom-in)
    (define-key map (kbd "C-c m -") #'mandal-org-zoom-out)
    (define-key map (kbd "q") #'mandal-org-quit)
    map)
  "Keymap for `mandal-org-view-mode'.")

;;;###autoload
(define-derived-mode mandal-org-view-mode special-mode "Org-Mandala"
  "Major mode for viewing an Org file as a text mandala grid."
  (setq buffer-read-only t
        truncate-lines t))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-v m") #'mandal-org-view)
  (define-key org-mode-map (kbd "C-c m m") #'mandal-org-view)
  (define-key org-mode-map (kbd "C-c m r") #'mandal-org-repair-drawers))

;;;###autoload
(global-set-key (kbd "C-c m n") #'mandal-org-open-or-create)
;;;###autoload
(global-set-key (kbd "C-c m t") #'mandal-org-tutorial)

;; Defensive cleanup: older versions of this package could install a
;; `window-size-change-functions' hook that re-rendered the SVG on resize.
;; That machinery has been removed entirely, but a hook installed by an
;; older version can linger in a long-running Emacs session and freeze on
;; resize.  Strip any such lingering installation on (re)load.  These are
;; removed by symbol, so they work even though the functions no longer exist.
(remove-hook 'window-size-change-functions 'mandal-org--window-size-change)
(remove-hook 'window-size-change-functions 'mandal-org--refresh-visible-views)
(remove-hook 'window-state-change-functions 'mandal-org--window-size-change)

(provide 'mandal-org)

;;; mandal-org.el ends here
