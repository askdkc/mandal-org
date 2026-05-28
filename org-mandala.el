;;; org-mandala.el --- SVG mandala view for Org headings -*- lexical-binding: t; -*-

;; Author: askdkc
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: outlines, org, notes

;;; Commentary:

;; org-mandala displays one Org file as a hierarchical 3x3 mandala.
;; The current heading is shown in the center cell, and its direct
;; children with MANDALA_POS properties are shown around it.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'seq)
(require 'subr-x)
(require 'svg)

(defgroup org-mandala nil
  "SVG mandala view for Org headings."
  :group 'org)

(defcustom org-mandala-cell-preview-lines 6
  "Maximum number of preview lines shown in one mandala cell."
  :type 'integer
  :group 'org-mandala)

(defcustom org-mandala-cell-preview-width 18
  "Maximum display width of one preview line."
  :type 'integer
  :group 'org-mandala)

(defcustom org-mandala-svg-width 900
  "Width of the generated mandala SVG."
  :type 'integer
  :group 'org-mandala)

(defcustom org-mandala-svg-height 650
  "Height of the generated mandala SVG."
  :type 'integer
  :group 'org-mandala)

(defcustom org-mandala-fit-window nil
  "When non-nil, size the mandala SVG to the current view window."
  :type 'boolean
  :group 'org-mandala)

(defcustom org-mandala-auto-refresh-on-window-size-change nil
  "Deprecated.

Window-size driven SVG refresh is intentionally disabled because
some Emacs builds can become unresponsive while repeatedly
rasterizing SVG images during frame resizing.  Use
`org-mandala-refresh' manually after resizing."
  :type 'boolean
  :group 'org-mandala)

(defcustom org-mandala-window-size-change-delay 0.18
  "Deprecated idle delay for the disabled window-size refresh path."
  :type 'number
  :group 'org-mandala)

(defcustom org-mandala-min-svg-width 520
  "Minimum SVG width used when fitting a mandala view to a window."
  :type 'integer
  :group 'org-mandala)

(defcustom org-mandala-min-svg-height 420
  "Minimum SVG height used when fitting a mandala view to a window."
  :type 'integer
  :group 'org-mandala)

(defcustom org-mandala-min-window-columns 36
  "Minimum window columns required to render the mandala SVG view."
  :type 'integer
  :group 'org-mandala)

(defcustom org-mandala-min-window-lines 14
  "Minimum window lines required to render the mandala SVG view."
  :type 'integer
  :group 'org-mandala)

(defconst org-mandala--positions '(nw n ne w e sw s se))
(defconst org-mandala--grid '((nw n ne) (w center e) (sw s se)))

(defvar org-mandala--window-size-timer nil)

(defvar-local org-mandala-source-buffer nil)
(defvar-local org-mandala-source-file nil)
(defvar-local org-mandala-current-marker nil)
(defvar-local org-mandala-current-position nil)
(defvar-local org-mandala-selected-position 'center)
(defvar-local org-mandala-selected-marker nil)
(defvar-local org-mandala-edit-return-marker nil)
(defvar-local org-mandala-edit-return-position nil)
(defvar-local org-mandala--last-rendered-window-size nil)

(defvar-local org-mandala--last-view-buffer nil)
(defvar-local org-mandala--last-view-current-marker nil)
(defvar-local org-mandala--last-view-selected-position 'center)
(defvar-local org-mandala--last-edit-return-marker nil)

(defun org-mandala--package-root ()
  "Return the directory that contains `org-mandala.el'."
  (file-name-directory
   (or load-file-name
       (locate-library "org-mandala.el")
       (buffer-file-name))))

(defun org-mandala--tutorial-file ()
  "Return the tutorial Org file path."
  (expand-file-name "examples/mandala-example.org"
                    (org-mandala--package-root)))

(defun org-mandala--marker-at-point ()
  "Return a marker at point in the current buffer."
  (let ((marker (make-marker)))
    (set-marker marker (point) (current-buffer))
    marker))

(defun org-mandala--copy-marker (marker)
  "Return a copy of MARKER."
  (let ((copy (make-marker)))
    (set-marker copy (marker-position marker) (marker-buffer marker))
    copy))

(defun org-mandala--marker-position (marker)
  "Return MARKER's position, or nil."
  (and (markerp marker)
       (marker-position marker)))

(defun org-mandala--source-file-name (buffer)
  "Return BUFFER's file name, or nil."
  (and (buffer-live-p buffer)
       (buffer-file-name buffer)))

(defun org-mandala--marker-from-position (buffer position)
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
          (org-mandala--first-heading-marker)
        (org-back-to-heading t)
        (org-mandala--marker-at-point)))))

(defun org-mandala--ensure-source-buffer ()
  "Return a live source Org buffer for the current mandala view.

If the original source buffer was killed but the view still knows
the file name, reopen the file and restore the buffer reference."
  (cond
   ((buffer-live-p org-mandala-source-buffer)
    org-mandala-source-buffer)
   ((and org-mandala-source-file
         (file-readable-p org-mandala-source-file))
    (setq org-mandala-source-buffer
          (find-file-noselect org-mandala-source-file))
    (with-current-buffer org-mandala-source-buffer
      (org-mode))
    org-mandala-source-buffer)
   (t
    (user-error "Source Org buffer is no longer live and cannot be reopened"))))

(defun org-mandala--ensure-view-state ()
  "Ensure the current view has a live source buffer and markers."
  (let ((source (org-mandala--ensure-source-buffer)))
    (unless (and (markerp org-mandala-current-marker)
                 (marker-buffer org-mandala-current-marker))
      (setq org-mandala-current-marker
            (org-mandala--marker-from-position source org-mandala-current-position)))
    (when (and org-mandala-edit-return-position
               (not (and (markerp org-mandala-edit-return-marker)
                         (marker-buffer org-mandala-edit-return-marker))))
      (setq org-mandala-edit-return-marker
            (org-mandala--marker-from-position source
                                               org-mandala-edit-return-position)))
    source))

(defun org-mandala--valid-position-p (position)
  "Return non-nil when POSITION is a valid mandala position symbol."
  (memq position org-mandala--positions))

(defun org-mandala--position-string (position)
  "Return POSITION as an Org property string."
  (symbol-name position))

(defun org-mandala--property-position ()
  "Return MANDALA_POS at point as a symbol, or nil if absent."
  (let ((value (org-entry-get (point) "MANDALA_POS")))
    (when (and value (not (string-empty-p value)))
      (intern value))))

(defun org-mandala--subtree-body-end (level subtree-end)
  "Return the end of LEVEL heading's direct body before SUBTREE-END."
  (save-excursion
    (forward-line 1)
    (let ((body-start (point))
          (found nil))
      (while (and (not found)
                  (re-search-forward org-heading-regexp subtree-end t))
        (let ((heading-start (match-beginning 0)))
          (goto-char heading-start)
          (when (<= (org-outline-level) (1+ level))
            (setq found heading-start))
          (forward-line 1)))
      (or found subtree-end))))

(defun org-mandala--fallback-property-position (body-end)
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

(defun org-mandala--cell-position (body-end)
  "Return the mandala position at point, checking malformed drawers to BODY-END."
  (or (org-mandala--property-position)
      (org-mandala--fallback-property-position body-end)))

(defun org-mandala--heading-title (marker)
  "Return the Org heading title at MARKER."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (org-get-heading t t t t))))

(defun org-mandala--first-heading-marker ()
  "Return a marker for the first heading in the current Org buffer."
  (save-excursion
    (goto-char (point-min))
    (unless (org-next-visible-heading 1)
      (user-error "No Org heading found"))
    (org-mandala--marker-at-point)))

(defun org-mandala--direct-children-by-position (marker)
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
                                 (org-mandala--subtree-body-end level child-end)))
                     (pos (org-mandala--cell-position body-end)))
                (when pos
                  (unless (org-mandala--valid-position-p pos)
                    (user-error "Invalid MANDALA_POS: %s" pos))
                  (when (alist-get pos children)
                    (user-error "Duplicate MANDALA_POS under %s: %s"
                                (org-mandala--heading-title marker) pos))
                  (push (cons pos (org-mandala--marker-at-point)) children))))))
          (forward-line 1))
        children))))

(defun org-mandala--goto-parent (marker)
  "Return a marker for MARKER's parent heading, or nil at top level."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (org-back-to-heading t)
      (when (org-up-heading-safe)
        (org-mandala--marker-at-point)))))

(defun org-mandala--insert-property-drawer (id position)
  "Insert a property drawer for ID and POSITION at point."
  (insert ":PROPERTIES:\n")
  (insert (format ":ID:          %s\n" id))
  (when position
    (insert (format ":MANDALA_POS: %s\n" (org-mandala--position-string position))))
  (insert ":END:\n\n"))

(defun org-mandala--create-cell (parent-marker position)
  "Create a child heading below PARENT-MARKER at POSITION.

Return a marker pointing to the new heading.  The new title is
initially empty, and point is left where the title can be typed."
  (unless (org-mandala--valid-position-p position)
    (user-error "Invalid mandala position: %s" position))
  (with-current-buffer (marker-buffer parent-marker)
    (save-excursion
      (goto-char parent-marker)
      (org-back-to-heading t)
      (let* ((parent-level (org-outline-level))
             (children (org-mandala--direct-children-by-position parent-marker)))
        (when (alist-get position children)
          (user-error "Cell already exists at %s" position))
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (unless (or (bobp) (looking-back "\n\n" nil)) (insert "\n"))
        (let ((heading-start (point)))
          (insert (make-string (1+ parent-level) ?*) " \n")
          (org-mandala--insert-property-drawer (org-id-new) position)
          (copy-marker heading-start))))))

(defun org-mandala--drawer-line-p (line)
  "Return non-nil if LINE opens or closes an Org drawer."
  (string-match-p "\\`[ \t]*:\\([[:alnum:]_@#%]+\\|END\\):[ \t]*\\'" line))

(defun org-mandala--source-block-line-p (line)
  "Return non-nil if LINE starts or ends an Org source/example block."
  (string-match-p "\\`[ \t]*#\\+\\(begin\\|end\\)_\\(src\\|example\\)\\b" (downcase line)))

(defun org-mandala--body-lines (marker)
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
                         (org-mandala--subtree-body-end level subtree-end)))
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
           ((and in-block (org-mandala--source-block-line-p line))
            (setq in-block nil))
           (in-block)
           ((org-mandala--drawer-line-p line)
            (setq in-drawer t))
           ((org-mandala--source-block-line-p line)
            (setq in-block t))
           ((string-blank-p line))
           (t (push (string-trim line) kept))))
        (nreverse kept)))))

(defun org-mandala--wrap-line (line width)
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

(defun org-mandala--fit-lines (lines width max-lines)
  "Fit LINES to WIDTH and MAX-LINES, adding ellipsis when truncated."
  (let ((wrapped nil))
    (dolist (line lines)
      (setq wrapped (append wrapped (org-mandala--wrap-line line width))))
    (let ((truncated (> (length wrapped) max-lines))
          (result (seq-take wrapped max-lines)))
      (when (and truncated result)
        (let* ((last (car (last result)))
               (room (max 0 (1- width)))
               (short (truncate-string-to-width last room nil nil "")))
          (setq result (append (butlast result) (list (concat short "…"))))))
      result)))

(defun org-mandala--clamp (value min-value max-value)
  "Clamp VALUE between MIN-VALUE and MAX-VALUE."
  (min max-value (max min-value value)))

(defun org-mandala--window-svg-size ()
  "Return SVG size as a cons cell for the current mandala view."
  (if (not org-mandala-fit-window)
      (cons org-mandala-svg-width org-mandala-svg-height)
    (let* ((window (or (get-buffer-window (current-buffer) t)
                       (selected-window)))
           (pixel-width (and (window-live-p window)
                             (window-pixel-width window)))
           (pixel-height (and (window-live-p window)
                              (window-pixel-height window))))
      (cons (max org-mandala-min-svg-width
                 (or (and pixel-width (- pixel-width 24))
                     org-mandala-svg-width))
            (max org-mandala-min-svg-height
                 (or (and pixel-height (- pixel-height 24))
                     org-mandala-svg-height))))))

(defun org-mandala--window-pixel-size (window)
  "Return WINDOW's pixel size as a cons cell."
  (cons (window-pixel-width window)
        (window-pixel-height window)))

(defun org-mandala--window-too-small-p (&optional window)
  "Return non-nil when WINDOW is too small for safe SVG rendering."
  (let ((win (or window (get-buffer-window (current-buffer) t) (selected-window))))
    (and (window-live-p win)
         (or (< (window-total-width win) org-mandala-min-window-columns)
             (< (window-total-height win) org-mandala-min-window-lines)))))

(defun org-mandala--font-size-for-cell (cell-width cell-height center)
  "Return a readable font size for a cell of CELL-WIDTH and CELL-HEIGHT.

When CENTER is non-nil, return a larger size for the center cell."
  (let* ((base (min (/ cell-width 10.5) (/ cell-height 6.8)))
         (scaled (if center (* base 1.12) base)))
    (round (org-mandala--clamp scaled 14 32))))

(defun org-mandala--preview-width-for-cell (cell-width font-size)
  "Return preview display width for CELL-WIDTH and FONT-SIZE."
  (max 8
       (round (max org-mandala-cell-preview-width
                   (/ (- cell-width 56) (* font-size 0.56))))))

(defun org-mandala--preview-lines-for-cell (cell-height font-size)
  "Return preview line count for CELL-HEIGHT and FONT-SIZE."
  (max 1
       (min org-mandala-cell-preview-lines
            (floor (/ (max 0 (- cell-height 76)) (* font-size 1.22))))))

(defun org-mandala--cell-preview (marker)
  "Return display preview lines for the heading at MARKER."
  (let* ((title (org-mandala--heading-title marker))
         (body (org-mandala--body-lines marker)))
    (org-mandala--fit-lines
     (cons (if (string-empty-p title) "(untitled)" title) body)
     org-mandala-cell-preview-width
     org-mandala-cell-preview-lines)))

(defun org-mandala--cell-preview-parts-for-size (marker width max-lines)
  "Return title/body preview parts for MARKER fitted to WIDTH and MAX-LINES.

Each returned element is (KIND . TEXT), where KIND is `title' or
`body'."
  (let* ((title (org-mandala--heading-title marker))
         (title-lines (org-mandala--fit-lines
                       (list (if (string-empty-p title) "(untitled)" title))
                       width
                       2))
         (body-lines (org-mandala--fit-lines
                      (org-mandala--body-lines marker)
                      width
                      (max 0 (- max-lines (length title-lines)))))
         parts)
    (dolist (line title-lines)
      (push (cons 'title line) parts))
    (dolist (line body-lines)
      (push (cons 'body line) parts))
    (nreverse parts)))

(defun org-mandala--cell-preview-for-size (marker width lines)
  "Return display preview lines for MARKER fitted to WIDTH and LINES."
  (let* ((title (org-mandala--heading-title marker))
         (body (org-mandala--body-lines marker)))
    (org-mandala--fit-lines
     (cons (if (string-empty-p title) "(untitled)" title) body)
     width
     lines)))

(defun org-mandala--selected-marker ()
  "Return marker for the selected cell in the current view buffer."
  (if (eq org-mandala-selected-position 'center)
      org-mandala-current-marker
    (alist-get org-mandala-selected-position
               (org-mandala--direct-children-by-position org-mandala-current-marker))))

(defun org-mandala--cell-lines (position children width lines)
  "Return preview lines for POSITION using CHILDREN alist."
  (cond
   ((eq position 'center)
    (org-mandala--cell-preview-parts-for-size org-mandala-current-marker width lines))
   ((alist-get position children)
    (org-mandala--cell-preview-parts-for-size (alist-get position children) width lines))
   (t (list (cons 'empty "+")))))

(defun org-mandala--render-svg (&optional _view-state)
  "Render the current view state as an SVG image object."
  (let* ((size (org-mandala--window-svg-size))
         (width (car size))
         (height (cdr size))
         (short-side (min width height))
         (margin (round (org-mandala--clamp (/ short-side 22.0) 20 48)))
         (gap (round (org-mandala--clamp (/ short-side 58.0) 10 22)))
         (header (round (org-mandala--clamp (/ height 13.0) 44 68)))
         (footer (round (org-mandala--clamp (/ height 28.0) 24 38)))
         (grid-width (- width (* 2 margin) (* 2 gap)))
         (grid-height (- height header footer (* 2 margin) (* 2 gap)))
         (cell-width (/ grid-width 3.0))
         (cell-height (/ grid-height 3.0))
         (title-font (round (org-mandala--clamp (/ width 50.0) 16 26)))
         (help-font (round (org-mandala--clamp (/ width 70.0) 12 17)))
         (label-font (round (org-mandala--clamp (/ cell-width 16.0) 12 18)))
         (children (org-mandala--direct-children-by-position org-mandala-current-marker))
         (svg (svg-create width height)))
    (svg-rectangle svg 0 0 width height :fill "#08090d")
    (svg-text svg
              (format "Org Mandala: %s" (org-mandala--heading-title org-mandala-current-marker))
              :x margin :y (round (* header 0.58))
              :fill "#e8edf2" :font-size title-font :font-family "sans-serif")
    (svg-text svg "h/j/k/l/y/u/b/n select  c/. center  e edit  RET enter  DEL/^ up  g refresh  q quit"
              :x margin :y (- height (round (* footer 0.35)))
              :fill "#9aa4ad" :font-size help-font :font-family "sans-serif")
    (cl-loop for row from 0 below 3
             for row-positions in org-mandala--grid do
             (cl-loop for col from 0 below 3
                      for pos in row-positions do
                      (let* ((x (+ margin (* col (+ cell-width gap))))
                             (y (+ header margin (* row (+ cell-height gap))))
                             (selected (eq pos org-mandala-selected-position))
                             (fill (if (eq pos 'center) "#171d24" "#11161d"))
                             (stroke (if selected "#ffffff" "#39424d"))
                             (stroke-width (if selected 5 2))
                             (cell-title-font (org-mandala--font-size-for-cell
                                         cell-width cell-height (eq pos 'center)))
                             (cell-body-font (max 11 (round (* cell-title-font 0.82))))
                             (title-line-height (round (* cell-title-font 1.22)))
                             (body-line-height (round (* cell-body-font 1.28)))
                             (preview-width (org-mandala--preview-width-for-cell
                                             cell-width cell-body-font))
                             (preview-lines (org-mandala--preview-lines-for-cell
                                             cell-height cell-body-font))
                             (lines (org-mandala--cell-lines pos children
                                                             preview-width
                                                             preview-lines))
                             (radius (round (org-mandala--clamp (/ cell-width 17.0) 10 18))))
                        (svg-rectangle svg x y cell-width cell-height
                                       :rx radius :ry radius
                                       :fill fill :stroke stroke :stroke-width stroke-width)
                        (svg-text svg (symbol-name pos)
                                  :x (+ x 22) :y (+ y (round (* label-font 1.65)))
                                  :fill "#6f7a86" :font-size label-font
                                  :font-family "sans-serif")
                        (let ((text-y (+ y (round (* label-font 2.9)))))
                          (dolist (part lines)
                            (let* ((kind (car part))
                                   (line (cdr part))
                                   (font-size (if (eq kind 'body)
                                                  cell-body-font
                                                cell-title-font))
                                   (line-height (if (eq kind 'body)
                                                    body-line-height
                                                  title-line-height)))
                              (svg-text svg line
                                        :x (+ x 28)
                                        :y text-y
                                        :fill (if (eq kind 'empty) "#6f7a86" "#edf2f7")
                                        :font-size font-size
                                        :font-weight (if (eq kind 'title) "700" "400")
                                        :font-family "sans-serif")
                              (setq text-y (+ text-y line-height))))))))
    (svg-image svg :ascent 'center :scale 1.0)))

(defun org-mandala--view-buffer (source-buffer)
  "Return the mandala view buffer for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (or (and (buffer-live-p org-mandala--last-view-buffer)
             org-mandala--last-view-buffer)
        (let ((buffer (generate-new-buffer
                       (format "*Org Mandala: %s*" (buffer-name source-buffer)))))
          (setq org-mandala--last-view-buffer buffer)
          buffer))))

(defun org-mandala--refresh-selected-marker ()
  "Refresh `org-mandala-selected-marker' from the current selection."
  (setq org-mandala-selected-marker (org-mandala--selected-marker)))

(defun org-mandala-refresh ()
  "Redraw the current mandala view from the source Org buffer."
  (interactive)
  (unless (eq major-mode 'org-mandala-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (let* ((inhibit-read-only t)
         (source (org-mandala--ensure-view-state))
         (current org-mandala-current-marker)
         (selected org-mandala-selected-position)
         (return org-mandala-edit-return-marker)
         (window (get-buffer-window (current-buffer) t)))
    (erase-buffer)
    (if (org-mandala--window-too-small-p window)
        (insert "Window too small for mandala SVG.\nResize window, then press `g`.\n")
      (insert-image (org-mandala--render-svg))
      (insert "\n"))
    (setq org-mandala-source-buffer source
          org-mandala-source-file (org-mandala--source-file-name source)
          org-mandala-current-marker current
          org-mandala-current-position (org-mandala--marker-position current)
          org-mandala-selected-position selected
          org-mandala-edit-return-marker return
          org-mandala-edit-return-position (org-mandala--marker-position return))
    (when (window-live-p window)
      (setq org-mandala--last-rendered-window-size
            (org-mandala--window-pixel-size window)))
    (org-mandala--refresh-selected-marker)
    (setq header-line-format
          (format " %s   selected: %s"
                  (org-mandala--heading-title org-mandala-current-marker)
                  org-mandala-selected-position))
    (goto-char (point-min))))

(defun org-mandala--visible-view-buffers (&optional frame)
  "Return visible Org Mandala view buffers on FRAME."
  (let (buffers)
    (dolist (window (window-list frame 'no-minibuf))
      (let ((buffer (window-buffer window)))
        (when (and (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (eq major-mode 'org-mandala-view-mode)))
          (push buffer buffers))))
    (seq-uniq buffers)))

(defun org-mandala--refresh-visible-views (&optional frame)
  "Refresh visible Org Mandala views on FRAME."
  (ignore frame)
  (setq org-mandala--window-size-timer nil)
  nil)

(defun org-mandala--window-size-refresh-needed-p (&optional frame)
  "Return non-nil if a visible mandala view on FRAME needs resizing."
  (seq-some
   (lambda (buffer)
     (let ((window (get-buffer-window buffer t)))
       (and (window-live-p window)
            (with-current-buffer buffer
              (not (equal org-mandala--last-rendered-window-size
                          (org-mandala--window-pixel-size window)))))))
   (org-mandala--visible-view-buffers frame)))

(defun org-mandala--window-size-change (_frame)
  "Ignore window size changes.

SVG refresh during frame resizing can make Emacs unresponsive on
some builds, so resizing never schedules automatic redraw.  Press
\\[org-mandala-refresh] in the mandala view to redraw after resizing."
  (when (timerp org-mandala--window-size-timer)
    (cancel-timer org-mandala--window-size-timer))
  (setq org-mandala--window-size-timer nil)
  nil)

(defun org-mandala--display-view (source-buffer current-marker &optional selected-position return-marker)
  "Display SOURCE-BUFFER's mandala view centered on CURRENT-MARKER."
  (let ((view (org-mandala--view-buffer source-buffer)))
    (with-current-buffer view
      (org-mandala-view-mode)
      (setq org-mandala-source-buffer source-buffer
            org-mandala-source-file (org-mandala--source-file-name source-buffer)
            org-mandala-current-marker (org-mandala--copy-marker current-marker)
            org-mandala-current-position (org-mandala--marker-position current-marker)
            org-mandala-selected-position (or selected-position 'center)
            org-mandala-edit-return-marker (when return-marker
                                             (org-mandala--copy-marker return-marker))
            org-mandala-edit-return-position (org-mandala--marker-position return-marker))
      (org-mandala-refresh))
    (with-current-buffer source-buffer
      (setq org-mandala--last-view-buffer view
            org-mandala--last-view-current-marker (org-mandala--copy-marker current-marker)
            org-mandala--last-view-selected-position (or selected-position 'center)
            org-mandala--last-edit-return-marker (when return-marker
                                                   (org-mandala--copy-marker return-marker))))
    (pop-to-buffer-same-window view)))

(defun org-mandala--select-position (position)
  "Select POSITION in the current mandala view."
  (unless (eq major-mode 'org-mandala-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (org-mandala--ensure-view-state)
  (setq org-mandala-selected-position position)
  (org-mandala-refresh))

(defun org-mandala-select-nw () (interactive) (org-mandala--select-position 'nw))
(defun org-mandala-select-n () (interactive) (org-mandala--select-position 'n))
(defun org-mandala-select-ne () (interactive) (org-mandala--select-position 'ne))
(defun org-mandala-select-w () (interactive) (org-mandala--select-position 'w))
(defun org-mandala-select-center () (interactive) (org-mandala--select-position 'center))
(defun org-mandala-select-e () (interactive) (org-mandala--select-position 'e))
(defun org-mandala-select-sw () (interactive) (org-mandala--select-position 'sw))
(defun org-mandala-select-s () (interactive) (org-mandala--select-position 's))
(defun org-mandala-select-se () (interactive) (org-mandala--select-position 'se))

(defun org-mandala-enter-selected-cell ()
  "Enter the selected existing surrounding cell as the next center."
  (interactive)
  (unless (eq major-mode 'org-mandala-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (org-mandala--ensure-view-state)
  (if (eq org-mandala-selected-position 'center)
      (message "Already at center")
    (let ((marker (org-mandala--selected-marker)))
      (if marker
          (progn
            (setq org-mandala-current-marker marker
                  org-mandala-current-position (org-mandala--marker-position marker)
                  org-mandala-selected-position 'center
                  org-mandala-edit-return-marker nil
                  org-mandala-edit-return-position nil)
            (with-current-buffer org-mandala-source-buffer
              (setq org-mandala--last-view-current-marker
                    (org-mandala--copy-marker marker)
                    org-mandala--last-view-selected-position 'center
                    org-mandala--last-edit-return-marker nil))
            (org-mandala-refresh))
        (message "No cell at %s" org-mandala-selected-position)))))

(defun org-mandala-up ()
  "Move to the parent mandala of the current center heading."
  (interactive)
  (unless (eq major-mode 'org-mandala-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (org-mandala--ensure-view-state)
  (let ((parent (org-mandala--goto-parent org-mandala-current-marker)))
    (if parent
        (progn
          (setq org-mandala-current-marker parent
                org-mandala-current-position (org-mandala--marker-position parent)
                org-mandala-selected-position 'center
                org-mandala-edit-return-marker nil
                org-mandala-edit-return-position nil)
          (with-current-buffer org-mandala-source-buffer
            (setq org-mandala--last-view-current-marker
                  (org-mandala--copy-marker parent)
                  org-mandala--last-view-selected-position 'center
                  org-mandala--last-edit-return-marker nil))
          (org-mandala-refresh))
      (message "Already at the top mandala"))))

(defun org-mandala-edit-selected-cell ()
  "Open the selected cell in the source Org buffer for normal editing.

If the selected surrounding cell does not exist, create it first."
  (interactive)
  (unless (eq major-mode 'org-mandala-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (org-mandala--ensure-view-state)
  (let* ((source org-mandala-source-buffer)
         (view (current-buffer))
         (return (org-mandala--copy-marker org-mandala-current-marker))
         (selected org-mandala-selected-position)
         (marker (if (eq selected 'center)
                     org-mandala-current-marker
                   (or (org-mandala--selected-marker)
                       (org-mandala--create-cell org-mandala-current-marker selected)))))
    (with-current-buffer source
      (setq org-mandala--last-view-buffer view
            org-mandala--last-view-current-marker return
            org-mandala--last-view-selected-position selected
            org-mandala--last-edit-return-marker return))
    (pop-to-buffer-same-window source)
    (delete-other-windows)
    (org-mode)
    (goto-char marker)
    (org-back-to-heading t)
    (end-of-line)))

(defun org-mandala-view ()
  "Show the mandala view for the current Org buffer.

When returning from an edit started in a mandala view, restore the
same center heading that was visible before editing."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mandala-view must be called from an Org buffer"))
  (let* ((source (current-buffer))
         (current (or org-mandala--last-edit-return-marker
                      org-mandala--last-view-current-marker
                      (save-excursion
                        (if (org-before-first-heading-p)
                            (org-mandala--first-heading-marker)
                          (org-back-to-heading t)
                          (org-mandala--marker-at-point)))))
         (selected (or org-mandala--last-view-selected-position 'center)))
    (setq org-mandala--last-edit-return-marker nil)
    (org-mandala--display-view source current selected nil)))

(defun org-mandala-new (file title)
  "Create a new Org mandala FILE with top-level TITLE and show its view."
  (interactive
   (list (read-file-name "New mandala Org file: " nil nil nil nil
                         (lambda (name) (string-suffix-p ".org" name)))
         (read-string "Central theme: ")))
  (when (file-exists-p file)
    (user-error "Refusing to overwrite existing file: %s" file))
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Mandala Memo\n#+MANDALA: t\n\n")
      (insert "* " title "\n")
      (org-mandala--insert-property-drawer (org-id-new) nil)
      (save-buffer)
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (org-mandala--display-view buffer (org-mandala--marker-at-point) 'center))))

;;;###autoload
(defun org-mandala-tutorial ()
  "Open the bundled tutorial sample and display it as a mandala."
  (interactive)
  (let ((file (org-mandala--tutorial-file)))
    (unless (file-readable-p file)
      (user-error "Tutorial file not found: %s" file))
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (org-mode)
        (goto-char (point-min))
        (if (org-next-visible-heading 1)
            (org-mandala--display-view buffer (org-mandala--marker-at-point) 'center)
          (user-error "Tutorial file has no Org heading: %s" file))))))

(defun org-mandala-quit ()
  "Quit mandala view and return to the source Org buffer."
  (interactive)
  (unless (eq major-mode 'org-mandala-view-mode)
    (user-error "Not in an Org Mandala view buffer"))
  (let ((source (ignore-errors (org-mandala--ensure-source-buffer)))
        (view (current-buffer)))
    (if source
        (progn
          (pop-to-buffer-same-window source)
          (delete-other-windows))
      (kill-buffer view)
      (message "Closed mandala view; source Org buffer could not be reopened"))))

(defvar org-mandala-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") #'org-mandala-select-w)
    (define-key map (kbd "j") #'org-mandala-select-s)
    (define-key map (kbd "k") #'org-mandala-select-n)
    (define-key map (kbd "l") #'org-mandala-select-e)
    (define-key map (kbd "y") #'org-mandala-select-nw)
    (define-key map (kbd "u") #'org-mandala-select-ne)
    (define-key map (kbd "b") #'org-mandala-select-sw)
    (define-key map (kbd "n") #'org-mandala-select-se)
    (define-key map (kbd "c") #'org-mandala-select-center)
    (define-key map (kbd ".") #'org-mandala-select-center)
    (define-key map (kbd "e") #'org-mandala-edit-selected-cell)
    (define-key map (kbd "RET") #'org-mandala-enter-selected-cell)
    (define-key map (kbd "DEL") #'org-mandala-up)
    (define-key map (kbd "^") #'org-mandala-up)
    (define-key map (kbd "g") #'org-mandala-refresh)
    (define-key map (kbd "q") #'org-mandala-quit)
    map)
  "Keymap for `org-mandala-view-mode'.")

;;;###autoload
(define-derived-mode org-mandala-view-mode special-mode "Org-Mandala"
  "Major mode for viewing an Org file as a SVG mandala."
  (setq buffer-read-only t
        truncate-lines t)
  ;; `org-mandala' owns the rendered size through the SVG custom variables.
  ;; Letting Emacs auto-fit image display during window changes can make
  ;; resizing expensive, especially on GUI builds with SVG rasterization.
  (when (boundp 'image-auto-resize)
    (setq-local image-auto-resize nil)))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-v m") #'org-mandala-view))

(remove-hook 'window-size-change-functions #'org-mandala--window-size-change)
(when (timerp org-mandala--window-size-timer)
  (cancel-timer org-mandala--window-size-timer)
  (setq org-mandala--window-size-timer nil))

(provide 'org-mandala)

;;; org-mandala.el ends here
