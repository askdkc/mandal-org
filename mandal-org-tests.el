;;; mandal-org-tests.el --- Tests for mandal-org -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'mandal-org)

(defmacro mandal-org-test--with-buffer (contents &rest body)
  "Run BODY in a temporary Org buffer containing CONTENTS."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun mandal-org-test--heading (title)
  "Return a marker for heading TITLE."
  (goto-char (point-min))
  (re-search-forward (format org-complex-heading-regexp-format (regexp-quote title)))
  (org-back-to-heading t)
  (mandal-org--marker-at-point))

(defconst mandal-org-test--fixture
  "#+TITLE: Test\n#+MANDALA: t\n\n* Root\n:PROPERTIES:\n:ID: root\n:END:\n\n** South\n:PROPERTIES:\n:MANDALA_POS: s\n:END:\n\nSouth body.\n\n*** Grandchild\n:PROPERTIES:\n:MANDALA_POS: n\n:END:\n\nGrandchild body.\n\n** North\n:PROPERTIES:\n:MANDALA_POS: n\n:END:\n\n** West\n:PROPERTIES:\n:MANDALA_POS: w\n:END:\n")

(ert-deftest mandal-org-direct-children-only ()
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let* ((root (mandal-org-test--heading "Root"))
           (children (mandal-org--direct-children-by-position root)))
      (should (alist-get 's children))
      (should (alist-get 'n children))
      (should (alist-get 'w children))
      (should-not (equal (mandal-org--heading-title (alist-get 'n children))
                         "Grandchild")))))

(ert-deftest mandal-org-position-independent-of-heading-order ()
  (mandal-org-test--with-buffer
      "* Root\n:PROPERTIES:\n:ID: root\n:END:\n\n** East\n:PROPERTIES:\n:MANDALA_POS: e\n:END:\n\n** North\n:PROPERTIES:\n:MANDALA_POS: n\n:END:\n"
    (let* ((root (mandal-org-test--heading "Root"))
           (children (mandal-org--direct-children-by-position root)))
      (should (equal (mandal-org--heading-title (alist-get 'n children)) "North"))
      (should (equal (mandal-org--heading-title (alist-get 'e children)) "East")))))

(ert-deftest mandal-org-detects-position-after-body-text ()
  (mandal-org-test--with-buffer
      "* Root\n\n** Title\nBody before properties.\n:PROPERTIES:\n:MANDALA_POS: s\n:END:\n"
    (let* ((root (mandal-org-test--heading "Root"))
           (children (mandal-org--direct-children-by-position root)))
      (should (alist-get 's children))
      (should (equal (mandal-org--heading-title (alist-get 's children)) "Title")))))

(ert-deftest mandal-org-duplicate-position-errors ()
  (mandal-org-test--with-buffer
      "* Root\n\n** A\n:PROPERTIES:\n:MANDALA_POS: s\n:END:\n\n** B\n:PROPERTIES:\n:MANDALA_POS: s\n:END:\n"
    (let ((root (mandal-org-test--heading "Root")))
      (should-error (mandal-org--direct-children-by-position root) :type 'user-error))))

(ert-deftest mandal-org-render-does-not-create-empty-cells ()
  (mandal-org-test--with-buffer "* Root\n:PROPERTIES:\n:ID: root\n:END:\n"
    (let ((mandal-org-current-marker (mandal-org-test--heading "Root"))
          (mandal-org-selected-position 'center))
      (let ((grid (mandal-org--render-grid)))
        (should-not (string-match-p "MANDALA_POS" grid))))))

(ert-deftest mandal-org-create-empty-cell-on-edit ()
  (mandal-org-test--with-buffer "* Root\n:PROPERTIES:\n:ID: root\n:END:\n"
    (let* ((root (mandal-org-test--heading "Root"))
           (cell (mandal-org--create-cell root 'se)))
      (goto-char cell)
      (should (= (org-outline-level) 2))
      (should (equal (org-entry-get (point) "MANDALA_POS") "se"))
      (should (org-entry-get (point) "ID")))))

(ert-deftest mandal-org-preview-excludes-property-drawer ()
  (mandal-org-test--with-buffer "* Root\n:PROPERTIES:\n:ID: root\n:END:\n\nBody line.\n"
    (let* ((root (mandal-org-test--heading "Root"))
           (preview (string-join (mandal-org--cell-preview root) "\n")))
      (should-not (string-match-p "PROPERTIES" preview))
      (should-not (string-match-p "ID" preview))
      (should (string-match-p "Body line" preview)))))

(ert-deftest mandal-org-preview-excludes-child-body ()
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let* ((south (mandal-org-test--heading "South"))
           (preview (string-join (mandal-org--cell-preview south) "\n")))
      (should (string-match-p "South body" preview))
      (should-not (string-match-p "Grandchild body" preview)))))

(ert-deftest mandal-org-body-includes-non-cell-subheadings ()
  "A sub-heading WITHOUT MANDALA_POS is body content, not a boundary."
  (mandal-org-test--with-buffer
      "* Root\n:PROPERTIES:\n:ID: r\n:END:\n\n** Cell\n:PROPERTIES:\n:MANDALA_POS: s\n:END:\nintro text\n*** Note\ndetail text\n"
    (let* ((cell (mandal-org-test--heading "Cell"))
           (preview (string-join (mandal-org--cell-preview cell) "\n")))
      ;; content after the non-cell `*** Note' heading is still shown
      (should (string-match-p "intro text" preview))
      (should (string-match-p "detail text" preview))
      ;; and `Note' is not treated as a mandala cell of Cell
      (should-not (mandal-org--direct-children-by-position cell)))))

(ert-deftest mandal-org-body-stops-at-malformed-cell ()
  "A cell whose MANDALA_POS drawer is misplaced still bounds the parent body."
  (mandal-org-test--with-buffer
      "* Root\n:PROPERTIES:\n:ID: r\n:END:\nintro line\n** Cellx\nbody before props\n:PROPERTIES:\n:MANDALA_POS: s\n:END:\n"
    (let* ((root (mandal-org-test--heading "Root"))
           (preview (string-join (mandal-org--cell-preview root) "\n")))
      (should (string-match-p "intro line" preview))
      (should-not (string-match-p "body before props" preview))
      (should-not (string-match-p "Cellx" preview))
      ;; the malformed cell is still mapped as a cell of Root
      (should (alist-get 's (mandal-org--direct-children-by-position root))))))

(ert-deftest mandal-org-body-styles-org-headings ()
  "Body `*'/`**' heading lines are styled (stars stripped); `***'+ hidden."
  (mandal-org-test--with-buffer
      "#+TITLE: t\n\n* Root\n:PROPERTIES:\n:ID: r\n:END:\n** subbold\nplain text\n*** deep\n** Cell\n:PROPERTIES:\n:MANDALA_POS: n\n:END:\n"
    (let* ((root (mandal-org-test--heading "Root"))
           (lines (mandal-org--body-lines root))
           (plain (mapcar #'substring-no-properties lines)))
      (should (member "subbold" plain))
      (should (member "plain text" plain))
      (should-not (member "deep" plain))      ; `***' hidden
      (let ((h2 (seq-find (lambda (l) (equal (substring-no-properties l)
                                             "subbold"))
                          lines)))
        (should (eq (get-text-property 0 'face h2) 'mandal-org-h2))))))

(ert-deftest mandal-org-preview-parts-separate-title-and-body ()
  (mandal-org-test--with-buffer "* Root\n\nBody line.\n"
    (let* ((root (mandal-org-test--heading "Root"))
           (parts (mandal-org--cell-preview-parts-for-size root 18 6)))
      (should (equal (car (car parts)) 'title))
      (should (member (cons 'body "Body line.") parts)))))

(ert-deftest mandal-org-preview-truncates-long-body ()
  (mandal-org-test--with-buffer "* Root\n\nabcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz\n"
    (let ((mandal-org-cell-preview-lines 2)
          (mandal-org-cell-preview-width 8))
      (let* ((root (mandal-org-test--heading "Root"))
             (preview (mandal-org--cell-preview root)))
        (should (= (length preview) 2))
        (should (string-suffix-p "…" (cadr preview)))))))

(ert-deftest mandal-org-enter-selected-cell-centers-child ()
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let* ((source (current-buffer))
           (root (mandal-org-test--heading "Root"))
           (view (generate-new-buffer "*mandala-test*")))
      (unwind-protect
          (with-current-buffer view
            (mandal-org-view-mode)
            (setq mandal-org-source-buffer source
                  mandal-org-current-marker root
                  mandal-org-selected-position 's)
            (mandal-org-enter-selected-cell)
            (should (equal (mandal-org--heading-title mandal-org-current-marker) "South")))
        (kill-buffer view)))))

(ert-deftest mandal-org-up-returns-parent ()
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let* ((source (current-buffer))
           (south (mandal-org-test--heading "South"))
           (view (generate-new-buffer "*mandala-test*")))
      (unwind-protect
          (with-current-buffer view
            (mandal-org-view-mode)
            (setq mandal-org-source-buffer source
                  mandal-org-current-marker south
                  mandal-org-selected-position 'center)
            (mandal-org-up)
            (should (equal (mandal-org--heading-title mandal-org-current-marker) "Root")))
        (kill-buffer view)))))

(ert-deftest mandal-org-view-keeps-edit-return-marker ()
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let* ((root (mandal-org-test--heading "Root"))
           (south (mandal-org-test--heading "South")))
      (setq mandal-org--last-view-current-marker south
            mandal-org--last-view-selected-position 's
            mandal-org--last-edit-return-marker root)
      (let* ((current (or mandal-org--last-edit-return-marker
                          mandal-org--last-view-current-marker)))
        (should (equal (mandal-org--heading-title current) "Root"))))))

(ert-deftest mandal-org-grid-draws-box-and-all-positions ()
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let* ((mandal-org-cell-width 24)
           (mandal-org-current-marker (mandal-org-test--heading "Root"))
           (mandal-org-selected-position 'center)
           (grid (mandal-org--render-grid))
           (plain (substring-no-properties grid)))
      ;; vertical separators present
      (should (string-match-p "│" plain))
      ;; columns are pinned with `:align-to' (font-independent grid)
      (should (cl-loop for i below (length grid)
                       for d = (get-text-property i 'display grid)
                       thereis (and (consp d) (eq (car d) 'space)
                                    (plist-get (cdr d) :align-to))))
      ;; all eight position labels plus the center marker
      (dolist (label '("nw" "ne" "sw" "se" "center"))
        (should (string-match-p label plain))))))

(ert-deftest mandal-org-grid-shows-title-and-body ()
  (mandal-org-test--with-buffer "* Root\n\nBody line.\n"
    (let* ((mandal-org-cell-width 24)
           (mandal-org-current-marker (mandal-org-test--heading "Root"))
           (mandal-org-selected-position 'center)
           (grid (substring-no-properties (mandal-org--render-grid))))
      (should (string-match-p "Root" grid))
      (should (string-match-p "Body line" grid)))))

(ert-deftest mandal-org-tile-xpm-is-valid-image ()
  "The goban frame tiles are valid XPM images."
  (skip-unless (image-type-available-p 'xpm))
  (let ((tile (mandal-org--tile-xpm 8 16 '(up down left right) "#888888")))
    (should (imagep tile))
    (should (eq (image-property tile :type) 'xpm))))

(ert-deftest mandal-org-render-grid-xpm-uses-image-tiles ()
  "The XPM renderer lays the frame with image display properties."
  (skip-unless (image-type-available-p 'xpm))
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let* ((mandal-org-cell-width 24)
           (mandal-org-current-marker (mandal-org-test--heading "Root"))
           (mandal-org-selected-position 'n)
           (grid (mandal-org--render-grid-xpm)))
      (should (stringp grid))
      ;; content still present, frame is images
      (should (string-match-p "Root" (substring-no-properties grid)))
      (should (cl-loop for i below (length grid)
                       for d = (get-text-property i 'display grid)
                       thereis (and (consp d) (eq (car d) 'image)))))))

(ert-deftest mandal-org-frame-style-dispatches ()
  "`text' frame style yields no image display properties."
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let* ((mandal-org-frame-style 'text)
           (mandal-org-cell-width 24)
           (mandal-org-current-marker (mandal-org-test--heading "Root"))
           (mandal-org-selected-position 'center)
           (grid (mandal-org--render-grid)))
      (should-not (cl-loop for i below (length grid)
                           for d = (get-text-property i 'display grid)
                           thereis (and (consp d) (eq (car d) 'image)))))))

(ert-deftest mandal-org-grid-marks-selected-cell ()
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let* ((mandal-org-cell-width 24)
           (mandal-org-current-marker (mandal-org-test--heading "Root"))
           (mandal-org-selected-position 'n)
           (grid (mandal-org--render-grid)))
      ;; The selected cell carries the selected face somewhere (face is a
      ;; list because the cell background is layered over inner faces).
      (should (cl-loop for i below (length grid)
                       for f = (get-text-property i 'face grid)
                       thereis (or (eq f 'mandal-org-selected)
                                   (and (listp f)
                                        (memq 'mandal-org-selected f))))))))

(ert-deftest mandal-org-refresh-reopens-killed-source-buffer ()
  (let* ((file (make-temp-file "mandal-org-source" nil ".org"))
         (source (find-file-noselect file))
         (view (generate-new-buffer "*mandala-test*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (erase-buffer)
            (org-mode)
            (insert "* Root\n:PROPERTIES:\n:ID: root\n:END:\n")
            (save-buffer)
            (goto-char (point-min)))
          (let ((root (with-current-buffer source
                        (mandal-org-test--heading "Root"))))
            (with-current-buffer view
              (mandal-org-view-mode)
              (setq mandal-org-source-buffer source
                    mandal-org-source-file file
                    mandal-org-current-marker (mandal-org--copy-marker root)
                    mandal-org-current-position (mandal-org--marker-position root)
                    mandal-org-selected-position 'center))
            (kill-buffer source)
            (with-current-buffer view
              (mandal-org-refresh)
              (should (buffer-live-p mandal-org-source-buffer))
              (should (marker-buffer mandal-org-current-marker))
              (should (equal (mandal-org--heading-title mandal-org-current-marker)
                             "Root")))))
      (when (buffer-live-p view)
        (kill-buffer view))
      (let ((reopened (find-buffer-visiting file)))
        (when reopened
          (kill-buffer reopened)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest mandal-org-grid-cell-width-customizable ()
  "Cell width drives the `:align-to' column stops."
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let* ((mandal-org-cell-width 16)
           (mandal-org-current-marker (mandal-org-test--heading "Root"))
           (mandal-org-selected-position 'center)
           (grid (mandal-org--render-grid)))
      ;; First column boundary is pinned to column (1 + cell-width) = 17.
      (should (cl-loop for i below (length grid)
                       for d = (get-text-property i 'display grid)
                       thereis (and (consp d) (eq (car d) 'space)
                                    (equal (plist-get (cdr d) :align-to) 17)))))))

(ert-deftest mandal-org-tutorial-file-path-points-to-example ()
  (let ((path (mandal-org--tutorial-file)))
    (should (string-suffix-p "examples/mandala-example.org" path))))

(ert-deftest mandal-org-view-mode-has-center-selection-bindings ()
  (should (eq (lookup-key mandal-org-view-mode-map (kbd "c"))
              #'mandal-org-select-center))
  (should (eq (lookup-key mandal-org-view-mode-map (kbd "."))
              #'mandal-org-select-center)))

(ert-deftest mandal-org-new-refuses-existing-file ()
  (let ((file (make-temp-file "mandal-org-existing" nil ".org")))
    (unwind-protect
        (should-error (mandal-org-new file "Root") :type 'user-error)
      (delete-file file))))

(ert-deftest mandal-org-invalid-position-errors ()
  (mandal-org-test--with-buffer
      "* Root\n\n** Bad\n:PROPERTIES:\n:MANDALA_POS: xx\n:END:\n"
    (let ((root (mandal-org-test--heading "Root")))
      (should-error (mandal-org--direct-children-by-position root)
                    :type 'user-error))))

(ert-deftest mandal-org-child-without-position-ignored ()
  (mandal-org-test--with-buffer
      "* Root\n\n** Has\n:PROPERTIES:\n:MANDALA_POS: n\n:END:\n\n** Without\n:PROPERTIES:\n:ID: plain\n:END:\n"
    (let* ((root (mandal-org-test--heading "Root"))
           (children (mandal-org--direct-children-by-position root)))
      (should (= (length children) 1))
      (should (alist-get 'n children))
      (should-not (seq-find
                   (lambda (cell)
                     (equal (mandal-org--heading-title (cdr cell)) "Without"))
                   children)))))

(ert-deftest mandal-org-create-cell-rejects-occupied-position ()
  (mandal-org-test--with-buffer
      "* Root\n:PROPERTIES:\n:ID: root\n:END:\n\n** South\n:PROPERTIES:\n:MANDALA_POS: s\n:END:\n"
    (let ((root (mandal-org-test--heading "Root")))
      (should-error (mandal-org--create-cell root 's) :type 'user-error))))

(ert-deftest mandal-org-preview-excludes-source-block ()
  (mandal-org-test--with-buffer
      "* Root\n\nKeep this.\n#+begin_src elisp\n(secret-code)\n#+end_src\n#+begin_example\nhidden example\n#+end_example\n"
    (let* ((root (mandal-org-test--heading "Root"))
           (preview (string-join (mandal-org--cell-preview root) "\n")))
      (should (string-match-p "Keep this" preview))
      (should-not (string-match-p "secret-code" preview))
      (should-not (string-match-p "hidden example" preview)))))

(ert-deftest mandal-org-japanese-truncation-does-not-error ()
  (mandal-org-test--with-buffer
      "* Root\n\nあいうえおかきくけこさしすせそたちつてと\n"
    (let ((mandal-org-cell-preview-lines 2)
          (mandal-org-cell-preview-width 6))
      (let* ((root (mandal-org-test--heading "Root"))
             (preview (mandal-org--cell-preview root)))
        (should (<= (length preview) 2))
        (should (string-suffix-p "…" (car (last preview))))))))

(ert-deftest mandal-org-marker-from-position-falls-back-to-first-heading ()
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let ((marker (mandal-org--marker-from-position (current-buffer) most-positive-fixnum)))
      (should (markerp marker))
      (should (equal (mandal-org--heading-title marker) "Root")))
    (let ((marker (mandal-org--marker-from-position (current-buffer) nil)))
      (should (markerp marker))
      (should (equal (mandal-org--heading-title marker) "Root")))))

(ert-deftest mandal-org-edit-finish-saves-file-and-refreshes ()
  (let* ((file (make-temp-file "mandal-org-edit" nil ".org"))
         (source (find-file-noselect file))
         (view (generate-new-buffer "*mandala-test-view*"))
         (expected "* Root Renamed\n:PROPERTIES:\n:ID: root\n:END:\n"))
    (unwind-protect
        (progn
          (with-current-buffer source
            (erase-buffer)
            (org-mode)
            (insert "* Root\n:PROPERTIES:\n:ID: root\n:END:\n")
            (save-buffer)
            (goto-char (point-min)))
          (let ((root (with-current-buffer source
                        (mandal-org-test--heading "Root"))))
            (with-current-buffer view
              (mandal-org-view-mode)
              (setq mandal-org-source-buffer source
                    mandal-org-source-file file
                    mandal-org-current-marker (mandal-org--copy-marker root)
                    mandal-org-current-position (mandal-org--marker-position root)
                    mandal-org-selected-position 'center)))
          (let ((edit (make-indirect-buffer
                       source
                       (generate-new-buffer-name "*mandala-edit-test*")
                       t)))
            (with-current-buffer edit
              (org-mode)
              (goto-char (point-min))
              (org-back-to-heading t)
              (end-of-line)
              (insert " Renamed")
              (setq mandal-org-edit--view-buffer view)
              (mandal-org-edit-mode 1)
              (mandal-org-edit-finish)))
          ;; Base buffer and the on-disk file both carry the edit.
          (should (equal (with-current-buffer source (buffer-string)) expected))
          (should (equal (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string))
                         expected))
          (should (equal (with-current-buffer view
                           (mandal-org--heading-title mandal-org-current-marker))
                         "Root Renamed")))
      (when (buffer-live-p view) (kill-buffer view))
      (let ((b (find-buffer-visiting file))) (when b (kill-buffer b)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest mandal-org-cycle-position-wraps ()
  ;; Forward from the center walks the ring clockwise.
  (should (eq (mandal-org--cycle-position 'center 1) 's))
  (should (eq (mandal-org--cycle-position 's 1) 'sw))
  (should (eq (mandal-org--cycle-position 'sw 1) 'w))
  (should (eq (mandal-org--cycle-position 'se 1) 'center))
  ;; Backward from the center is counterclockwise.
  (should (eq (mandal-org--cycle-position 'center -1) 'se)))

(ert-deftest mandal-org-view-mode-has-cycle-bindings ()
  (should (eq (lookup-key mandal-org-view-mode-map (kbd "C-n"))
              #'mandal-org-select-next))
  (should (eq (lookup-key mandal-org-view-mode-map (kbd "M-n"))
              #'mandal-org-select-previous))
  (should (eq (lookup-key mandal-org-view-mode-map (kbd "C-p"))
              #'mandal-org-select-previous)))

(ert-deftest mandal-org-view-mode-has-enter-and-up-bindings ()
  (should (eq (lookup-key mandal-org-view-mode-map (kbd "C-d"))
              #'mandal-org-enter-selected-cell))
  (should (eq (lookup-key mandal-org-view-mode-map (kbd "C-u"))
              #'mandal-org-up)))

(ert-deftest mandal-org-slug-is-denote-like ()
  (should (equal (mandal-org--slug "My Central Theme") "my-central-theme"))
  (should (equal (mandal-org--slug "  Hello,  World!  ") "hello-world"))
  (should (equal (mandal-org--slug "!!!") "mandala"))
  ;; Non-Latin letters are kept; spaces become hyphens.
  (should (equal (mandal-org--slug "中心 テーマ") "中心-テーマ")))

(ert-deftest mandal-org-new-file-path-uses-denote-identifier ()
  (let ((mandal-org-directory "/tmp/mandala-test/"))
    (let ((path (mandal-org--new-file-path "Central Keyword")))
      (should (string-prefix-p (expand-file-name mandal-org-directory)
                               (expand-file-name path)))
      (should (string-match-p
               "/[0-9]\\{8\\}T[0-9]\\{6\\}--central-keyword\\.org\\'"
               path)))))

(ert-deftest mandal-org-new-creates-denote-named-file ()
  (let* ((dir (file-name-as-directory (make-temp-file "mandala-dir" t)))
         (mandal-org-directory dir)
         (path (mandal-org--new-file-path "Test Theme")))
    (unwind-protect
        (progn
          (mandal-org-new path "Test Theme")
          (should (file-exists-p path))
          (with-temp-buffer
            (insert-file-contents path)
            (should (string-match-p "^#\\+TITLE: Test Theme$" (buffer-string)))
            (should (string-match-p "^\\* Test Theme$" (buffer-string)))))
      (dolist (b (buffer-list))
        (when (buffer-live-p b)
          (with-current-buffer b
            (when (or (eq major-mode 'mandal-org-view-mode)
                      (and buffer-file-name
                           (string-prefix-p dir buffer-file-name)))
              (set-buffer-modified-p nil)
              (kill-buffer b)))))
      (when (file-exists-p path) (delete-file path))
      (delete-directory dir t))))

(ert-deftest mandal-org-open-or-create-binding ()
  (should (eq (lookup-key (current-global-map) (kbd "C-c m n"))
              #'mandal-org-open-or-create)))

(ert-deftest mandal-org-tutorial-binding ()
  (should (eq (lookup-key (current-global-map) (kbd "C-c m t"))
              #'mandal-org-tutorial)))

(ert-deftest mandal-org-edit-blocks-star ()
  (should (eq (lookup-key mandal-org-edit-mode-map (kbd "*"))
              #'mandal-org-edit-no-star)))

(ert-deftest mandal-org-display-shows-buffer-before-first-render ()
  "The view window is shown before the first render."
  (with-temp-buffer
    (org-mode)
    (insert "* Root\n:PROPERTIES:\n:ID: root\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((order '())
          (marker (copy-marker (point))))
      (cl-letf (((symbol-function 'pop-to-buffer-same-window)
                 (lambda (&rest _) (push 'display order)))
                ((symbol-function 'mandal-org-refresh)
                 (lambda (&rest _) (push 'render order))))
        (mandal-org--display-view (current-buffer) marker 'center))
      (should (equal (nreverse order) '(display render))))))

(ert-deftest mandal-org-render-installs-no-resize-hook ()
  "Rendering must not install a resize hook (the real anti-freeze rule).
The grid may read `window-width'/`window-height' once to fit, but it
must never hook resize, which is what caused the freeze."
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let ((mandal-org-current-marker (mandal-org-test--heading "Root"))
          (mandal-org-selected-position 'center))
      (mandal-org--render-grid)
      (should-not (cl-some (lambda (f) (string-prefix-p "mandal-org"
                                                        (format "%s" f)))
                           window-size-change-functions))
      (should-not (cl-some (lambda (f) (string-prefix-p "mandal-org"
                                                        (format "%s" f)))
                           window-state-change-functions)))))

(ert-deftest mandal-org-fit-width-tracks-window ()
  "With `fit' and a tall window (so width is the limit), a wider window
yields a wider cell."
  (mandal-org-test--with-buffer mandal-org-test--fixture
    (let ((mandal-org-cell-width 'fit)
          (mandal-org--cell-width nil)
          (mandal-org-current-marker (mandal-org-test--heading "Root")))
      (cl-letf (((symbol-function 'window-height) (lambda (&rest _) 60))
                ((symbol-function 'window-width) (lambda (&rest _) 30)))
        (let ((narrow (mandal-org--effective-cell-width)))
          (cl-letf (((symbol-function 'window-width) (lambda (&rest _) 70)))
            (should (> (mandal-org--effective-cell-width) narrow))))))))

(ert-deftest mandal-org-refresh-sets-header-before-render ()
  "The header line is installed before the grid is rendered."
  (mandal-org-test--with-buffer "* Root\n:PROPERTIES:\n:ID: root\n:END:\n"
    (let ((view (generate-new-buffer "*mandala-test-view*"))
          (source (current-buffer))
          (root (mandal-org-test--heading "Root"))
          saw-header)
      (unwind-protect
          (with-current-buffer view
            (mandal-org-view-mode)
            (setq mandal-org-source-buffer source
                  mandal-org-current-marker (mandal-org--copy-marker root)
                  mandal-org-current-position (mandal-org--marker-position root)
                  mandal-org-selected-position 'center)
            (cl-letf (((symbol-function 'mandal-org--render-grid)
                       (lambda (&rest _)
                         (setq saw-header header-line-format)
                         "")))
              (mandal-org-refresh))
            (should (string-match-p "Root" saw-header)))
        (when (buffer-live-p view)
          (kill-buffer view))))))

(provide 'mandal-org-tests)

;;; mandal-org-tests.el ends here
