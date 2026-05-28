;;; org-mandala-tests.el --- Tests for org-mandala -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org-mandala)

(defmacro org-mandala-test--with-buffer (contents &rest body)
  "Run BODY in a temporary Org buffer containing CONTENTS."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun org-mandala-test--heading (title)
  "Return a marker for heading TITLE."
  (goto-char (point-min))
  (re-search-forward (format org-complex-heading-regexp-format (regexp-quote title)))
  (org-back-to-heading t)
  (org-mandala--marker-at-point))

(defconst org-mandala-test--fixture
  "#+TITLE: Test\n#+MANDALA: t\n\n* Root\n:PROPERTIES:\n:ID: root\n:END:\n\n** South\n:PROPERTIES:\n:MANDALA_POS: s\n:END:\n\nSouth body.\n\n*** Grandchild\n:PROPERTIES:\n:MANDALA_POS: n\n:END:\n\nGrandchild body.\n\n** North\n:PROPERTIES:\n:MANDALA_POS: n\n:END:\n\n** West\n:PROPERTIES:\n:MANDALA_POS: w\n:END:\n")

(ert-deftest org-mandala-direct-children-only ()
  (org-mandala-test--with-buffer org-mandala-test--fixture
    (let* ((root (org-mandala-test--heading "Root"))
           (children (org-mandala--direct-children-by-position root)))
      (should (alist-get 's children))
      (should (alist-get 'n children))
      (should (alist-get 'w children))
      (should-not (equal (org-mandala--heading-title (alist-get 'n children))
                         "Grandchild")))))

(ert-deftest org-mandala-position-independent-of-heading-order ()
  (org-mandala-test--with-buffer
      "* Root\n:PROPERTIES:\n:ID: root\n:END:\n\n** East\n:PROPERTIES:\n:MANDALA_POS: e\n:END:\n\n** North\n:PROPERTIES:\n:MANDALA_POS: n\n:END:\n"
    (let* ((root (org-mandala-test--heading "Root"))
           (children (org-mandala--direct-children-by-position root)))
      (should (equal (org-mandala--heading-title (alist-get 'n children)) "North"))
      (should (equal (org-mandala--heading-title (alist-get 'e children)) "East")))))

(ert-deftest org-mandala-detects-position-after-body-text ()
  (org-mandala-test--with-buffer
      "* Root\n\n** Title\nBody before properties.\n:PROPERTIES:\n:MANDALA_POS: s\n:END:\n"
    (let* ((root (org-mandala-test--heading "Root"))
           (children (org-mandala--direct-children-by-position root)))
      (should (alist-get 's children))
      (should (equal (org-mandala--heading-title (alist-get 's children)) "Title")))))

(ert-deftest org-mandala-duplicate-position-errors ()
  (org-mandala-test--with-buffer
      "* Root\n\n** A\n:PROPERTIES:\n:MANDALA_POS: s\n:END:\n\n** B\n:PROPERTIES:\n:MANDALA_POS: s\n:END:\n"
    (let ((root (org-mandala-test--heading "Root")))
      (should-error (org-mandala--direct-children-by-position root) :type 'user-error))))

(ert-deftest org-mandala-render-does-not-create-empty-cells ()
  (org-mandala-test--with-buffer "* Root\n:PROPERTIES:\n:ID: root\n:END:\n"
    (let ((org-mandala-current-marker (org-mandala-test--heading "Root"))
          (org-mandala-selected-position 'center))
      (org-mandala--render-svg)
      (goto-char (point-min))
      (should-not (re-search-forward "MANDALA_POS" nil t)))))

(ert-deftest org-mandala-create-empty-cell-on-edit ()
  (org-mandala-test--with-buffer "* Root\n:PROPERTIES:\n:ID: root\n:END:\n"
    (let* ((root (org-mandala-test--heading "Root"))
           (cell (org-mandala--create-cell root 'se)))
      (goto-char cell)
      (should (= (org-outline-level) 2))
      (should (equal (org-entry-get (point) "MANDALA_POS") "se"))
      (should (org-entry-get (point) "ID")))))

(ert-deftest org-mandala-preview-excludes-property-drawer ()
  (org-mandala-test--with-buffer "* Root\n:PROPERTIES:\n:ID: root\n:END:\n\nBody line.\n"
    (let* ((root (org-mandala-test--heading "Root"))
           (preview (string-join (org-mandala--cell-preview root) "\n")))
      (should-not (string-match-p "PROPERTIES" preview))
      (should-not (string-match-p "ID" preview))
      (should (string-match-p "Body line" preview)))))

(ert-deftest org-mandala-preview-excludes-child-body ()
  (org-mandala-test--with-buffer org-mandala-test--fixture
    (let* ((south (org-mandala-test--heading "South"))
           (preview (string-join (org-mandala--cell-preview south) "\n")))
      (should (string-match-p "South body" preview))
      (should-not (string-match-p "Grandchild body" preview)))))

(ert-deftest org-mandala-preview-parts-separate-title-and-body ()
  (org-mandala-test--with-buffer "* Root\n\nBody line.\n"
    (let* ((root (org-mandala-test--heading "Root"))
           (parts (org-mandala--cell-preview-parts-for-size root 18 6)))
      (should (equal (car (car parts)) 'title))
      (should (member (cons 'body "Body line.") parts)))))

(ert-deftest org-mandala-preview-truncates-long-body ()
  (org-mandala-test--with-buffer "* Root\n\nabcdefghijklmnopqrstuvwxyz abcdefghijklmnopqrstuvwxyz\n"
    (let ((org-mandala-cell-preview-lines 2)
          (org-mandala-cell-preview-width 8))
      (let* ((root (org-mandala-test--heading "Root"))
             (preview (org-mandala--cell-preview root)))
        (should (= (length preview) 2))
        (should (string-suffix-p "…" (cadr preview)))))))

(ert-deftest org-mandala-enter-selected-cell-centers-child ()
  (org-mandala-test--with-buffer org-mandala-test--fixture
    (let* ((source (current-buffer))
           (root (org-mandala-test--heading "Root"))
           (view (generate-new-buffer "*mandala-test*")))
      (unwind-protect
          (with-current-buffer view
            (org-mandala-view-mode)
            (setq org-mandala-source-buffer source
                  org-mandala-current-marker root
                  org-mandala-selected-position 's)
            (org-mandala-enter-selected-cell)
            (should (equal (org-mandala--heading-title org-mandala-current-marker) "South")))
        (kill-buffer view)))))

(ert-deftest org-mandala-up-returns-parent ()
  (org-mandala-test--with-buffer org-mandala-test--fixture
    (let* ((source (current-buffer))
           (south (org-mandala-test--heading "South"))
           (view (generate-new-buffer "*mandala-test*")))
      (unwind-protect
          (with-current-buffer view
            (org-mandala-view-mode)
            (setq org-mandala-source-buffer source
                  org-mandala-current-marker south
                  org-mandala-selected-position 'center)
            (org-mandala-up)
            (should (equal (org-mandala--heading-title org-mandala-current-marker) "Root")))
        (kill-buffer view)))))

(ert-deftest org-mandala-view-keeps-edit-return-marker ()
  (org-mandala-test--with-buffer org-mandala-test--fixture
    (let* ((root (org-mandala-test--heading "Root"))
           (south (org-mandala-test--heading "South")))
      (setq org-mandala--last-view-current-marker south
            org-mandala--last-view-selected-position 's
            org-mandala--last-edit-return-marker root)
      (let* ((current (or org-mandala--last-edit-return-marker
                          org-mandala--last-view-current-marker)))
        (should (equal (org-mandala--heading-title current) "Root"))))))

(ert-deftest org-mandala-svg-has-nine-rectangles ()
  (org-mandala-test--with-buffer org-mandala-test--fixture
    (let* ((org-mandala-current-marker (org-mandala-test--heading "Root"))
           (org-mandala-selected-position 'center)
           (image (org-mandala--render-svg))
           (data (plist-get (cdr image) :data)))
      (should (stringp data))
      (should (> (length data) 0))
      (with-temp-buffer
        (insert data)
        (goto-char (point-min))
        (should (>= (how-many "<rect" (point-min) (point-max)) 9))))))

(ert-deftest org-mandala-svg-distinguishes-title-and-body-text ()
  (org-mandala-test--with-buffer "* Root\n\nBody line.\n"
    (let* ((org-mandala-current-marker (org-mandala-test--heading "Root"))
           (org-mandala-selected-position 'center)
           (image (org-mandala--render-svg))
           (data (plist-get (cdr image) :data)))
      (should (string-match-p "font-weight=\"700\"" data))
      (should (string-match-p "font-weight=\"400\"" data)))))

(ert-deftest org-mandala-svg-image-uses-fixed-scale ()
  (org-mandala-test--with-buffer org-mandala-test--fixture
    (let* ((org-mandala-current-marker (org-mandala-test--heading "Root"))
           (org-mandala-selected-position 'center)
           (image (org-mandala--render-svg)))
      (should (equal (plist-get (cdr image) :scale) 1.0)))))

(ert-deftest org-mandala-view-mode-disables-image-auto-resize ()
  (let ((buffer (generate-new-buffer "*mandala-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mandala-view-mode)
          (when (boundp 'image-auto-resize)
            (should-not image-auto-resize)
            (should (local-variable-p 'image-auto-resize))))
      (kill-buffer buffer))))

(ert-deftest org-mandala-refresh-reopens-killed-source-buffer ()
  (let* ((file (make-temp-file "org-mandala-source" nil ".org"))
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
                        (org-mandala-test--heading "Root"))))
            (with-current-buffer view
              (org-mandala-view-mode)
              (setq org-mandala-source-buffer source
                    org-mandala-source-file file
                    org-mandala-current-marker (org-mandala--copy-marker root)
                    org-mandala-current-position (org-mandala--marker-position root)
                    org-mandala-selected-position 'center))
            (kill-buffer source)
            (with-current-buffer view
              (org-mandala-refresh)
              (should (buffer-live-p org-mandala-source-buffer))
              (should (marker-buffer org-mandala-current-marker))
              (should (equal (org-mandala--heading-title org-mandala-current-marker)
                             "Root")))))
      (when (buffer-live-p view)
        (kill-buffer view))
      (let ((reopened (find-buffer-visiting file)))
        (when reopened
          (kill-buffer reopened)))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest org-mandala-render-can-use-custom-fixed-size ()
  (org-mandala-test--with-buffer org-mandala-test--fixture
    (let* ((org-mandala-fit-window nil)
           (org-mandala-svg-width 640)
           (org-mandala-svg-height 480)
           (org-mandala-current-marker (org-mandala-test--heading "Root"))
           (org-mandala-selected-position 'center)
           (image (org-mandala--render-svg))
           (data (plist-get (cdr image) :data)))
      (should (string-match-p "width=\"640\"" data))
      (should (string-match-p "height=\"480\"" data)))))

(ert-deftest org-mandala-default-svg-size-is-moderate ()
  (should (= org-mandala-svg-width 900))
  (should (= org-mandala-svg-height 650)))

(ert-deftest org-mandala-window-size-change-does-not-schedule-refresh ()
  (let ((buffer (generate-new-buffer "*mandala-test*"))
        (org-mandala-auto-refresh-on-window-size-change t)
        (org-mandala-fit-window t)
        (org-mandala-window-size-change-delay 60))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (org-mandala-view-mode)
          (setq org-mandala--last-rendered-window-size '(0 . 0))
          (org-mandala--window-size-change nil)
          (should-not (timerp org-mandala--window-size-timer)))
      (when (timerp org-mandala--window-size-timer)
        (cancel-timer org-mandala--window-size-timer))
      (setq org-mandala--window-size-timer nil)
      (kill-buffer buffer))))

(ert-deftest org-mandala-window-size-change-ignores-non-mandala-windows ()
  (let ((org-mandala-auto-refresh-on-window-size-change t)
        (org-mandala-fit-window t))
    (unwind-protect
        (progn
          (org-mandala--window-size-change nil)
          (should-not (timerp org-mandala--window-size-timer)))
      (when (timerp org-mandala--window-size-timer)
        (cancel-timer org-mandala--window-size-timer))
      (setq org-mandala--window-size-timer nil))))

(ert-deftest org-mandala-window-size-auto-refresh-defaults-off ()
  (should-not org-mandala-auto-refresh-on-window-size-change))

(ert-deftest org-mandala-fit-window-defaults-off ()
  (should-not org-mandala-fit-window))

(ert-deftest org-mandala-window-too-small-p-detects-tiny-window ()
  (let ((org-mandala-min-window-columns 36)
        (org-mandala-min-window-lines 14))
    (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
              ((symbol-function 'window-total-width) (lambda (_w) 20))
              ((symbol-function 'window-total-height) (lambda (_w) 10)))
      (should (org-mandala--window-too-small-p (selected-window))))))

(ert-deftest org-mandala-window-too-small-p-allows-large-window ()
  (let ((org-mandala-min-window-columns 36)
        (org-mandala-min-window-lines 14))
    (cl-letf (((symbol-function 'window-live-p) (lambda (_w) t))
              ((symbol-function 'window-total-width) (lambda (_w) 100))
              ((symbol-function 'window-total-height) (lambda (_w) 40)))
      (should-not (org-mandala--window-too-small-p (selected-window))))))

(ert-deftest org-mandala-window-size-hook-not-installed-by-default ()
  (should-not (memq #'org-mandala--window-size-change
                    window-size-change-functions)))

(ert-deftest org-mandala-tutorial-file-path-points-to-example ()
  (let ((path (org-mandala--tutorial-file)))
    (should (string-suffix-p "examples/mandala-example.org" path))))

(ert-deftest org-mandala-view-mode-has-center-selection-bindings ()
  (should (eq (lookup-key org-mandala-view-mode-map (kbd "c"))
              #'org-mandala-select-center))
  (should (eq (lookup-key org-mandala-view-mode-map (kbd "."))
              #'org-mandala-select-center)))

(ert-deftest org-mandala-new-refuses-existing-file ()
  (let ((file (make-temp-file "org-mandala-existing" nil ".org")))
    (unwind-protect
        (should-error (org-mandala-new file "Root") :type 'user-error)
      (delete-file file))))

(provide 'org-mandala-tests)

;;; org-mandala-tests.el ends here
