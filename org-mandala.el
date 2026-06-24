;;; org-mandala.el --- Compatibility alias for mandal-org -*- lexical-binding: t; -*-

;;; Commentary:

;; The package was renamed from `org-mandala' to `mandal-org'.  This file
;; loads the new implementation, provides the old feature name, and aliases
;; the public commands so existing configurations keep working.

;;; Code:

(require 'mandal-org)

(dolist (pair '((org-mandala-view            . mandal-org-view)
                (org-mandala-new             . mandal-org-new)
                (org-mandala-open-or-create  . mandal-org-open-or-create)
                (org-mandala-tutorial        . mandal-org-tutorial)
                (org-mandala-refresh         . mandal-org-refresh)
                (org-mandala-quit            . mandal-org-quit)
                (org-mandala-repair-drawers  . mandal-org-repair-drawers)
                (org-mandala-edit-finish     . mandal-org-edit-finish)
                (org-mandala-edit-cancel     . mandal-org-edit-cancel)))
  (defalias (car pair) (cdr pair)))

(provide 'org-mandala)

;;; org-mandala.el ends here
