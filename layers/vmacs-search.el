;;; vmacs-search.el -*- lexical-binding:t -*-

;;; Commentary: Configure grepping and finding.

;;; Code:

(use-package visual-regexp
  :straight t
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))

(provide 'vmacs-search)
;;; vmacs-search.el ends here
