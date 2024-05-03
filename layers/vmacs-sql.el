;;; vmacs-sql.el --- Configure SQL mode. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :mode-hydra
  (sql-mode
   ("Connect"
    (("J" #'sql-postgres "postgres"))))
  :preface
  (defun configure-sql ()
    (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
    (sql-set-product-feature 'postgres :prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] "))
  :hook
  (((sql-mode sql-interactive-mode) . sqlup-mode)
   ((sql-mode sql-interactive-mode) . configure-sql)
   ((sql-mode sql-interactive-mode) . sqlind-minor-mode)))

(use-package emacs-sql-indent
  :straight
  (emacs-sql-index :type git :host github :repo "alex-hhh/emacs-sql-indent")
  :commands (sqlind-minor-mode))

(use-package sql-up-mode
  :straight
  (sql-up-mode :type git :host github :repo "Trevoke/sqlup-mode.el")
  :commands (sqlup-mode))

(provide 'vmacs-sql)
;;; vmacs-sql.el ends here
