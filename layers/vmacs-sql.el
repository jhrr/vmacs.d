;;; vmacs-sql.el --- Configure SQL mode. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :mode-hydra
  (sql-mode
   ("Connect"
    (("p" #'sql-postgres "postgres")
     ("u" #'sqlup-capitalize-keywords-in-region "capitalize region keywords"))))
  :init
  (add-hook 'sql-mode-hook #'sqlind-minor-mode)
  (add-hook 'sql-interactive-mode-hook #'sqlind-minor-mode)
  (add-hook 'sql-mode-hook #'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook #'sqlup-mode)
  :config
  (sql-set-product-feature
   'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
  (sql-set-product-feature
   'postgres :prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] "))

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
