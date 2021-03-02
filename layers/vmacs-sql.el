;;; vmacs-sql.el --- SQL mode. -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :mode-hydra
  (sql-mode
   ("Connect"
    (("p" #'sql-postgres "postgres")
     ("u" #'sqlup-capitalize-keywords-in-region "capitalize region keywords"))))
  :config
  (use-package sql-up-mode
    :straight
    (sql-up-mode :type git :host github :repo "Trevoke/sqlup-mode.el"))

  (add-hook 'sql-mode-hook #'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook #'sqlup-mode)

  (sql-set-product-feature
   'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
  (sql-set-product-feature
   'postgres :prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] "))

(provide 'vmacs-sql)
;;; vmacs-sql.el ends here
