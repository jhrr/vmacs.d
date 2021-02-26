;;; vmacs-sql.el --- SQL mode. -*- lexical-binding:t -*-

;;; Commentary:

;;; Code:

(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :mode-hydra
  (sql-mode
   ("Connect"
    (("p" #'sql-postgres "postgres"))))
  :config
  (use-package sql-up-mode
    :load-path lisp
    :commands sqlup-mode
    :straight
    (sql-up-mode
     :type git
     :host github
     :repo "Trevoke/sqlup-mode.el")
    :config
    (bind-key "C-. u" #'sqlup-capitalize-keywords-in-region sql-mode-hook))

  (add-hook 'sql-mode-hook #'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook #'sqlup-mode)

  (sql-set-product-feature
   'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
  (sql-set-product-feature
   'postgres :prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] "))

(provide 'vmacs-sql)
;;; vmacs-sql.el ends here
