;;; vmacs-terraform.el --- Configure Terraform. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package terraform-mode
  :straight t
  :mode (rx ".tf" (? "vars") eos)
  :hook
  (terraform-mode . terraform-format-on-save-mode)
  :custom
  (terraform-indent-level 2))

(use-package hcl-mode
  :straight t
  :mode (rx ".hcl" eos)
  :custom (hcl-indent-level 2))

(provide 'vmacs-terraform)
;;; vmacs-terraform.el ends here
