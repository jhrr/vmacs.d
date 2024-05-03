;;; init.el --- Initialize Emacs. -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Kwatz!

;;; Code:

(defconst emacs-start-time (current-time))

(when (version< emacs-version "26")
  (error (concat "At least emacs 26 is required. Currently running: " emacs-version)))

(setq package-enable-at-startup nil
      read-process-output-max (* 1024 1024)
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.7
      garbage-collection-messages nil
      max-specpdl-size 10000 ;; Needed for stream.el
      message-log-max 16384)

(custom-set-variables
 '(menu-bar-mode . nil)
 '(tool-bar-mode . nil)
 '(scroll-bar-mode . nil))
(modify-all-frames-parameters
 '((vertical-scroll-bars) (name . "Emacs")))
(set-face-attribute 'mode-line nil :box nil)
(setq initial-frame-alist '((name . "")))
(setq frame-resize-pixelwise t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name)
      inhibit-startup-message t
      initial-scratch-buffer nil)
(toggle-frame-maximized)

(unless noninteractive
  (message "Loading %s..." load-file-name))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(with-no-warnings
  (setq use-package-verbose t)
  ;; (setq straight-use-package-by-default t)
  ;; (setq use-package-always-defer t)
)

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'seq)
  (require 'subr-x)
  (with-no-warnings
    (require 'cl-lib)))

(use-package f :straight t :commands (f-exists? f-expand f-traverse-upwards))
(use-package s :straight t :commands (s-trim))
(use-package el-patch :straight t)
(use-package anaphora :straight t)
(use-package key-chord :straight t)

(use-package vmacs-paths :load-path "layers")
(use-package vmacs-core)
(use-package vmacs-darwin :if (equal system-type 'darwin))
(use-package vmacs-linux :if (equal system-type 'gnu/linux))
(use-package vmacs-org)  ;; Needs to be loaded early to avoid org-compat bug.
(seq-doseq (feature
            (seq-reduce (lambda (acc it)
                          (let ((layer (file-name-base it))
                                (exclude-layers
                                 '("vmacs-core"
                                   "vmacs-paths"
                                   "vmacs-darwin"
                                   "vmacs-linux"
                                   "vmacs-org")))
                            (if (not (member layer exclude-layers))
                                (cons (intern layer) acc)
                              acc)))
                        (directory-el vmacs-layers) nil))
  (eval `(use-package ,feature)))

;;; --- Post-init
(progn
  (when (member (expand-file-name "~/.emacs.d") load-path)
    (setq load-path (remove (expand-file-name "~/.emacs.d") load-path)))
  (setq load-path (delete-dups load-path)))

(defvar config-default-gc-threshold (* 1024 1024 100))

(defun config--inhibit-gc ()
  "Inhibit garbage collection."
  (setq gc-cons-threshold most-positive-fixnum))

(defun config--enable-gc ()
  "Enable garbage collection."
  (setq gc-cons-threshold config-default-gc-threshold
        garbage-collection-messages t))

(add-hook 'minibuffer-setup-hook #'config--inhibit-gc)
(add-hook 'minibuffer-exit-hook #'config--enable-gc)
(add-hook 'after-init-hook #'config--enable-gc)

(when window-system
  (let ((elapsed (float-time (time-subtract
                              (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract
                                           (current-time) emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
;;; init.el ends here
;; Local Variables:
;; no-byte-compile: t
;; End:
