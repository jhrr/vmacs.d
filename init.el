;;; init.el -*- lexical-binding:t -*-

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

;; Induce Emacs into a state of ready-for-use; appoint load-paths, establish
;; environments and prime package layers. Kwatz!

;;; Code:

(defconst emacs-start-time (current-time))

(when (version< emacs-version "26")
  (error (concat "Emacs 26 is required. Currently running: " emacs-version)))

(setq package-enable-at-startup nil)
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold (* 1024 1024 20))
(setq frame-resize-pixelwise t)
(setq max-specpdl-size 10000) ;; Needed for stream.el
(setq message-log-max 16384)
(progn
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message (user-login-name)
        inhibit-startup-message t
        initial-scratch-buffer nil)
  (custom-set-variables '(menu-bar-mode nil)
                        '(tool-bar-mode . nil)
                        '(scroll-bar-mode nil))
  (toggle-frame-maximized)
  (modify-all-frames-parameters '((vertical-scroll-bars)
                                  (name . "Emacs")))
  (set-face-attribute 'mode-line nil :box nil))

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
  (setq use-package-verbose t))
(straight-use-package 'bind-map)
(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package)
  (require 'seq))

(use-package vmacs-paths :load-path "layers")
(use-package vmacs-theme)
(use-package vmacs-core)
(use-package vmacs-darwin :if (equal system-type 'darwin))
(use-package vmacs-linux :if (equal system-type 'gnu/linux))

; (use-package vmacs-clojure)
; (use-package vmacs-company)
; (use-package vmacs-css)
; (use-package vmacs-edit)
; (use-package vmacs-erlang)
; (use-package vmacs-evil)
; (use-package vmacs-flycheck)
(use-package vmacs-git)
; (use-package vmacs-haskell)
; (use-package vmacs-html)
(use-package vmacs-menus)
; (use-package vmacs-js)
; (use-package vmacs-lisp)
(use-package vmacs-org)
; (use-package vmacs-python)
; (use-package vmacs-rust)
(use-package vmacs-search)
; (use-package vmacs-snippets)
; (use-package vmacs-supercollider)
; (use-package vmacs-web)

(defun vmacs/layers ()
  (seq-filter
   (lambda (path) (not (string-prefix-p ".#" path)))
   (directory-files vmacs/layers t "\.el$" nil)))

(defun vmacs/layers-map ()
  "Return a hash of layer names mapped to layer paths."
  (let ((layers-hash (make-hash-table :test 'equal)))
    (seq-map (lambda (path)
               (puthash
                (file-name-sans-extension
                 (file-name-nondirectory path))
                path layers-hash))
             (vmacs/layers))
    layers-hash))

(defun vmacs/jump-to-layer ()
  "Quickly jump to a config layer."
  (interactive)
  (let ((layers-map (vmacs/layers-map)))
    (find-file
     (gethash
      (completing-read "Open layer file: " (hash-keys layers-map))
      layers-map))))
(bind-key* "C-c L" 'vmacs/jump-to-layer)

;;; --- Post-init
(progn
  (when (member (expand-file-name "~/.emacs.d") load-path)
    (setq load-path (remove (expand-file-name "~/.emacs.d") load-path)))
  (setq load-path (delete-dups load-path)))

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
