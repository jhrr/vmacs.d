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

(setq gc-cons-threshold (* 1024 1024 20))
(setq max-specpdl-size 10000) ;; Needed for stream.el
(setq message-log-max 16384)
(setq package-enable-at-startup nil)

(unless noninteractive
  (message "Loading %s..." load-file-name))

(eval-and-compile
  (defvar bootstrap-version 5)
  (defvar bootstrap-file
    (expand-file-name
      "straight/repos/straight.el/bootstrap.el"
      user-emacs-directory)))

(unless (file-exists-p bootstrap-file)
  (with-current-buffer
      (url-retrieve-synchronously
        (concat "https://raw.githubusercontent.com/"
                "raxod502/straight.el/develop/install.el")
        'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))

(load bootstrap-file nil 'nomessage)

(with-no-warnings
  (setq straight-cache-autoloads t)
  (setq straight-check-for-modifications '(watch-files)))

(require 'straight bootstrap-file t)

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
; (use-package vmacs-edit)
; (use-package vmacs-company)
; (use-package vmacs-evil)
; (use-package vmacs-flycheck)
; (use-package vmacs-git)
; (use-package vmacs-helm)
(use-package vmacs-ido)
; (use-package vmacs-org)
(use-package vmacs-search)
; (use-package vmacs-snippets)
; (use-package vmacs-clojure)
; (use-package vmacs-css)
; (use-package vmacs-elm)
; (use-package vmacs-erlang)
; (use-package vmacs-haskell)
; (use-package vmacs-html)
; (use-package vmacs-js)
; (use-package vmacs-lisp)
; (use-package vmacs-python)
; (use-package vmacs-supercollider)
; (use-package vmacs-rust)
; (use-package vmacs-web)

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
