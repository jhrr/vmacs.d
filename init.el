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

;; Invoke, entice and massage Emacs to be in the state we desire once
;; it has booted up and is ready for use; appoint load-paths,
;; establish environment basics and prime package layers. Kwatz!

;;; Code:

(defconst emacs-start-time (current-time))
(setq gc-cons-threshold (* 1024 1024 20))
(setq max-specpdl-size 10000) ;; Needed for stream.el
(setq message-log-max 16384)

(when (version< emacs-version "26")
  (error "This version of Emacs is not supported"))

(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq package-enable-at-startup nil)

(require 'seq)
(seq-map
  (lambda (path)
    (setq load-path
          (cons (expand-file-name path user-emacs-directory) load-path)))
  '("layers" "lisp"))

; (use-package vmacs/theme)
; (use-package vmacs/core)
; (use-package vmacs/darwin :if (equal system-type 'darwin))
; (use-package vmacs/linux :if (equal system-type 'gnu/linux))
; (use-package vmacs/evil)
; (use-package vmacs/edit)
; (use-package vmacs/helm)
; (use-package vmacs/ido)
; (use-package vmacs/org)
; (use-package vmacs/git)
; (use-package vmacs/clojure)
; (use-package vmacs/company)
; (use-package vmacs/css)
; (use-package vmacs/haskell)
; (use-package vmacs/html)
; (use-package vmacs/js)
; (use-package vmacs/lisp)
; (use-package vmacs/python)
; (use-package vmacs/rust)

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
