;;; tox-pytest.el --- Run pytest via tox.

;; Copyright (C) 2021 jhrr

;; Author: jhrr@users.noreply.github.com
;; Homepage: https://github.com/jhrr/tox-pytest.el
;; Version: 0.1
;; Keywords: python, py.test, tox, unittests

;;; Installation:

;;; Commentary:

;;; License:

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; TODO: prefix arguments, &optional askenv
;; TODO: region -> tox -- do/it/like/this.py::test_x do/it/like/this.py::test_y

(defconst tox-pytest--tox-command "tox"
  "How tox is called.")

(defconst tox-pytest--separator "::"
  "The pytest `path.module::test' separator.")

(defvar tox-pytest--prompt-for-env-p t
  "If we should prompt for a tox env selection before calling tox.")

(defun tox-pytest--locate-tox-file ()
  "Return the absolute path of `tox.ini' relative to the current file.
Returns nil if one cannot be found."
  (ignore))

(defun tox-pyenv--prompt-for-env ()
  "Prompt for a tox env.
If there is only one env available then silently use it."
  (ignore))

(defun tox-pytest--prompt-and-save-env ()
  "Prompt for a tox env and remember the choice."
  (ignore))

(defun tox-pytest--parse-envs ()
  "Read all available environments from `tox.ini'."
  (ignore))

;; ###autoload
(defun tox-pytest ()
  "Run the test-suite via tox."
  (interactive)
  (ignore))

;;;###autoload
(defun tox-pytest--module ()
  "Run all unnittests in the current file via tox."
  (interactive)
  (ignore))

;;;###autoload
(defun tox-pytest--region ()
  "Run the unittests contained under a region via tox."
  (interactive)
  (if (use-region-p)
      (ignore)
    (message "tox-pytest: No active region")))

;;;###autoload
(defun tox-pytest--at-point ()
  "Run the unittest under the cursor via tox.
If the cursor is anywhere in the body of a test function, try and work
out what that test is. If the cursor is in or on a method in a class,
just run that method. If the cursor is on a class, or in the body of a
class somehow in a way where it's not obvious how to resolve it to a
particular method, run that whole class of tests. Basically, try and
do the intuitive thing."
  (interactive)
  (ignore))

(provide 'tox-pytest)
;;; tox-pytest.el ends here
