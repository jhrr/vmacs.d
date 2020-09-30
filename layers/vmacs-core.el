;;; vmacs-core.el -*- lexical-binding:t -*-

;;; Commentary: Core environment configuration, funs and bindings.

;;; Code:

;;; --- Custom key-maps
;; Main keymap prefixes are:
;;
;;   C-x <keys> -- primary map
;;   C-c <keys> -- secondary map
;;   C-. <keys> -- tertiary map
;;   C-h <keys> -- help map
;;   C-; <keys> -- helm map

;; "C-. r" brings up a mode-appropriate repl/interpreter, if one is available.
;; So ielm for elisp modes, slime for common lisp modes, ghci when in
;; haskell-mode, and on on and so forth.  Likewise "C-. c" will try to run a
;; mode specific compilation or evaluation process.

;; TODO: work a out consistent info/help lookup system
;; TODO: parallel evil bindings

(defvar ctrl-period-map)
(define-prefix-command 'ctrl-period-map)
(bind-key "C-." 'ctrl-period-map)

(defvar ctrl-semicolon-map)
(define-prefix-command 'ctrl-semicolon-map)
(bind-key "C-;" 'ctrl-semicolon-map)

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name)
      inhibit-startup-message t
      initial-scratch-buffer nil)

(set-face-attribute 'mode-line nil :box nil)

(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(if (display-graphic-p)
    (set-fontset-font
     (frame-parameter nil 'font)
     'japanese-jisx0208 '("VL Gothic" . "unicode-bmp")))

(defalias 'yes-or-no-p 'y-or-n-p)

(subword-mode t)
(show-paren-mode t)
(blink-cursor-mode t)
(global-linum-mode t)
(global-hl-line-mode t)
(auto-compression-mode t)
(delete-selection-mode t) ;; overwrite marked text
(global-auto-revert-mode t)
(mouse-avoidance-mode 'jump)

(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)
(setq-default ispell-dictionary "en_GB")

(setq browse-url-generic-program (executable-find "chromium")
      browse-url-browser-function 'browse-url-generic)

(setq tab-width 4)
(setq standard-indent 4)
(setq create-lockfiles nil)
(setq vc-follow-symlinks t)
(setq mouse-yank-at-point t)
(setq transient-mark-mode t)
(setq inhibit-default-init t)
(setq sentence-end-double-space nil)
(setq disabled-command-function nil)
(setq scroll-preserve-screen-position t)
(setq initial-major-mode 'fundamental-mode)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq doc-view-continuous t
      doc-view-resolution 300)

(setq compile-command "CC=clang make -k")

(bind-key* "<C-return>" #'other-window)
(bind-key* "<C-right>" #'next-buffer)
(bind-key* "<C-left>" #'previous-buffer)

(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x C-k" #'kill-region) ;; We lose edit-kbd-macro with this.
(bind-key "C-c C-k" #'kill-region) ;; Catch typos.
(bind-key "<s-return>" #'toggle-frame-fullscreen)
(bind-key "C-c I"
  (lambda ()
    (interactive)
    (dired-other-window user-emacs-directory)))

(defface dim-parens-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to tastefully dim parentheses.")

(defmacro hook-into-modes (func mode-hooks)
  "Add a FUNC to multiple MODE-HOOKS simultaneously."
  `(dolist (mode-hook ,mode-hooks)
     (add-hook mode-hook ,func)))

;; M-S-? combos kind of annoy me...
(bind-key "C-c !" #'shell-command)
(bind-key "C-c &" #'async-shell-command)

;; TODO: Use ~ up/down case when evil mode is up.
(bind-key "C-c U" #'capitalize-region)

(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'. Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook #'dubcaps-mode)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(bind-key "C-c D B" #'delete-file-and-buffer)

(defun font-exists-p (font)
  "Determine if a particular FONT is available on your system."
  (if (null (find-font (font-spec :name font)))
      nil
    t))

(defun font-candidate (&rest fonts)
  "Return the first available font."
  (seq-find #'font-exists-p fonts))

(defun google ()
  "Google the selected region, if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

(bind-key "C-c G" #'google)
(bind-key "C-c l" #'list-packages)

(defun insert-guid ()
  "Insert a GUID at point."
  (interactive "*")
  (let ((uuid (trim (shell-command-to-string "uuidgen"))))
    (insert uuid)))

(defun nuke-all ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (seq-map #'kill-buffer (buffer-list))
  (delete-other-windows))

(bind-key "C-c n a" #'nuke-all)

(defun nuke-others ()
  "Kill all other buffers."
  (interactive)
  (seq-map #'kill-buffer
           (delq (current-buffer)
                 (remove-if-not 'buffer-file-name (buffer-list)))))

(bind-key "C-c n o" #'nuke-others)

(defun quickping (host)
  "Ping a HOST once and return success or failure."
  (= 0 (call-process "/sbin/ping" nil nil nil "-c1" "-W50" "-q" host)))

(defun scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (if (memq current-mode lisp-modes)
        (funcall current-mode))))

(defun trim-left (s)
  "Remove whitespace at the beginning of S."
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (if (string-match "\\`[ \t\n\r]+" s)
        (replace-match "" t t s)
      s)))

(defun trim-right (s)
  "Remove whitespace at the end of S."
  (save-match-data
    (declare (pure t) (side-effect-free t))
    (if (string-match "[ \t\n\r]+\\'" s)
        (replace-match "" t t s)
      s)))

(defun trim (s)
  "Remove whitespace at the beginning and end of S."
  (declare (pure t) (side-effect-free t))
  (trim-left (trim-right s)))

(defun what-face (pos)
  "Get the font-face at cursor POS."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(bind-key "C-c w f" #'what-face)

(defun win-swap (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(bind-key "C-x 4" #'win-swap)

(defun view-clipboard ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))
    (html-mode)
    (view-mode)))

(bind-key "C-c V" #'view-clipboard)

(provide 'vmacs-core)
;;; vmacs-core.el ends here
