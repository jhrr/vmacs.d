;;; vmacs-core.el -*- lexical-binding:t -*-

;;; Commentary: Core editor configuration.

;;; Code:

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(if (display-graphic-p)
    (set-fontset-font
     (frame-parameter nil 'font)
     'japanese-jisx0208 '("VL Gothic" . "unicode-bmp")))

(setq tabs-width 4)
(setq standard-indent 4)
(setq use-dialog-box nil)
(setq create-lockfiles nil)
(setq vc-follow-symlinks t)
(setq mouse-yank-at-point t)
(setq transient-mark-mode t)
(setq inhibit-default-init t)
(setq doc-view-continuous t
      doc-view-resolution 300)
(setq sentence-end-double-space nil)
(setq disabled-command-function nil)
(setq scroll-preserve-screen-position t)
(setq compile-command "CC=clang make -k")
(setq initial-major-mode 'fundamental-mode)
(setq base16-theme-256-color-source "base16-shell")
(setq browse-url-generic-program (executable-find "chromium")
      browse-url-browser-function 'browse-url-generic)

(setq-default line-number-mode t)
(setq-default column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)
(setq-default ispell-dictionary "en_GB")

(subword-mode t)
(show-paren-mode t)
(blink-cursor-mode t)
(global-linum-mode t)
(global-hl-line-mode t)
(auto-compression-mode t)
(delete-selection-mode t) ;; overwrite marked text
(global-auto-revert-mode t)
(mouse-avoidance-mode 'jump)

(recentf-mode)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

(defalias 'yes-or-no-p 'y-or-n-p)

(defmacro hook-into-modes (func mode-hooks)
  "Add a FUNC to multiple MODE-HOOKS simultaneously."
  `(dolist (mode-hook ,mode-hooks)
     (add-hook mode-hook ,func)))

(defvar ctrl-period-map)
(define-prefix-command 'ctrl-period-map)
(bind-key "C-." 'ctrl-period-map)

(defface dim-parens-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to tastefully dim parentheses.")

(add-hook 'before-save-hook 'whitespace-cleanup)

(defun jump-to-init ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))
(bind-key* "C-c I" 'jump-to-init)

(defun layers ()
  "List all layer files."
  (seq-filter
   (lambda (path) (not (string-prefix-p ".#" path)))
   (directory-files layers t "\.el$" nil)))

(defun layers-map ()
  "Return a hash of layer names mapped to layer paths."
  (let ((layers-hash (make-hash-table :test 'equal)))
    (seq-map (lambda (path)
               (puthash
                (file-name-sans-extension
                 (file-name-nondirectory path))
                path layers-hash))
             (layers))
    layers-hash))

(defun jump-to-layer ()
  "Quickly jump to a config layer."
  (interactive)
  (let ((layers-map (layers-map)))
    (find-file
     (gethash
      (completing-read "Open layer file: " (hash-keys layers-map))
      layers-map))))
(bind-key* "C-c L" 'jump-to-layer)

;; TODO: Move all to lisp/utility-funcs.el
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
  "Return the first available font from FONTS."
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

(defun insert-guid ()
  "Insert a GUID at point."
  (interactive "*")
  (let ((uuid (s-trim (shell-command-to-string "uuidgen"))))
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
  "Jump to the scratch buffer."
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

(defun under-vc-p ()
  "Determine if we are currently under version control."
  (magit-git-repo-p (or (vc-root-dir) "")))

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

;; TODO: Replace all this with Evil & Hydras.
(bind-key* "<C-return>" #'other-window)
(bind-key* "<C-right>" #'next-buffer)
(bind-key* "<C-left>" #'previous-buffer)

(bind-key "C-c !" #'shell-command)
(bind-key "C-c &" #'async-shell-command)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x C-k" #'kill-region) ;; We lose edit-kbd-macro with this.
(bind-key "C-c C-k" #'kill-region) ;; Catch typos.
(bind-key "<s-return>" #'toggle-frame-fullscreen)

(provide 'vmacs-core)
;;; vmacs-core.el ends here
