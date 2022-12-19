;;; vmacs-linux.el

;;; Commentary: Linux configuration.

;;; Code:

(if (font-exists-p "ttf-inconsolata-hellenic")
    (progn
      (setq initial-frame-alist
	    '((font . "ttf-inconsolata-hellenic-10")))
      (setq default-frame-alist
	    '((font . "ttf-inconsolata-hellenic-10"))))
  (progn
    (setq initial-frame-alist
	  '((font . "inconsolata-g-10")))
    (setq default-frame-alist
	  '((font . "inconsolata-g-10")))))

(defun client-save-kill-emacs(&optional display)
  "Save buffers and shutdown the emacs daemon. It should be
called using emacsclient -e '(client-save-kill-emacs)'. It checks
to see if there are any modified buffers, active clients or
frames. If so an X window will be opened and the user will be
prompted to proceed."
  (let (new-frame modified-buffers active-clients-or-frames)
    ;; Check if there are modified buffers, active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames ( or (> (length server-clients) 1)
                                        (> (length (frame-list)) 1)))
    ;; Create a new frame if prompts are needed.
    (when (or modified-buffers active-clients-or-frames)
      (when (not (eq window-system 'x))
        (message "Initializing x windows system.")
        (x-initialize-window-system))
      (when (not display) (setq display (getenv "DISPLAY")))
      (message "Opening frame on display: %s" display)
      (select-frame (make-frame-on-display display '((window-system . x)))))
    ;; Save the current frame.
    (setq new-frame (selected-frame))
    ;; When displaying the number of clients and frames:
    ;; - subtract 1 from the clients for this client.
    ;; - subtract 2 from the frames this frame (that we just created)
    ;;   and the default frame.
    (when ( or (not active-clients-or-frames)
               (yes-or-no-p
                (format "There are currently %d clients and %d frames. Exit anyway?"
                        (- (length server-clients) 1)
                        (- (length (frame-list)) 2))))
      ;; If the user quits during the save dialog then don't exit
      ;; emacs. Still close the terminal though.
      (let((inhibit-quit t))
        ;; Save buffers
        (with-local-quit
          (save-some-buffers))
        (if quit-flag
            (setq quit-flag nil)
          ;; Kill all remaining clients
          (progn
            (dolist (client server-clients)
              (server-delete-client client))
            ;; Exit emacs
            (kill-emacs)))))
    ;; If we made a frame then kill it.
    (when (or modified-buffers active-clients-or-frames) (delete-frame new-frame))))

(defun modified-buffers-exist()
  "Check if any buffers have been modified. Buffers that have
buffer-offer-save set to nil are ignored."
  (let (modified-found)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (buffer-modified-p buffer)
                 (not (buffer-base-buffer buffer))
                 (or
                  (buffer-file-name buffer)
                  (progn
                    (set-buffer buffer)
                    (and buffer-offer-save (> (buffer-size) 0)))))
        (setq modified-found t)))
    modified-found))

(provide 'vmacs-linux)
;;; vmacs-linux.el ends here
