;;; $DOOMDIR/lib.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;   Auxiliary functions.
;;;
;;; Author: sav@tal
;;; Created: 23 Jan 2024
;;; Updated:  9 Jun 2024
;;;
;;; Code:

(defun my/close-all ()
  "Close all buffers, windows and tabs."
  (interactive)
  (my/buffer/kill-all)
  (delete-other-windows)
  (tab-bar-close-other-tabs))

(defun my/save-all ()
  "Save all buffers."
  (interactive)
  (mapc 'save-buffer (buffer-list))
  (save-some-buffers)
  (message "All buffers were saved."))

(defun my/buffer/kill ()
  "Kill the current buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my/buffer/kill-window ()
  "Kill the current buffer without confirmation and delete its window."
  (interactive)
  (my/buffer/kill)
  (delete-window))

(defun my/buffer/kill-all ()
  "Close all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun my/window/enlarge (&optional delta)
  "Enlarge window by DELTA lines in both directions."
  (interactive)
  (let ((n (or delta 10)))
    (ignore-errors
      (enlarge-window n)
      (enlarge-window-horizontally n))))

(defun my/window/shrink (&optional delta)
  "Shrink window by DELTA lines in both directions."
  (interactive)
  (my/window/enlarge (* -1 (or delta 10))))

(defun my/org/open (name)
  "Open encrypted Org file by its NAME."
  (find-file (format "%s/%s.org.gpg" org-directory name)))

(defun my/org/open-plain (name)
  "Open plain-text Org file by its NAME."
  (find-file (format "%s/%s.org" org-directory name)))

(defun my/debug-on-error (&optional local)
  "Toggle the state of `debug-on-error' variable.
With prefix argument LOCAL, set variables buffer-locally."
  (interactive "P")
  (cond (local
         (setq-local debug-on-error (not debug-on-error))
         (setq-local debug-on-signal debug-on-error))
        ((setq debug-on-error (not debug-on-error))
         (setq debug-on-signal debug-on-error)))
  (message
   (format
    "%sdebug on error: %s, debug on signal %s"
    (if local "[LOCAL] " "")
    (if debug-on-error "ON" "OFF")
    (if debug-on-signal "ON" "OFF"))))

(defun my/debug-call (fn &rest args)
  (let ((debug-on-error t)
	(debug-on-signal t))
    (apply fn args)))

(defvar my/highlight/token nil)

(defun my/highlight ()
  "Highlight occurrences of the token under cursor."
  (interactive)
  (if (not (equal my/highlight/token nil))
      (unhighlight-regexp my/highlight/token))
  (if (not (string-equal my/highlight/token (current-word)))
      (and
       (setq my/highlight/token (current-word))
       (highlight-regexp (current-word) 'highlight))
    (setq my/highlight/token nil)))

(defvar my/vterm/buffer-name "*doom:vterm-popup:main*")

(defun my/vterm/focus (&optional buffer-name)
  "Focus on an existing vterm window or create a new one with BUFFER-NAME.

If BUFFER-NAME is nil, use `my/vterm/default-buffer'.
If BUFFER-NAME is not nil, attempt to find the corresponding buffer and create
a new window with it.
If neither the window nor the buffer exists, initiate a new vterm instance and
associate its buffer with BUFFER-NAME."
  (interactive)
  (let*
      ((buffer (or buffer-name my/vterm/buffer-name))
       (existing-window (get-buffer-window buffer)))
    (if existing-window (select-window existing-window)
      (if buffer
          (if (get-buffer buffer)
              (display-buffer buffer t)
            (vterm buffer))
        (+vterm/toggle)))))

(defun my/tab/new ()
  "Open a new tab and focus Doom's Dashboard."
  (interactive)
  (tab-bar-new-tab)
  (let ((buffer (get-buffer +doom-dashboard-name)))
    (if buffer (switch-to-buffer buffer)
      (+doom-dashboard/open))))

(defun my/uptime ()
  "Return the current Emacs uptime."
  (format-seconds
   "%Y, %D, %H, %M %z"
   (time-convert (time-since before-init-time) 'integer)))

(defvar my/scratch/buffer-name "*scratch*"
  "My Scratch Buffer.")

(defun my/scratch/create-buffer ()
  "Dynamically create the scratch buffer content."
  (format
   ";;; %s -- Interactive scratch -*- mode: text; -*-
;;;
;;; Commentary:
;;;   %s@%s is up for %s.
;;;
;;; Author: %s
;;; Email: %s
;;; Updated: %s
;;;
;;; Code:\n\n"
   my/scratch/buffer-name
   user-login-name
   system-name
   (my/uptime)
   user-full-name
   user-mail-address
   (format-time-string "%b %d %Y")))

(defun my/scratch/show
    (&optional delete-other-windows force-recreate)
  "Show My Scratch Buffer.
If DELETE-OTHER-WINDOWS is non-nil, delete other windows.
When argument FORCE-RECREATE is non-nill, kill current
   scratch buffer and create a new one."
  (interactive)
  (when (and force-recreate (get-buffer my/scratch/buffer-name))
    (kill-buffer my/scratch/buffer-name))
  (let*
      ((scratch-buffer-is-new (unless (get-buffer my/scratch/buffer-name) t))
       (scratch-buffer (get-buffer-create my/scratch/buffer-name)))
    (switch-to-buffer scratch-buffer)
    (with-current-buffer scratch-buffer
      (when scratch-buffer-is-new (insert (my/scratch/create-buffer)))
      ;; (lisp-interaction-mode)
      (text-mode)
      ;; (flycheck-mode -1)
      (local-unset-key (kbd "M-g"))
      (local-unset-key (kbd "C-c"))
      (local-unset-key (kbd "C-x"))
      (local-unset-key (kbd "C-z")))
    (when delete-other-windows
      (delete-other-windows))))

(defun my/alarm (time)
  "Schedule an alarm after TIME (a string like '2 hours', '30 minutes', '90 seconds')."
  (interactive "sEnter delay (e.g. 2 hours, 30 minutes, 90 seconds): ")
  (run-at-time
   time nil
   (lambda ()
     (beep)
     (play-sound-file (expand-file-name "/home/sav/mus/audio/bell.wav") 70)
     (alert "Time is over!" :title "Alarm"))))

(defun my/auth-source-flush ()
 "Flush and clear cached authentication sources.

This function ensures that Emacs forgets any cached auth-source passwords
and re-enables `epa-file-mode` to handle GPG-encrypted files."
 (require 'epa)
 (epa-file-enable)
 (save-window-excursion
   (mapc
    (lambda (f)
      (kill-buffer (find-file f)))
    auth-sources))
 (auth-source-forget+)
 (auth-source-forget-all-cached))

(defun my/auth-source-password (host &optional login once)
 "Retrieve password from auth-source for HOST and optional LOGIN.
Signals `user-error' if password not found."
 (let
     ((password (if login
                    (auth-source-pick-first-password :host host :login login)
                  (auth-source-pick-first-password :host host))))
   (unless password
     (setq password (if (not once)
                        (my/auth-source-flush)
                      (my/auth-source-password host login t))))
   (unless password
     (user-error
      "my/password-by-host: password not found: %s%s"
      (if login (format "%s@" login) "")
      host))
   password))

(defun my/byte-recompile-directory (dir)
  "Recursively `byte-compile` all `.el` files in DIR and subdirectories."
  (interactive "DDirectory: ")
  (dolist (file (directory-files-recursively dir "^.*\\.el\\(\\.gz\\)?$"))
   (message "Compiling %s" file)
   (byte-recompile-file file)))

;;;
;;; $DOOMDIR/lib.el ends here
;;;

;;; vim:ft=lisp:ts=2:sw=2:et:
