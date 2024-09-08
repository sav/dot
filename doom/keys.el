;;; $DOOMDIR/keys.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;   Custom key bindings configuration.
;;;
;;; Author: sav@tal
;;; Created: 23 Jan 2024
;;; Updated:  9 Jun 2024
;;;
;;; Code:

;; Unset keys

(global-unset-key (kbd "C-z"))

(global-unset-key (kbd "C-'"))

(global-unset-key (kbd "M-z"))

(global-unset-key (kbd "<f2>"))

(global-unset-key (kbd "<f3>"))

(global-unset-key (kbd "<f4>"))

(global-unset-key (kbd "<f5>"))

(global-unset-key (kbd "<f12>"))


;; Function Keys

(map! "S-<f1>" #'eval-buffer)

(map! "C-<f1>" #'eval-region)

(map! "<f2>" #'recompile)

(map! "S-<f2>" #'doom/reload)

(map! "<f3>" #'flycheck-buffer)

(map! "S-<f3>" #'flycheck-list-errors)

(map! "<f12>" #'toggle-frame-tab-bar)

(map! "S-<f12>" #'my/debug-on-error)


;; Mouse
;; (see https://wilkesley.org/~ian/xah/emacs/emacs_mouse_wheel_config.html)

(map! "<mouse-2>" #'clipboard-yank)


;; Prefix: C-

(map! "C-t" #'my/tab/new)

(map! "C->" #'xref-find-definitions-other-window)

(map! "C-," #'pop-tag-mark)

(map! "C-*" #'my/highlight)

(map! "C-?" #'treemacs)

(map! "C-/" #'treemacs-select-window)

(map! "C-<tab>" #'tab-next)

(map! "C-<iso-lefttab>" #'tab-previous)

(map! "C-}" #'tab-move)

(map! "C-{" (lambda () (interactive) (tab-move -1)))


;; Prefix: C-S-

(map! "C-S-a" #'other-window)

(map! "C-S-d" #'delete-window)

(map! "C-S-e" #'eshell)

(map! "C-S-h" #'split-window-vertically)

(map! "C-S-k" #'kill-current-buffer)

(map! "C-S-o" #'delete-other-windows)

(map! "C-S-p" (lambda () (interactive) (other-window -1)))

(map! "C-S-q" #'bury-buffer)

(map! "C-S-r" #'unbury-buffer)

(map! "C-S-s" #'+vterm/toggle)

(map! "C-S-t" #'tab-undo)

(map! "C-S-v" #'split-window-horizontally)

(map! "C-S-w" #'tab-bar-close-tab)


;; Prefix: C-M-

(map! "C-M-/" (lambda () (interactive) (treemacs-select-window) (delete-window)))

(map! "C-M-?" #'treemacs-select-directory)


;; Prefix: C-M-S-

(map! "C-M-S-c" #'switch-to-minibuffer)


;; Prefix: C-c

(map! "C-c F" #'toggle-frame-fullscreen)

(map! "C-c N" #'goto-line)

(map! "C-c Q" #'kill-emacs)

(map! "C-c R" #'rename-buffer)

(map! "C-c Z" #'winner-undo)

(map! "C-c w" #'clipboard-kill-ring-save)

(map! "C-c y" #'clipboard-yank)

(map! "C-c <left>" #'winner-undo)

(map! "C-c <tab>" #'+zen/toggle-fullscreen)

(map! "C-c C-<tab>" (lambda () (interactive) (writeroom-adjust-width 15)))

(map! "C-c C-<iso-lefttab>" (lambda () (interactive) (writeroom-adjust-width -15)))

(map! "C-c #" #'add-file-local-variable-prop-line)

(map! "C-c C-!" #'tramp-cleanup-all-connections)

(map! "C-c C-+" #'my/window/enlarge)

(map! "C-c C--" #'my/window/shrink)

(map! "C-c C-{" #'centaur-tabs-move-current-tab-to-left)

(map! "C-c C-}" #'centaur-tabs-move-current-tab-to-right)


;; Prefix: C-c C-

(map! "C-c C-n" #'next-buffer)

(map! "C-c C-p" #'previous-buffer)


;; Prefix: C-c C-c

(map! "C-c C-c o" #'browse-url-at-point)


;; Prefix: C-c f

(map! "C-c f d" #'fzf-directory)

(map! "C-c f F" #'fzf-find-file-in-dir)

(map! "C-c f f" #'counsel-recentf)


;; Prefix: C-c g

(map! "C-c g g" #'rg)


;; Prefix: C-c m (Mail)

(map! "C-c m R" #'mu4e-headers-mark-all-unread-read)


;; Prefix: C-c o

(map! "C-c o a" (lambda () (interactive) (my/org/open "agenda")))

(map! "C-c o b" (lambda () (interactive) (my/org/open "knowledgebase")))

(map! "C-c o c" (lambda () (interactive) (my/org/open "capture")))

(map! "C-c o h" (lambda () (interactive) (my/org/open "habits")))

(map! "C-c o i" (lambda () (interactive) (my/org/open-plain "inbox")))

(map! "C-c o j" (lambda () (interactive) (my/org/open "job")))

(map! "C-c o k" (lambda () (interactive) (my/org/open "keyring")))

(map! "C-c o l" (lambda () (interactive) (my/org/open "life")))

(map! "C-c o n" (lambda () (interactive) (my/org/open "notes")))

(map! "C-c o s" #'+doom-dashboard/open)

(map! "C-c o S" #'my/scratch/show)

(map! "C-c o t" (lambda () (interactive) (my/org/open "todo")))


;; Prefix: C-c r

(map! "C-c r l" #'consult-bookmark)

(map! "C-c r t" #'google-translate-smooth-translate)


;; Prefix: C-x

(map! "C-x B" #'consult-buffer)

(map! "C-x k" #'my/buffer/kill)

(map! "C-x K" #'my/buffer/kill-window)

(map! "C-x p" (lambda () (interactive) (other-window -1)))

(map! "C-x P" #'+popup/other)

(map! "C-x S" #'my/save-all)

(map! "C-x K" #'my/close-all)

(map! "C-x 5 5" #'other-tab-prefix)


;; Prefix: M-

(map! "M-," #'pop-tag-mark)

(map! "M-1" (lambda () (interactive) (tab-select '1)))

(map! "M-2" (lambda () (interactive) (tab-select '2)))

(map! "M-3" (lambda () (interactive) (tab-select '3)))

(map! "M-4" (lambda () (interactive) (tab-select '4)))

(map! "M-5" (lambda () (interactive) (tab-select '5)))

(map! "M-6" (lambda () (interactive) (tab-select '6)))

(map! "M-7" (lambda () (interactive) (tab-select '7)))

(map! "M-8" (lambda () (interactive) (tab-select '8)))

(map! "M-9" (lambda () (interactive) (tab-select '9)))


;; Prefix: C-z (Tab-Bar)

(map! "C-z C-c" #'my/tab/new)

(map! "C-z c" #'my/tab/new)

(map! "C-z ," #'tab-rename)

(map! "C-z C-," #'tab-rename)

(map! "C-z ." #'tab-move)

(map! "C-z C-." #'tab-move)

(map! "C-z k" #'tab-close)

(map! "C-z C-k" #'tab-close)

(map! "C-z 0" #'tab-undo)

(map! "C-z C-0" #'tab-undo)

(map! "C-z n" #'tab-next)

(map! "C-z C-n" #'tab-next)

(map! "C-z p" #'tab-previous)

(map! "C-z C-p" #'tab-previous)

(map! "C-z l" #'tab-recent)

(map! "C-z C-l" #'tab-recent)

(map! "C-z g" #'tab-group)

(map! "C-z C-g" #'tab-group)

(map! "C-z K" #'tab-close-group)

(map! "C-z C-S-k" #'tab-close-group)

(map! "C-z b" #'switch-to-buffer-other-tab)

(map! "C-z C-b" #'switch-to-buffer-other-tab)

(map! "C-z f" #'find-file-other-tab)

(map! "C-z C-f" #'find-file-other-tab)

(map! "C-z C-z" #'other-tab-prefix)

(map! "C-z 1" (lambda () (interactive) (tab-select '1)))

(map! "C-z 2" (lambda () (interactive) (tab-select '2)))

(map! "C-z 3" (lambda () (interactive) (tab-select '3)))

(map! "C-z 4" (lambda () (interactive) (tab-select '4)))

(map! "C-z 5" (lambda () (interactive) (tab-select '5)))

(map! "C-z 6" (lambda () (interactive) (tab-select '6)))

(map! "C-z 7" (lambda () (interactive) (tab-select '7)))

(map! "C-z 8" (lambda () (interactive) (tab-select '8)))

(map! "C-z 9" (lambda () (interactive) (tab-select '9)))

;;;
;;; $DOOMDIR/keys.el ends here
;;;

;;; vim:ft=lisp:ts=2:sw=2:et:
