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

(global-set-key (kbd "S-<f1>") #'eval-buffer)

(global-set-key (kbd "C-<f1>") #'eval-region)

(global-set-key (kbd "<f2>") #'recompile)

(global-set-key (kbd "S-<f2>") #'doom/reload)

(global-set-key (kbd "<f3>") #'flycheck-buffer)

(global-set-key (kbd "S-<f3>") #'flycheck-list-errors)

(global-set-key (kbd "<f12>") #'toggle-frame-tab-bar)

(global-set-key (kbd "S-<f12>") #'my/debug-on-error)


;; Mouse
;; (see https://wilkesley.org/~ian/xah/emacs/emacs_mouse_wheel_config.html)

(global-set-key (kbd "<mouse-2>") #'clipboard-yank)


;; Prefix: C-

(global-set-key (kbd "C-t") #'my/tab/new)

(global-set-key (kbd "C->") #'xref-find-definitions-other-window)

(global-set-key (kbd "C-,") #'pop-tag-mark)

(global-set-key (kbd "C-*") #'my/highlight)

(global-set-key (kbd "C-?") #'eldoc-doc-buffer)

(global-set-key (kbd "C-/") #'treemacs-select-window)

(global-set-key (kbd "C-<tab>") #'tab-next)

(global-set-key (kbd "C-<iso-lefttab>") #'tab-previous)

(global-set-key (kbd "C-}") #'tab-move)

(global-set-key (kbd "C-{") (lambda () (interactive) (tab-move -1)))


;; Prefix: C-S-

(global-set-key (kbd "C-S-a") #'other-window)

(global-set-key (kbd "C-S-d") #'delete-window)

(global-set-key (kbd "C-S-e") #'eshell)

(global-set-key (kbd "C-S-h") #'split-window-vertically)

(global-set-key (kbd "C-S-k") #'kill-current-buffer)

(global-set-key (kbd "C-S-o") #'delete-other-windows)

(global-set-key (kbd "C-S-p") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-S-q") #'bury-buffer)

(global-set-key (kbd "C-S-r") #'unbury-buffer)

(global-set-key (kbd "C-S-s") #'+vterm/toggle)

(global-set-key (kbd "C-S-t") #'tab-undo)

(global-set-key (kbd "C-S-v") #'split-window-horizontally)

(global-set-key (kbd "C-S-w") #'tab-bar-close-tab)


;; Prefix: C-M-

(global-set-key (kbd "C-M-/") (lambda () (interactive) (treemacs-select-window) (delete-window)))

(global-set-key (kbd "C-M-?") #'treemacs-select-directory)


;; Prefix: C-M-S-

(global-set-key (kbd "C-M-S-c") #'switch-to-minibuffer)


;; Prefix: C-c

(global-set-key (kbd "C-c F") #'toggle-frame-fullscreen)

(global-set-key (kbd "C-c N") #'goto-line)

(global-set-key (kbd "C-c Q") #'kill-emacs)

(global-set-key (kbd "C-c R") #'rename-buffer)

(global-set-key (kbd "C-c Z") #'winner-undo)

(global-set-key (kbd "C-c w") #'clipboard-kill-ring-save)

(global-set-key (kbd "C-c y") #'clipboard-yank)

(global-set-key (kbd "C-c <left>") #'winner-undo)

(global-set-key (kbd "C-c <tab>") #'+zen/toggle-fullscreen)

(global-set-key (kbd "C-c C-<tab>") (lambda () (interactive) (writeroom-adjust-width 15)))

(global-set-key (kbd "C-c C-<iso-lefttab>") (lambda () (interactive) (writeroom-adjust-width -15)))

(global-set-key (kbd "C-c #") #'add-file-local-variable-prop-line)

(global-set-key (kbd "C-c C-!") #'tramp-cleanup-all-connections)

(global-set-key (kbd "C-c C-+") #'my/window/enlarge)

(global-set-key (kbd "C-c C--") #'my/window/shrink)

(global-set-key (kbd "C-c C-{") #'centaur-tabs-move-current-tab-to-left)

(global-set-key (kbd "C-c C-}") #'centaur-tabs-move-current-tab-to-right)


;; Prefix: C-c C-

(global-set-key (kbd "C-c C-n") #'next-buffer)

(global-set-key (kbd "C-c C-p") #'previous-buffer)


;; Prefix: C-c C-c

(global-set-key (kbd "C-c C-c o") #'browse-url-at-point)


;; Prefix: C-c f

(global-set-key (kbd "C-c f d") #'fzf-directory)

(global-set-key (kbd "C-c f F") #'fzf-find-file-in-dir)

(global-set-key (kbd "C-c f f") #'counsel-recentf)


;; Prefix: C-c g

(global-set-key (kbd "C-c g g") #'rg)


;; Prefix: C-c m (Mail)

(global-set-key (kbd "C-c m R") #'mu4e-headers-mark-all-unread-read)


;; Prefix: C-c o

(global-set-key (kbd "C-c o a") (lambda () (interactive) (my/org/open "agenda")))

(global-set-key (kbd "C-c o b") (lambda () (interactive) (my/org/open "knowledgebase")))

(global-set-key (kbd "C-c o c") (lambda () (interactive) (my/org/open "capture")))

(global-set-key (kbd "C-c o h") (lambda () (interactive) (my/org/open "habits")))

(global-set-key (kbd "C-c o i") (lambda () (interactive) (my/org/open-plain "inbox")))

(global-set-key (kbd "C-c o k") (lambda () (interactive) (my/org/open "keyring")))

(global-set-key (kbd "C-c o l") (lambda () (interactive) (my/org/open "life")))

(global-set-key (kbd "C-c o n") (lambda () (interactive) (my/org/open "notes")))

(global-set-key (kbd "C-c o s") #'+doom-dashboard/open)

(global-set-key (kbd "C-c o S") #'my/scratch/show)

(global-set-key (kbd "C-c o t") (lambda () (interactive) (my/org/open "todo")))


;; Prefix: C-c r

(global-set-key (kbd "C-c r l") #'consult-bookmark)

(global-set-key (kbd "C-c r t") #'google-translate-smooth-translate)


;; Prefix: C-x

(global-set-key (kbd "C-x B") #'consult-buffer)

(global-set-key (kbd "C-x k") #'my/buffer/kill)

(global-set-key (kbd "C-x K") #'my/buffer/kill-window)

(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-x P") #'+popup/other)

(global-set-key (kbd "C-x S") #'my/save-all)

(global-set-key (kbd "C-x K") #'my/close-all)

(global-set-key (kbd "C-x 5 5") #'other-tab-prefix)


;; Prefix: M-

(global-set-key (kbd "M-1") (lambda () (interactive) (tab-select '1)))

(global-set-key (kbd "M-2") (lambda () (interactive) (tab-select '2)))

(global-set-key (kbd "M-3") (lambda () (interactive) (tab-select '3)))

(global-set-key (kbd "M-4") (lambda () (interactive) (tab-select '4)))

(global-set-key (kbd "M-5") (lambda () (interactive) (tab-select '5)))

(global-set-key (kbd "M-6") (lambda () (interactive) (tab-select '6)))

(global-set-key (kbd "M-7") (lambda () (interactive) (tab-select '7)))

(global-set-key (kbd "M-8") (lambda () (interactive) (tab-select '8)))

(global-set-key (kbd "M-9") (lambda () (interactive) (tab-select '9)))


;; Prefix: C-z (Tab-Bar)

(global-set-key (kbd "C-z C-c") #'my/tab/new)

(global-set-key (kbd "C-z c") #'my/tab/new)

(global-set-key (kbd "C-z ,") #'tab-rename)

(global-set-key (kbd "C-z C-,") #'tab-rename)

(global-set-key (kbd "C-z .") #'tab-move)

(global-set-key (kbd "C-z C-.") #'tab-move)

(global-set-key (kbd "C-z k") #'tab-close)

(global-set-key (kbd "C-z C-k") #'tab-close)

(global-set-key (kbd "C-z 0") #'tab-undo)

(global-set-key (kbd "C-z C-0") #'tab-undo)

(global-set-key (kbd "C-z n") #'tab-next)

(global-set-key (kbd "C-z C-n") #'tab-next)

(global-set-key (kbd "C-z p") #'tab-previous)

(global-set-key (kbd "C-z C-p") #'tab-previous)

(global-set-key (kbd "C-z l") #'tab-recent)

(global-set-key (kbd "C-z C-l") #'tab-recent)

(global-set-key (kbd "C-z g") #'tab-group)

(global-set-key (kbd "C-z C-g") #'tab-group)

(global-set-key (kbd "C-z K") #'tab-close-group)

(global-set-key (kbd "C-z C-S-k") #'tab-close-group)

(global-set-key (kbd "C-z b") #'switch-to-buffer-other-tab)

(global-set-key (kbd "C-z C-b") #'switch-to-buffer-other-tab)

(global-set-key (kbd "C-z f") #'find-file-other-tab)

(global-set-key (kbd "C-z C-f") #'find-file-other-tab)

(global-set-key (kbd "C-z C-z") #'other-tab-prefix)

(global-set-key (kbd "C-z 1") (lambda () (interactive) (tab-select '1)))

(global-set-key (kbd "C-z 2") (lambda () (interactive) (tab-select '2)))

(global-set-key (kbd "C-z 3") (lambda () (interactive) (tab-select '3)))

(global-set-key (kbd "C-z 4") (lambda () (interactive) (tab-select '4)))

(global-set-key (kbd "C-z 5") (lambda () (interactive) (tab-select '5)))

(global-set-key (kbd "C-z 6") (lambda () (interactive) (tab-select '6)))

(global-set-key (kbd "C-z 7") (lambda () (interactive) (tab-select '7)))

(global-set-key (kbd "C-z 8") (lambda () (interactive) (tab-select '8)))

(global-set-key (kbd "C-z 9") (lambda () (interactive) (tab-select '9)))

;;;
;;; $DOOMDIR/keys.el ends here
;;;

;;; vim:ft=lisp:ts=2:sw=2:et:
