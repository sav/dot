;;; $DOOMDIR/packages.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;   Install packages.
;;;
;;; Author: sav@tal
;;; Created: 23 Jan 2024
;;; Updated:  9 Jun 2024
;;;
;;; Code:

(package! ag)
(package! alert)
(package! all-the-icons-ivy)
(package! bookmark+)
(package! cargo-mode)
(package! counsel)
(package! counsel-notmuch)
(package! counsel-projectile)
(package! counsel-web)
(package! dash)
(package! eat)
(package! elpher)
(package! flycheck)
(package! fzf)
(package! gcmh)
(package! gdb-mi)
(package! go-errcheck)
(package! go-imports)
(package! go-mode)
(package! golint)
(package! google-translate)
(package! hover)
(package! i3wm)
(package! i3wm-config-mode)
(package! info+)
(package! ivy)
(package! lean4-mode
  :recipe (:host github
           :repo "leanprover-community/lean4-mode"
           :files ("*.el" "data")))
(package! lsp-dart)
(package! lsp-ui)
(package! lsp-treemacs)
(package! magit)
(package! mpdmacs)
(package! mutt-mode)
(package! notifications)
(package! oj)
(package! org-gcal)
(package! org-notify)
(package! org-superstar)
(package! org-tag-beautify)
(package! parinfer)
(package! pinentry)
(package! pushbullet
  :recipe (:host github
           :repo "sav/emacs-pushbullet"))
(package! rainbow-delimiters)
(package! rainbow-mode)
(package! rg)
(package! rust-mode)
(package! shfmt)
(package! tab-bar)
(package! telega)
(package! tramp)
(package! treemacs-all-the-icons)
(package! uptimes)
(package! vimrc-mode)
(package! winner)
(package! whitespace)
(package! yaml-mode)
(package! xcscope)

;; Tentative
;; (package! copilot)
;; (package! gptel)
;; (package! flutter)
;; (package! dart-mode)

;;;
;;; $DOOMDIR/packages.el ends here
;;;

;;; vim:ft=lisp:ts=2:sw=2:et:
