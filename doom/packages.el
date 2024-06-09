;;; $DOOMDIR/packages.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;   Import external dependencies.
;;;
;;; Author: sav@tal
;;; Created: 23 Jan 2024
;;;
;;; Code:

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

(package! ag)
(package! alert)
(package! all-the-icons-ivy)
(package! bookmark+)
(package! cargo-mode)
(package! counsel)
(package! counsel-notmuch)
(package! counsel-projectile)
(package! counsel-web)
(package! eat)
(package! elpher)
(package! fzf)
(package! gdb-mi)
(package! go-errcheck)
(package! go-imports)
(package! go-mode)
(package! golint)
(package! google-translate)
(package! i3wm)
(package! i3wm-config-mode)
(package! info+)
(package! ivy)
(package! lsp-ui)
(package! mpdmacs)
(package! mutt-mode)
(package! notifications)
(package! oj)
(package! org-notify)
(package! org-superstar)
(package! org-tag-beautify)
(package! parinfer)
(package! pinentry)
(package! pushbullet)
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
(package! whitespace)
(package! woman)
(package! yaml-mode)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")

;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

;;;
;;; $DOOMDIR/packages.el ends here
;;;

;;; vim:ft=lisp:ts=2:sw=2:et:
