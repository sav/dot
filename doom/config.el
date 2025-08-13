;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;  Place your private configuration here! Remember, you do not need to run 'doom
;;;  sync' after modifying this file!
;;;
;;; Author: sav@tal
;;; Created: 23 Jan 2024
;;; Updated:  9 Jun 2024
;;;
;;; Code:

(load! "vars.el")
(load! "lib.el")
(load! "keys.el")
(load! "contrib.el")

(load! "~/.doom.el")

(setq
 doom-theme
 (cond
  ((eq doom-theme-style 'dark) 'seti)
  ((eq doom-theme-style 'light) 'doom-nord-light)))

(setq
 doom-font (font-spec :family doom-font-family :size doom-font-size :weight doom-font-weight)
 doom-big-font (font-spec :family doom-font-family :size (+ doom-font-size 2) :weight doom-font-weight)
 doom-serif-font (font-spec :family doom-serif-font-family :size doom-font-size :weight doom-font-weight)
 doom-symbol-font (font-spec :family doom-symbol-font-family :size doom-font-size :weight doom-font-weight)
 doom-variable-pitch-font (font-spec :family doom-variable-pitch-font-family :size (+ doom-font-size 4) :weight doom-font-weight)
 doom-modeline-icon t
 doom-modeline-major-mode-icon t
 doom-modeline-lsp-icon t
 doom-modeline-major-mode-color-icon t
 +zen-text-scale 0)

(setq
 user-full-name "Savio Sena"
 user-mail-address "savio.sena@gmail.com"
 confirm-kill-processes nil
 confirm-kill-emacs nil
 line-number-mode t
 column-number-mode t
 cursor-in-non-selected-windows 'hollow
 display-line-numbers-type nil
 global-mark-ring-max 64
 kill-buffer-query-functions nil
 kill-emacs-query-functions nil
 org-directory (expand-file-name "~/org/")
 pop-up-frames nil
 save-abbrevs nil
 set-mark-command-repeat-pop t
 delete-trailing-lines nil
 show-trailing-whitespace t
 tab-always-indent t
 vc-follow-symlinks t
 prettify-symbols-alist '(("->" . ?→))
 frame-title-format '("%n %b  --  %F")
 focus-in-hook nil
 focus-out-hook nil)

(setq-default
 tab-width 8
 evil-shift-width 8
 indent-tabs-mode t)

(blink-cursor-mode)
(doom-modeline-mode)

;;
;; Packages Configuration
;;

(use-package! alert
  :config
  (setq alert-default-style 'notifications))

(use-package! all-the-icons-ivy
  :config
  (add-hook 'after-init-hook #'all-the-icons-ivy-setup))

(use-package! auth-source
  :config
  (require 'epa)
  (epa-file-enable)
  (setq authinfo-hide-elements nil)
  (setq auth-sources '("~/.authinfo.gpg")))

(use-package! bookmark
  :config
  (setq
   bookmark-bmenu-toggle-filenames nil
   bookmark-default-file (expand-file-name "~/.emacs-bookmarks.el")
   bookmark-menu-confirm-deletion t
   bookmark-menu-length 120
   bookmark-sort-flag t))

(after! browse-url
  (setq
   browse-url-browser-function #'browse-url-default-browser
   browse-url-chrome-program "my.xsel-browser"
   browse-url-chromium-program "my.xsel-browser"))

(after! centaur-tabs
  (setq
   centaur-tabs-adjust-buffer-order nil
   centaur-tabs-cycle-scope 'tabs
   centaur-tabs-enable-key-bindings t
   centaur-tabs-set-bar 'under)
  (defun centaur-tabs-buffer-groups ()
    (list
     (cond
      ((string-equal "*" (substring (buffer-name) 0 1)) "Emacs")
      ((derived-mode-p 'prog-mode) "Editing")
      ((derived-mode-p 'dired-mode) "Dired")
      ((memq major-mode '(helpful-mode help-mode)) "Help")
      (t "Main"))))
  (centaur-tabs-mode))

(use-package! company
  :demand t
  :config
  (global-company-mode))

(after! counsel
  (require 'counsel-web)
  (setq counsel-search-engine 'google)
  (setq counsel-web-search-action 'eww))

(after! dired
  (add-hook 'dired-mode-hook (lambda () (local-unset-key (kbd "C-t"))))
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))))

(after! editorconfig
  (require 'loadhist)
  (require 'hi-lock))

(use-package! elfeed
  :config
  (setq
   elfeed-search-filter "@6-weeks-ago"
   elfeed-search-title-max-width 90
   elfeed-search-title-min-width 40
   elfeed-summary-width 140
   elfeed-feeds
   '("https://without.boats/index.xml"
     "https://smallcultfollowing.com/babysteps/index.xml"
     "https://lord.io/feed.xml"
     "https://rust-embedded.github.io/blog/rss.xml"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCP5sC22d-Z5B53VjgrPY0DQ"
     "https://tokio.rs/blog/index.xml"
     "https://rust-gamedev.github.io/rss.xml"
     "https://oribenshir.github.io/afternoon_rusting/feed.xml"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCyWj0MNyC0KNnyCBskIJKvw"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC7YY_Y0UXAbL_1xkY0hQjJA"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCpeX4D-ArTrsqvhLapAHprQ"
     "https://feeds.feedburner.com/steveklabnik/words"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC_iD0xppBwwsrM9DegC5cQQ"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCaYhcUwRBNscFNUKTjgPFiA"
     "https://rust-analyzer.github.io/feed.xml"
     "https://os.phil-opp.com/rss.xml"
     "https://seanmonstar.com/rss"
     "https://manishearth.github.io/atom.xml"
     "https://matklad.github.io/feed.xml"
     "https://deterministic.space/feed.xml"
     "http://carol-nichols.com/feed.xml"
     "https://blog.yoshuawuyts.com/rss.xml"
     "https://this-week-in-rust.org/rss.xml"
     "https://rustacean-station.org/podcast.rss"
     "https://readrust.net/all/feed.rss"
     "https://ferrous-systems.com/blog/feed.xml"
     "https://boats.gitlab.io/blog/index.xml"
     "https://blog.rust-lang.org/inside-rust/feed.xml"
     "http://feeds.soundcloud.com/users/soundcloud:users:721404514/sounds.rss"
     "https://blog.rust-lang.org/feed.xml"
     "http://blog.malwarebytes.org/feed/"
     "http://blog.talosintel.com/feeds/posts/default?alt=rss"
     "http://blog.webroot.com/feed/"
     "http://erratasec.blogspot.com/feeds/posts/default"
     "http://googleonlinesecurity.blogspot.com/atom.xml"
     "http://iscxml.sans.org/rssfeed.xml"
     "http://letsknowthings.com/feed/"
     "http://programming-journal.org/feed.rss"
     "http://rss.packetstormsecurity.com/news/"
     "http://ruzkuku.com/emacs.atom"
     "http://seclists.org/rss/fulldisclosure.rss"
     "http://seclists.org/rss/oss-sec.rss"
     "http://www.chess.com/rss/articles"
     "http://www.chess.com/rss/news"
     "http://www.eff.org/rss/updates.xml"
     "http://www.exploit-db.com/rss.php"
     "http://www.pixelbeat.org/feed/rss2.xml"
     "http://www.schneier.com/blog/index.rdf"
     "http://www.securelist.com/en/rss/allupdates"
     "https://api.reddit.com/timeline/me/492ff218-d4a4-45d9-a61d-f58b67fc8269"
     "https://blog.cmpxchg8b.com/feeds/posts/default?alt=rss"
     "https://blog.japaric.io/index.xml"
     "https://blog.locut.us/feed/"
     "https://blog.m-ou.se/index.xml"
     "https://blog.qutebrowser.org/feeds/all.rss.xml"
     "https://blog.rust-lang.org/feed.xml"
     "https://c4ss.org/?feed=rss"
     "https://chess24.com/en/read/news.rss"
     "https://cppcon.org/feed/"
     "https://crg.eti.br/index.xml"
     "https://feeds2.feedburner.com/Cprogrammingcom/"
     "https://ferrous-systems.com/blog/feed.xml"
     "https://geti2p.net/en/feed/blog/atom"
     "https://github.com/ipfs/go-ipfs/commits/master.atom"
     "https://github.com/ipfs/ipfs-docs/commits/main.atom"
     "https://github.com/sgriffin53/raven/commits/master.atom"
     "https://googleprojectzero.blogspot.com/feeds/posts/default"
     "https://googleprojectzero.blogspot.com/feeds/posts/default?alt=rss"
     "https://insights.sei.cmu.edu/blog/feeds/topic/secure-development/atom/"
     "https://isocpp.org/blog/rss"
     "https://isocpp.org/blog/rss/category/standardization"
     "https://kadampalife.org/feed/"
     "https://kagifeedback.org/atom/t/release-notes"
     "https://karthinks.com/index.xml"
     "https://kura.gg/feeds/rss.xml"
     "https://max-inden.de/index.xml"
     "https://newrustacean.com/feed.xml"
     "https://openai.com/feed.xml"
     "https://os.kaspersky.com/feed/"
     "https://protesilaos.com/codelog.xml"
     "https://readrust.net/all/feed.rss"
     "https://readrust.net/security/feed.rss"
     "https://rustacean-station.org/podcast.rss"
     "https://rustsec.org/feed.xml"
     "https://rusty-spike.blubrry.net/feed/"
     "https://sachachua.com/blog/category/emacs/feed/"
     "https://seclists.org/rss/bugtraq.rss"
     "https://seclists.org/rss/dailydave.rss"
     "https://seclists.org/rss/fulldisclosure.rss"
     "https://seclists.org/rss/microsoft.rss"
     "https://seclists.org/rss/nmap-announce.rss"
     "https://seclists.org/rss/nmap-dev.rss"
     "https://seclists.org/rss/oss-sec.rss"
     "https://seclists.org/rss/pauldotcom.rss"
     "https://seclists.org/rss/securecoding.rss"
     "https://seclists.org/rss/snort.rss"
     "https://seclists.org/rss/tcpdump.rss"
     "https://seclists.org/rss/wireshark.rss"
     "https://sounds-like-hate.captivate.fm/rssfeed"
     "https://sqrtminusone.xyz/posts/index.xml"
     "https://stegosaurusdormant.com/feed.xml"
     "https://taosecurity.blogspot.com/feeds/posts/default?alt=rss"
     "https://www.cvedetails.com/vulnerability-feed.php?vendor_id=0&product_id=0&version_id=0&orderby=3&cvssscoremin=5"
     "https://www.phoronix.com/rss.php"
     "https://www.reddit.com/r/Freenet/.rss"
     "https://www.reddit.com/r/emacs.rss"
     "https://www.reddit.com/r/planetemacs.rss"
     "https://www.schneier.com/feed/"
     "https://www.zerodayinitiative.com/blog/?format=rss"
     "https://www.zerodayinitiative.com/rss/published/"
     "https://www.zerodayinitiative.com/rss/upcoming/"
     "https://xairy.io/feed.xml"))
  (save-window-excursion
    (elfeed)
    (elfeed-update)))

(after! epa
  :config
  (require 'epg)
  (require 'pinentry)
  (setq
   epg-pinentry-mode 'ask
   epa-mail-aliases
   '(("savio.sena@gmail.com")
     ("savio.sena@acm.org")))
  (pinentry-start))

(after! erc
  (require 'gnutls)
  (setq
   erc-server "irc.libera.chat"
   erc-nick "_sav"
   erc-user "sav"
   erc-user-full-name "sav"
   erc-port 6697
   erc-use-ssl t
   erc-auth-source-mode t))

(after! eshell
  (defalias 'e 'find-file)
  (defalias 'ee 'find-file-other-window)
  (defalias 'e+ 'find-file-other-tab))

(after! eww
  (setq eww-default-download-directory (expand-file-name "~/dl")
        eww-search-prefix "https://www.google.com/search?q="))

(use-package! gcmh
  :config
  (gcmh-mode 1))

(after! gdb
  (setq
   gdb-display-io-nopopup t
   gdb-many-windows nil
   gdb-show-main t))

(after! flycheck
  (setq flycheck-error-list-minimum-level nil)
  (custom-set-faces!
    '(flycheck-error ((t (:underline nil))))
    '(flycheck-warning ((t (:underline nil))))
    '(flycheck-info ((t (:underline nil))))))

(after! google-translate
  (setq
   google-translate-default-source-language "pt"
   google-translate-default-target-language "en"))

(use-package! gptel
  :config
  (require 'pass)
  (require 'gptel-org)
  (require 'gptel-anthropic)
  (require 'gptel-gh)
  (setq
   gptel-api-key (my/auth-source-password "anthropic")
   ggptel-backend
   (gptel-make-anthropic "Claude"
     :stream t
     :key gptel-api-key)))

(after! hl-todo
  (setq
   hl-todo-keyword-faces
   '(("TODO" warning bold)
     ("FIXME" warning bold)
     ("XXX" error bold)
     ("HACK" font-lock-constant-face bold)
     ("REVIEW" font-lock-keyword-face bold)
     ("NOTE" success bold)
     ("DEPRECATED" font-lock-doc-face bold))))

(after! ibuffer
  (setq
   ibuffer-saved-filter-groups nil
   ibuffer-saved-filters
   '(("apps"
      (or
       (mode . eaf-mode)
       (mode . eww-mode)
       (mode . xwidget-webkit-mode)
       (mode . vterm-mode)
       (mode . term-mode)))
     ("code"
      (or
       (derived-mode . prog-mode)
       (mode . ess-mode)
       (mode . emacs-lisp-mode)
       (mode . sh-mode)
       (mode . go-mode)
       (mode . rust-mode)
       (mode . cargo-mode)
       (mode . sh-mode)
       (mode . compilation-mode)))
     ("text" (and (derived-mode . text-mode) (not (starred-name))))
     ("latex"
      (or
       (derived-mode . tex-mode)
       (mode . latex-mode)
       (mode . context-mode)
       (mode . ams-tex-mode)
       (mode . bibtex-mode)))
     ("news"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . elfeed-dashboard-mode)
       (mode . elfeed-search-mode)
       (mode . elfeed-summary-mode)
       (mode . elfeed-show-mode)
       (mode . mu4e-main-mode)
       (mode . mu4e-headers-mode)
       (mode . mu4e-view-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode)))
     ("*starred*" (starred-name)) ("others" (not (starred-names))))))

(use-package! iedit
  :config
  (define-key iedit-mode-keymap (kbd "C-c i ;") 'iedit-mode))

(after! image-dired
  (add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "C-<tab>") #'centaur-tabs-forward))))

(use-package! info+)

(use-package! ivy
  :config
  (map!
   :map ivy-minibuffer-map
   "C-<return>" #'ivy-immediate-done))

(use-package! lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode objc-mode) . lsp-deferred)
  :config
  (setq lsp-clients-clangd-executable "/usr/bin/clangd"))

(after! lsp
  (require 'lsp-rust)
  (setq lsp-keymap-prefix "C-c c")
  (setq lsp-auto-select-workspace nil)
  (setq lsp-auto-guess-root nil)
  (setq
   lsp-clients-clangd-args
   "-c ~/.clangd/clangd-config.yaml --header-insertion-decorators=0")
  (setq lsp-clients-clangd-executable "/usr/bin/clangd")
  (setq lsp-prefer-flymake nil)
  (set-lsp-priority! 'clangd 1)
  (set-lsp-priority! 'ccls 0)
  (add-hook 'lsp-mode-hook (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
  (add-hook 'lsp-mode-hook 'which-key-mode))

(after! lsp-ui
  (require 'lsp-ui-peek)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-mode t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-text-scale-level 0)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-mode t)
  (setq lsp-ui-flycheck-enable t)
  ;; disable underlines for errors and warnings in lsp-mode
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-doc-enable nil)
  ;; add initialization hooks
  (add-hook 'lsp-after-initialize-hook #'lsp-ui-mode)
  (add-hook 'lsp-ui-mode-hook #'lsp-ui-peek-mode)
  (add-hook 'lsp-ui-mode-hook #'lsp-ui-doc-mode))

(use-package! ligature
  :config
  (global-ligature-mode))

(after! lispy
  (add-to-list 'lispy-no-indent-modes 'emacs-lisp-mode)
  (setq lispy-insert-space-after-wrap t))

(after! mail
  (setq
   mail-default-directory "/data/mail/.inboxes"
   mail-user-agent 'mu4e-user-agent
   read-mail-command 'mu4e
   send-mail-function 'smtpmail-send-it))

(use-package! marginalia)

(after! message
  (setq
   message-confirm-send t
   message-signature-insert-empty-line nil))

(after! mu4e
  (require 'mu4e-config)
  (require 'mu4e-contrib)
  (require 'mu4e-message)
  (require 'mu4e-notification)
  (setq
   mu4e-bookmarks
   '((:name "All unread" :query "flag:unread AND NOT flag:trashed" :key 117)
     (:name "Today's unread" :query "date:today..now AND flag:u" :key 116)
     (:name "Unread to: savio.sena@gmail.com"
      :query "contact:savio.sena@gmail.com AND flag:u" :key 62)
     (:name "Unread to: savio.sena@acm.org"
      :query "contact:savio.sena@acm.org AND flag:u" :key 63))
   mu4e-change-filenames-when-moving t
   mu4e-compose-crypto-policy
   '(sign-all-messages
     sign-all-replies
     sign-encrypted-replies
     encrypt-encrypted-replies)
   mu4e-compose-dont-reply-to-self t
   mu4e-compose-keep-self-cc t
   mu4e-compose-signature t
   mu4e-confirm-quit nil
   mu4e-context-policy nil
   mu4e-date-format " %b %d %a  %R "
   mu4e-debug nil
   mu4e-get-mail-command "my.fetchmail"
   mu4e-headers-leave-behavior 'ask
   mu4e-headers-precise-alignment t
   mu4e-index-lazy-check t
   mu4e-maildir-shortcuts
   '(("ALLMAIL" . 42)
     ("/ALEKHINE.LOCAL" . 41)
     ("/ACM.ORG/" . 45)
     ("/GMAIL.COM/" . 46)
     ("/MAIL_LISTS/LIBP2P-RUST" . 48)
     ("/MAIL_LISTS/RUST-LANG/GITHUB" . 49)
     ("/MAIL_LISTS/GOLANG/GITHUB" . 50)
     ("/MAIL_LISTS/GOLANG/REVIEW" . 51)
     ("/MAIL_LISTS/FREENET/LOCUTUS-DEV" . 52)
     ("/MAIL_LISTS/FREENET/LOCUTUS-GIT" . 52)
     ("/MAIL_LISTS/GITHUB/SAV/NOTIFICATIONS" . 53)
     ("/MAIL_LISTS/OSS-SECURITY" . 54)
     ("/MAIL_LISTS/JOB-ALERTS" . 55))
   mu4e-mu-binary "/usr/local/bin/mu"
   mu4e-mu-debug nil
   mu4e-mu-home (expand-file-name "~/.cache/mu")
   mu4e-root-maildir "/data/mail/.inboxes"
   mu4e-search-results-limit 300000
   mu4e-update-interval 1800
   mu4e-use-fancy-chars t))

(after! notmuch
  (setq notmuch-init-file (expand-file-name "~/.notmuch-config")))

(use-package! org
  :config
  (require 'org-agenda)
  (require 'org-capture)
  (require 'org-clock)
  (require 'org-crypt)
  (require 'org-cycle)
  (require 'org-fold)
  (require 'org-indent)
  (require 'org-inlinetask)
  (require 'org-habit)
  (require 'org-mouse)
  (require 'org-notify)
  (require 'org-superstar)
  (setq
   org-adapt-indentation t
   org-agenda-files
   '("life.org.gpg"
     "agenda.org.gpg"
     "todo.org.gpg"
     "job.org.gpg")
   org-agenda-confirm-kill t
   org-agenda-remove-tags t
   org-agenda-show-inherited-tags 'always
   org-agenda-use-tag-inheritance nil
   org-auto-align-tags t
   org-blank-before-new-entry '((heading) (plain-list-item))
   org-clock-persist t
   org-clock-persist-file (expand-file-name "~/.emacs.org-clock-save.el")
   org-crypt-disable-auto-save t
   org-crypt-key "10356E2DE5C67FB0"
   org-crypt-tag-matcher "Secret"
   org-cycle-emulate-tab 'white
   org-cycle-hook
   '(org-inlinetask-hide-tasks
     org-cycle-hide-archived-subtrees
     org-cycle-hide-drawers
     org-cycle-show-empty-lines
     org-optimize-window-after-visibility-change)
   org-default-notes-file (format "%s%s" org-directory "inbox.org")
   org-export-backends '(ascii html icalendar latex md odt org)
   org-export-with-drawers nil
   org-export-with-tags nil
   org-export-with-toc 1
   org-fold-catch-invisible-edits 'smart
   org-hide-leading-stars t
   org-log-done 'time
   org-log-into-drawer t
   org-log-note-clock-out t
   org-log-reschedule 'time
   org-tags-column -108
   org-todo-keywords
   '((sequence
      "TODO(t!)"
      "URGE(u!)"
      "PLAN(p!)"
      "DOIN(s@/!)"
      "BLOCKED(b@/!)"
      "HOLD(h@/!)"
      "CONFIRM(f@/!)"
      "|"
      "DONE(d@/!)"
      "COMPLETE(c@/!)"
      "INCOMPLETE(i@/!)"
      "CANCELLED(x@/!)"
      "ABORT(A@/!)"
      "OVERDUE(o!)"
      "DROP(D@/!)"))
   org-todo-keyword-faces
   '(("TODO"
      :foreground "#445ca7"
      :weight bold
      :height 0.9)
     ("URGE"
      :background "gold2"
      :foreground "tan3"
      :weight bold
      :height 0.9)
     ("PLAN"
      :foreground "azure4"
      :slant oblique
      :weight regular
      :height 0.9)
     ("DOIN"
      :background "#2c81e9"
      :foreground "white"
      :weight black
      :height 1.0)
     ("BLOCKED"
      :background "red3"
      :foreground "white"
      :weight bold
      :height 1.0)
     ("HOLD"
      :background "orange3"
      :foreground "white"
      :weight bold
      :height 1.0)
     ("CANCELLED"
      :background "dark green"
      :foreground "white"
      :slant oblique
      :height 1.0)
     ("OVERDUE"
      :background "LightSteelBlue"
      :foreground "black"
      :slant oblique
      :height 1.0)
     ("COMPLETE" :foreground "green3" :weight bold :height 1.0)
     ("DONE" :foreground "green4" :weight normal :height 1.0)
     ("INCOMPLETE"
      :foreground "dark olive green"
      :weight normal
      :slant italic
      :height 1.0)
     ("ABORT" :foreground "DarkCyan" :weight bold :height 1.0)
     ("DROP" :foreground "DarkOrange4" :weight bold :height 1.0))
   org-use-tag-inheritance nil)
  (add-hook 'org-mode-hook #'org-superstar-mode))

(after! org-capture
  (setq
   org-capture-templates
   '(("l"
      "link"
      entry
      (file+olp (format "%s%s" org-directory "inbox.org") "Links")
      "* %a\n %?\n %i"
      :prepend t
      :immediate-finish t
      :jump-to-captured t
      :empty-lines-after 2)
     ("t"
      "todo"
      entry
      (file+headline (format "%s%s" org-directory "inbox.org") "Tasks")
      "* TODO %a\n SCHEDULED: %t\n\n#+begin_comment\n\n;;; initial-content\n%i\n\n;;; kill-ring-head\n%c\n\n;;; x-clipboard\n%x\n\n#+end_comment\n\n"
      :prepend t
      :immediate-finish t
      :jump-to-captured t
      :empty-lines-after 2)
     ("n"
      "note"
      entry
      (file+headline (format "%s%s" org-directory "inbox.org") "Notes")
      "* %a  %^g\n:PROPERTIES:\n:URL: %L\n:KEYWORDS: %^{KEYWORDS|my}p\n:ADDED: %T\n:END:\n\n#+begin_comment\n\n;;; initial-content\n%i\n\n;;; kill-ring-head\n%c\n\n;;; x-clipboard\n%x\n\n#+end_comment\n\n"
      :prepend t
      :jump-to-captured t
      :empty-lines-after 2)
     ("r"
      "read"
      entry
      (file+olp (format "%s%s" org-directory "inbox.org") "Readings")
      "* %a\n %?\n %i"
      :prepend t
      :immediate-finish t
      :jump-to-captured t
      :empty-lines-after 2)
     ("j"
      "journal"
      entry
      (file+olp+datetree (format "%s%s" org-directory "inbox.org") "Journey")
      "* %a\nEntered on %U\n  %l\n  %a\n  %i\n"
      :clock-in t
      :clock-resume t
      :jump-to-captured t
      :immediate-finish t
      :empty-lines-after 2)
     ("u"
      "push"
      entry
      (clock)
      "  - [ ] %i :: %l\n"
      :clock-keep t
      :jump-to-captured t
      :empty-lines-after 2))))

(after! org-gcal ;; https://github.com/myuhe/org-gcal.el
  (require 'auth-source)
  (require 'oauth2)
  (require 'request)
  (require  alert)
  (add-to-list
   'oauth2-auto-additional-providers-alist
   '(org-gcal
     (authorize-url . "https://accounts.google.com/o/oauth2/auth")
     (access-token-url . "https://oauth2.googleapis.com/token")
     (scope . "https://www.googleapis.com/auth/calendar")))
  (setq
   org-gcal-client-id (my/auth-source-password "google-calendar" "client-id")
   org-gcal-client-secret (my/auth-source-password "google-calendar" "client-secret")
   org-gcal-file-alist
   '(("savio.sena@gmail.com" . "~/org/gcal/default.org")
     ("mnmfjjfria4n4rjg7euqa0dtk4@group.calendar.google.com" . "~/org/gcal/anniversaries.org")
     ("8hpkdjoq4foso4475u3n0t86hc@group.calendar.google.com" . "~/org/gcal/annotations.org")
     ("vccoj5evoffjdf8qsiamcbal7o@group.calendar.google.com" . "~/org/gcal/appointments.org")
     ("u1l0ld6tp95c9um5inl91q853o@group.calendar.google.com" . "~/org/gcal/important.org")
     ("r0ob5ugb003qmltfsu2u2qn1ic@group.calendar.google.com" . "~/org/gcal/notes.org")
     ("6vj2pugrfr7lfhfrsc9kjhgcg4@group.calendar.google.com" . "~/org/gcal/social.org")))
  (org-gcal-reload-client-id-secret))

(after! org-notify
  (setq org-notify-max-notifications-per-run 1))

(use-package! org-superstar
  :config
  (setq
   org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?◦) (?- . ?‣))
   org-superstar-special-todo-items t
   org-superstar-remove-leading-stars t
   org-superstar-special-todo-items nil
   org-superstar-remove-leading-stars nil
   org-superstar-todo-bullet-alist
   '(("TODO" . ?⛶)
     ("DOING" . ?⸎)
     ("BLOCKED" . ?⏰)
     ("HOLD" . ?☕)
     ("CANCELLED" . ?✘)
     ("DONE" . ?✔)
     ("COMPLETE" . ?✔)
     ("INCOMPLETE" . ?✔)
     ("OVERDUE" . ?✔)))
  (with-eval-after-load 'org-superstar
    (org-superstar-restart)))

(use-package! parinfer
  :config
  (setq
   parinfer-extensions
   '(defaults
     pretty-parens
     smart-tab
     smart-yank)))

(use-package! pass) ;; https://jherrlin.github.io/posts/emacs-gnupg-and-pass/

(after! persp-mode
  :config
  (persp-mode))

(after! prog-mode
  (global-prettify-symbols-mode))

(use-package! projectile
  :config
  (setq
   projectile-auto-discover nil
   projectile-default-src-directory (expand-file-name "~/src")
   projectile-dynamic-mode-line nil
   projectile-known-projects-file (expand-file-name "~/.emacs-projectiles")
   projectile-mode-line-prefix " Proj")
  (define-key projectile-mode-map (kbd "C-c r p") #'projectile-command-map))

(use-package! rcirc
  :config
  (require 'epa)
  (epa-file-enable)
  (my/auth-source-flush)
  (setq
   libera-chat-password (my/auth-source-password "irc.libera.chat")
   rcirc-default-nick "sav"
   rcirc-default-user-name "sav"
   rcirc-default-full-name "github.com/sav"
   rcirc-authenticate-before-join t
   rcirc-authinfo '(("irc.libera.chat" nickserv "_sav" libera-chat-password))
   rcirc-server-alist
   '(("irc.libera.chat"
      :nick "_sav"
      :user-name "sav"
      :full-name "github.com/sav"
      ;; :port 6667
      :port 6697
      :encryption tls
      :channels ("#tsar" "#eev" "#lean" "#emacs")))))

(use-package! recentf
  :config
  (setq
   recentf-max-menu-items 2500
   recentf-max-saved-items 5000
   recentf-mode 1
   recentf-auto-cleanup 'never
   recentf-save-file (expand-file-name "~/.emacs-recentf"))
  (recentf-mode))

(after! rmail
  (setq
   rmail-confirm-expunge 'yes-or-no-p
   rmail-default-body-file (expand-file-name "/data/mail/.rmail/sav@alekhine+mailout.rmail")
   rmail-default-file (expand-file-name "/data/mail/.rmail/sav@alekhine+xmail.rmail")
   rmail-display-summary t
   rmail-file-name (expand-file-name "/data/mail/.rmail/sav@alekhine.rmail")
   rmail-mime-show-images 'show
   rmail-preserve-inbox t
   rmail-redisplay-summary t
   rmail-summary-window-size 92))

(after! rust-mode
  (add-hook 'rust-mode-hook #'lsp-deferred)
  (add-hook 'rust-mode-hook (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
  (add-hook 'rust-mode-hook (lambda () (local-set-key (kbd "C-c C-n") #'next-buffer))))

(use-package! savehist
  :config
  (add-to-list 'savehist-additional-variables 'buffer-name-history)
  (setq savehist-file (expand-file-name "~/.emacs-history"))
  (savehist-mode 1))

(use-package! saveplace
  :config
  (save-place-mode))

(after! smartparens
  (setq sp-autoinsert-pair nil))

(after! smtpmail
  (setq
   smtpmail-default-smtp-server nil
   smtpmail-local-domain "gmail.com"
   smtpmail-queue-dir (expand-file-name "/data/mail/.queued-mail/")
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587
   smtpmail-smtp-user "savio.sena@gmail.com"
   smtpmail-store-queue-variables nil
   smtpmail-stream-type 'starttls))

(after! tab-bar
  :config
  (tab-bar-mode t))

(use-package! telega)

(use-package! tree-sitter
  :config
  (global-tree-sitter-mode))

(after! treemacs-all-the-icons
  (treemacs-load-theme 'all-the-icons))

(use-package! vertico
  :config
  (setq
   completion-styles '(orderless flex)
   completion-category-defaults nil)
  (setq
   completion-category-overrides
   '((file (styles partial-completion))
     (command (styles orderless))))
  (vertico-mode)
  (vertico-grid-mode))

(after! vterm
  (setq vterm-shell "/usr/bin/zsh")
  (add-hook
   'vterm-exit-functions
   (lambda (&optional buf _)
     (interactive)
     (delete-window (get-buffer-window buf))))
  (add-hook
   'term-setup
   (lambda ()
     (highlight-changes-mode -1)
     (whitespace-mode -1)))
  (add-hook
   'vterm-mode-hook
   (lambda () (local-set-key (kbd "C-S-v") #'term-paste))))

(use-package! which-key
  :config
  (which-key-mode))

(use-package! winner
  :config
  (winner-mode))

(use-package! writeroom-mode
  :config
  (setq
   writeroom-extra-line-spacing nil
   writeroom-fringes-outside-margins t
   writeroom-global-effects
   '(writeroom-set-fullscreen
     writeroom-set-menu-bar-lines
     writeroom-set-tool-bar-lines
     writeroom-set-vertical-scroll-bars
     writeroom-set-bottom-divider-width)
   writeroom-header-line t
   writeroom-major-modes nil
   writeroom-mode-line t
   writeroom-border-width 0
   writeroom-width 180)
  (global-writeroom-mode))

(after! ws-butler
  (ignore-errors
    (unload-feature 'ws-butler)))

(after! xwidget
  :config
  (setq
   xwidget-webkit-bookmark-jump-new-session t
   xwidget-webkit-cookie-file (expand-file-name "~/.emacs-webkit-cookie")
   xwidget-webkit-download-dir (expand-file-name "~/dl")))

(use-package! xcscope
  :config
  (cscope-setup))

(use-package! pushbullet
  :load-path "~/my/src/pushbullet"
  :config
  (setq pushbullet-token (password-store-get "PushBullet/savio.sena@gmail.com/API_KEY_V2")))

;;;
;;; Hooks
;;;

(add-hook! 'prog-mode-hook #'prettify-symbols-mode)

(add-hook! 'prog-mode-hook #'treemacs-tag-follow-mode)

(add-hook!
 'emacs-startup-hook
 #'(lambda ()
     (message "╭─● my-emacs::emacs-startup-hook → called")
     (doom/load-session "~/.emacs-doom-session")
     (message "╰─● my-emacs::emacs-startup-hook → returned")))

(add-hook!
 'kill-emacs-hook
 #'(lambda ()
     (message "╭─● my-emacs::kill-emacs-hook → called")
     (doom/save-session "~/.emacs-doom-session")
     (message "╰─● my-emacs::kill-emacs-hook → returned")))

;;;
;;; Faces
;;;

(when (eq doom-theme-style 'dark)
  (custom-set-faces!
    '(default :background "#090909" :foreground "#d8d8d8")
    '(highlight :inherit (mode-line-highlight mode-line-buffer-id) :background nil)
    '(tab-bar :inherit mode-line)
    '(tab-bar-tab :inherit mode-line-buffer-id)
    '(tab-bar-tab-inactive :inherit mode-line-inactive)
    '(font-lock-builtin-face :foreground "SlateGray4" :background nil :underline nil :inherti default)
    '(font-lock-comment-face :foreground "grey30" :background nil :underline nil :inherit default)
    '(font-lock-constant-face :foreground "grey70" :background nil :underline nil :inherit default)
    '(font-lock-doc-face :foreground "grey30" :background nil :underline nil :inherti default)
    '(font-lock-function-name-face :foreground "LightSteelBlue3" :background nil :underline nil :inherti default)
    '(font-lock-keyword-face :foreground "white" :background nil :underline nil :inherti default)
    '(font-lock-preprocessor-face :foreground nil :background nil :underline nil :inherti default)
    '(font-lock-string-face :foreground "gray50" :background nil :underline nil :slant oblique :inherti default)
    '(font-lock-type-face :foreground "LightCyan3" :background nil :underline nil :inherti default)
    '(font-lock-variable-name-face :foreground "azure4" :background nil :underline nil :inherti default)
    '(font-lock-warning-face :foreground "tan" :background nil :underline nil :inherti default)
    ;; '(font-lock-doc-markup-face :foreground nil :background nil :underline nil :inherit default)
    ;; '(font-lock-function-call-face :foreground nil :background nil :underline nil :inherti default)
    ;; '(font-lock-property-name-face :foreground nil :background nil :underline nil :inherti default)
    ;; '(font-lock-regexp-face :foreground nil :background nil :underline nil :inherti default)
    '(org-headline-done :foreground nil :background nil :slant oblique :inherit font-lock-comment-face)))

;;;
;;; $DOOMDIR/config.el ends here
;;;
