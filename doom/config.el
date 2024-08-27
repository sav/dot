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

(load! "lib.el")
(load! "keys.el")
(load! "contrib/init.el")

(setq user-full-name "Savio Sena"
      user-mail-address "savio.sena@gmail.com")

(setq doom-theme-style 'light)

(if (eq doom-theme-style 'dark)
    (setq doom-theme 'doom-acario-dark)
  (setq doom-theme 'doom-opera-light))

(cond
 ((string-equal system-name "tal.local")
  (if (eq doom-theme-style 'dark)
      (setq doom-font-size 16
            doom-font-weight-default 'regular)
    (setq doom-font-size 14
          doom-font-weight-default 'medium)))
 (t
  (setq doom-font-size 16
        doom-font-weight-default
        (if (eq doom-theme-style 'dark)
            'regular
          'medium))))

(setq doom-leader-alt-key "C-c d")

(setq doom-big-font-size 20
      doom-font-default "Iosevka"
      doom-font (font-spec :family doom-font-default :size doom-font-size :weight doom-font-weight-default)
      doom-big-font (font-spec :family doom-font-default :size doom-big-font-size :weight doom-font-weight-default)
      doom-serif-font (font-spec :family "Iosevka Slab" :size doom-font-size :weight doom-font-weight-default)
      doom-symbol-font (font-spec :family "FontAwesome" :size doom-font-size :weight doom-font-weight-default)
      doom-variable-pitch-font (font-spec :family "Gentium Book Plus" :size doom-font-size :weight doom-font-weight-default)
      +zen-text-scale 0)

(setq confirm-kill-processes nil
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
      vc-follow-symlinks t)

(after! alert
  (setq alert-default-style 'notifications))

(after! all-the-icons-ivy
  (add-hook 'after-init-hook #'all-the-icons-ivy-setup))

(after! auth-source
  (setq auth-sources '("~/.authinfo.gpg" "~/.netrc")))

(after! bookmark
  (setq bookmark-bmenu-toggle-filenames nil
        bookmark-default-file (expand-file-name "~/.emacs-bookmarks.el")
        bookmark-menu-confirm-deletion t
        bookmark-menu-length 120
        bookmark-sort-flag t))

(after! browse-url
  (setq browse-url-browser-function #'browse-url-default-browser
        browse-url-chrome-program "my.xsel-browser"
        browse-url-chromium-program "my.xsel-browser"))

(after! centaur-tabs
   (setq centaur-tabs-adjust-buffer-order t
       centaur-tabs-cycle-scope 'tabs
       centaur-tabs-enable-key-bindings t
      centaur-tabs-set-bar 'under))

(after! counsel
  (setq counsel-search-engine 'google))

(after! counsel-web
  (setq counsel-web-search-action 'eww))

(after! dired
  (add-hook 'dired-mode-hook (lambda () (local-unset-key (kbd "C-t"))))
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))))

(after! elfeed
  (setq elfeed-search-filter "@6-months-ago")
  (setq elfeed-search-title-max-width 90)
  (setq elfeed-search-title-min-width 40)
  (setq elfeed-summary-width 140)
  (setq elfeed-feeds
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
          "https://xairy.io/feed.xml")))

(after! epa
  (setq epa-mail-aliases '(("savio.sena@acm.org")
                           ("savio.sena@gmail.com"))))

(after! epg
  (require 'pinentry)
  (setq epg-pinentry-mode 'ask))

(after! eshell
  (defalias 'e 'find-file)
  (defalias 'ee 'find-file-other-window)
  (defalias 'e+ 'find-file-other-tab))

(after! eww
  (setq eww-default-download-directory (expand-file-name "~/dl")
        eww-search-prefix "https://www.google.com/search?q="))

(after! gcmh
  (gcmh-mode 1))

(after! gdb
  (setq gdb-display-io-nopopup t
        gdb-many-windows nil
        gdb-show-main t))

(after! flycheck
  (setq flycheck-error-list-minimum-level nil))

(after! google-translate
  (setq google-translate-default-source-language "pt"
        google-translate-default-target-language "en"))

(after! hl-todo
  (setq hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("XXX" error bold)
          ("HACK" font-lock-constant-face bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("NOTE" success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(after! ibuffer
  (setq ibuffer-saved-filter-groups nil
        ibuffer-saved-filters
        '(("apps" (or (mode . eaf-mode)
                      (mode . eww-mode)
                      (mode . xwidget-webkit-mode)
                      (mode . vterm-mode)
                      (mode . term-mode)))
          ("code" (or (derived-mode . prog-mode)
                      (mode . ess-mode)
                      (mode . emacs-lisp-mode)
                      (mode . sh-mode)
                      (mode . go-mode)
                      (mode . rust-mode)
                      (mode . cargo-mode)
                      (mode . sh-mode)
                      (mode . compilation-mode)))
          ("text" (and (derived-mode . text-mode) (not (starred-name))))
          ("latex" (or (derived-mode . tex-mode)
                       (mode . latex-mode)
                       (mode . context-mode)
                       (mode . ams-tex-mode)
                       (mode . bibtex-mode)))
          ("news" (or (mode . message-mode)
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

(after! image-dired
  (add-hook 'dired-mode-hook (lambda () (local-set-key (kbd "C-<tab>") #'centaur-tabs-forward))))

(after! info+)

(after! ivy
  (map! :map ivy-minibuffer-map
        "C-<return>" #'ivy-immediate-done))

(after! lsp
  (require 'lsp)
  (require 'lsp-rust)
  (setq lsp-keymap-prefix "C-c c")
  (setq lsp-auto-select-workspace nil)
  (setq lsp-auto-guess-root nil)
  (setq lsp-clients-clangd-args
        "-c ~/.clangd/clangd-config.yaml --header-insertion-decorators=0")
  (add-hook 'lsp-mode-hook (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
  (add-hook 'lsp-mode-hook 'which-key-mode))

(after! lsp-ui
  (require 'lsp-ui)
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
  (add-hook 'lsp-after-initialize-hook #'lsp-ui-mode)
  (add-hook 'lsp-ui-mode-hook #'lsp-ui-peek-mode)
  (add-hook 'lsp-ui-mode-hook #'lsp-ui-doc-mode))

(after! lispy
  (setq lispy-insert-space-after-wrap t))

(after! mail
  (setq mail-default-directory "/data/mail/.inboxes"
        mail-user-agent 'mu4e-user-agent
        read-mail-command 'mu4e
        send-mail-function 'smtpmail-send-it))

(after! message
  (setq message-confirm-send t
        message-signature-insert-empty-line nil))

(after! mu4e
  (require 'mu4e-config)
  (require 'mu4e-contrib)
  (require 'mu4e-message)
  (require 'mu4e-notification)
  (setq mu4e-bookmarks
        '((:name "All unread" :query "flag:unread AND NOT flag:trashed" :key 117)
          (:name "Today's unread" :query "date:today..now AND flag:u" :key 116)
          (:name "Unread to: savio.sena@gmail.com" :query
                 "contact:savio.sena@gmail.com AND flag:u" :key 62)
          (:name "Unread to: savio.sena@acm.org" :query
                 "contact:savio.sena@acm.org AND flag:u" :key 63))
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

(after! org
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
  (setq org-adapt-indentation t
        org-agenda-files '("life.org.gpg" "agenda.org.gpg")
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
        '(("TODO" :foreground "#445ca7" :weight bold :height 0.9)
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
  (setq org-capture-templates
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

(after! org-notify
  (setq org-notify-max-notifications-per-run 1))

(after! org-superstar
  (setq org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?◦) (?- . ?‣))
        org-superstar-special-todo-items t
        org-superstar-remove-leading-stars t
        org-superstar-special-todo-items nil
        org-superstar-remove-leading-stars nil
        org-superstar-todo-bullet-alist
        '(("TODO" . ?⛶)
          ("DOING" . ?👷)
          ("BLOCKED" . ?🛑)
          ("HOLD" . ?☕)
          ("CANCELLED" . ?✘)
          ("DONE" . ?✔)
          ("COMPLETE" . ?✔)
          ("INCOMPLETE" . ?✔)
          ("OVERDUE" . ?✔)))
  (with-eval-after-load 'org-superstar
    (org-superstar-restart)))

(after! parinfer
  (setq parinfer-extensions
        '(defaults
          pretty-parens
          smart-tab ; C-b & C-f jump positions and smart shift with tab & S-tab.
          smart-yank)))

(after! pass ;; https://jherrlin.github.io/posts/emacs-gnupg-and-pass/
  (setq epg-pinentry-mode 'loopback))

(after! pinentry
  (pinentry-start))

(after! projectile
  (setq projectile-auto-discover nil
        projectile-default-src-directory (expand-file-name "~/src")
        projectile-dynamic-mode-line nil
        projectile-known-projects-file (expand-file-name "~/.emacs-projectiles")
        projectile-mode-line-prefix " Proj")
  (define-key projectile-mode-map (kbd "C-c r p") #'projectile-command-map))

(after! pushbullet
  (setq pushbullet-api-key (password-store-get "PushBullet/API_KEY")))

(after! recentf
  (setq recentf-max-menu-items 50
        recentf-max-saved-items 2500
        recentf-mode 1
        recentf-save-file (expand-file-name "~/.emacs-recentf")))

(after! rmail
  (setq rmail-confirm-expunge 'yes-or-no-p
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

(after! savehist
  (setq savehist-file (expand-file-name "~/.emacs-history")))

(after! smartparens
  (setq sp-autoinsert-pair nil))

(after! smtpmail
  (setq smtpmail-default-smtp-server nil
        smtpmail-local-domain "gmail.com"
        smtpmail-queue-dir (expand-file-name "/data/mail/.queued-mail/")
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-smtp-user "savio.sena@gmail.com"
        smtpmail-store-queue-variables nil
        smtpmail-stream-type 'starttls))

(after! tab-bar
  (tab-bar-mode t))

(after! treemacs-all-the-icons
  (treemacs-load-theme 'all-the-icons))

(after! vterm
  (add-hook 'vterm-exit-functions
            (lambda (&optional buf _)
              (interactive)
              (delete-window (get-buffer-window buf))))
  (add-hook 'term-setup
            (lambda ()
              (highlight-changes-mode -1)
              (whitespace-mode -1)))
  (add-hook 'vterm-mode-hook
            (lambda () (local-set-key (kbd "C-S-v") #'term-paste))))

(after! winner
  (winner-mode))

(after! writeroom
  (setq writeroom-extra-line-spacing nil
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
        writeroom-width 120))

(after! ws-butler
  (ignore-errors
    (unload-feature 'ws-butler)))

(after! xwidget
  :config
  (setq xwidget-webkit-bookmark-jump-new-session t
       xwidget-webkit-cookie-file (expand-file-name "~/.emacs-webkit-cookie")
       xwidget-webkit-download-dir (expand-file-name "~/dl")))

(use-package! xcscope
  :config
  (cscope-setup))

;; FIXME apparent compatibility issues between Emacs 29.x, Doom Emacs, and `ivy'.
(add-hook 'emacs-startup-hook #'ivy-mode)

;;;
;;; $DOOMDIR/config.el ends here
;;;

;;; vim:ft=lisp:ts=2:sw=2:et:
