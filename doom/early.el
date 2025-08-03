;;; ../my/src/dot/doom/early-init.el -*- lexical-binding: t; -*-

;; (require 'backtrace)
;; (setq debug-on-error t)
;; (setq auth-source-debug t)

;; (defun trace-auth-source (&rest args)
;;   (message "auth-source-search called with: %S\nBacktrace:\n%s"
;;            args (backtrace-to-string)))
;; (advice-add 'auth-source-search :before #'trace-auth-source)
;;
;; (defun my-log-custom-initialize-reset (&rest args)
;;   "Advice to log calls to `custom-initialize-reset`."
;;   (message "Called `custom-initialize-reset`.")
;;   (let ((stack (backtrace-to-string)))
;;     (message "Backtrace:\n%s" stack)))

;; (advice-add 'custom-initialize-reset :before #'my-log-custom-initialize-reset)

;; (defun my-debug (filename &rest _)
;;   "Advice to debug who is opening authinfo.gpg."
;;   (when (string-match-p ".authinfo.gpg" filename)
;;     (message "opening .authinfo.gpg... %s" (backtrace-to-string))
;;     (debug)))
;; (advice-add 'file-read-string :before #'my-debug)
;; (advice-add 'find-file :before #'my-debug)

(message "╭─● my-emacs::early-init → called")
(require 'epa)
(require 'epg)
(epa-file-enable)
(message "╰─● my-emacs::early-init → returned")
