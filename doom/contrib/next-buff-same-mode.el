;;; https://www.reddit.com/r/emacs/comments/rtvpvw/switching_to_a_buffer_of_the_same_mode/

(defun my-change-buffer-same-major-mode (change-buffer)
  "Call CHANGE-BUFFER until the current buffer has the initial `major-mode'."
  (let ((initial (current-buffer))
        (mode major-mode))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (not (eq major-mode mode))
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

(defun my-next-buffer-same-major-mode ()
  "Like `next-buffer' for buffers in the current `major-mode'."
  (interactive)
  (my-change-buffer-same-major-mode 'next-buffer))

(defun my-previous-buffer-same-major-mode ()
  "Like `previous-buffer' for buffers in the current `major-mode'."
  (interactive)
  (my-change-buffer-same-major-mode 'previous-buffer))

(global-set-key [remap next-buffer] 'my-next-buffer-same-major-mode)
(global-set-key
 [remap previous-buffer] 'my-previous-buffer-same-major-mode)

(provide 'next-buff-same-mode)
