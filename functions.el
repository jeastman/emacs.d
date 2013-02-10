;; Collection of random functions picked up from all around.
;; Each function cites it origin

;; http://whattheemacsd.com/buffer-defuns.el-02.html
;; flips a two-window frame, so that left is right, or up is down.
(defun rotate-windows ()
"Rotate your windows"   (interactive)
   (cond ((not (> (count-windows)1))
          (message "You can't rotate a single window!"))
         (t
          (setq i 1)
          (setq numWindows (count-windows))
          (while  (< i numWindows)
            (let* (
                   (w1 (elt (window-list) i))
                   (w2 (elt (window-list) (+ (% i numWindows) 1)))
                   (b1 (window-buffer w1))
                   (b2 (window-buffer w2))
                   (s1 (window-start w1))
                   (s2 (window-start w2))
                   )
              (set-window-buffer w1  b2)
              (set-window-buffer w2 b1)
              (set-window-start w1 s2)
              (set-window-start w2 s1)
              (setq i (1+ i)))))))

;; bind a key similar to winner-mode's
(bind-key "C-x <up>" `rotate-windows)

;; http://whattheemacsd.com/buffer-defuns.el-03.html
;; toggles between horizontal and vertical layout of two windows
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; http://whattheemacsd.com/buffer-defuns.el-01.html
;; Witespace cleanup
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; Various superfluous white-space. Just say no.
;; We don't set the hook here becasue it is too invasive
;;(add-hook 'before-save-hook 'cleanup-buffer-safe)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(bind-key "C-c n" 'cleanup-buffer)
