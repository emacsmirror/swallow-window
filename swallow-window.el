;;; swallow-window.el --- Grow the current window into the space occupied by neighboring windows

;; Author: Kyle Hargraves <pd@krh.me>
;; URL: https://github.com/pd/swallow-window.el
;; Version: 0.1
;; Keywords: convenience, frames
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Ever have your windows arranged in, say, a 2x2 square and go to delete
;; one of them, only to end up with the wrong window split disappearing?
;; It happens to me all the time. When you use `delete-window', the split
;; that will disappear depends on the order in which you created the
;; windows.
;;
;;     +-------+-------+
;;     |       |       |
;;     |   A   |   B   |
;;     |       |       |
;;     +-------+-------+
;;     |       |       |
;;     |   C*  |   D   |
;;     |       |       |
;;     +-------+-------+
;;
;; Imagine you are focused in window C, which contains tall content such
;; as a rapidly scrolling log file you're tailing. It'd be nice to eliminate
;; window A so that you could see more at once. Unfortunately, you actually
;; opened these in the order (A C D B), so when you kill A, B takes over
;; all of its space.
;;
;; This library fixes that.
;;
;; Rather than deleting window A, stay focused on C and tell it to consume
;; all the space in the window above it.
;;
;;     +-------+-------+      +-------+-------+
;;     |       |       |      |       |       |
;;     |   A   |   B   |      |       |   B   |
;;     |       |       |      |       |       |
;;     +-------+-------+  ->  |   C*  +-------+
;;     |       |       |      |       |       |
;;     |   C*  |   D   |      |       |   D   |
;;     |       |       |      |       |       |
;;     +-------+-------+      +-------+-------+
;;
;; But what if multiple windows were above it?
;;
;;     +-------+-------+      +-------+-------+      +-------+-------+
;;     |   A   |       |      |   A   |       |      |       |       |
;;     +-------+   B   |      +-------+   B   |      |       |   B   |
;;     |   E   |       |      |       |       |      |       |       |
;;     +-------+-------+  ->  |       +-------+  ->  |   C*  +-------+
;;     |       |       |      |   C*  |       |      |       |       |
;;     |   C*  |   D   |      |       |   D   |      |       |   D   |
;;     |       |       |      |       |       |      |       |       |
;;     +-------+-------+      +-------+-------+      +-------+-------+
;;
;; The same operation would first eliminate E, and running it once more
;; would eliminate A.
;;
;; What about a pair of windows split in the opposite direction as the
;; window you are focused on?
;;
;;     +---+---+-------+      +-------+-------+
;;     |   |   |       |      |       |       |
;;     | A | E |   B   |      |       |   B   |
;;     |   |   |       |      |       |       |
;;     +---+---+-------+  ->  |   C*  +-------+
;;     |       |       |      |       |       |
;;     |   C*  |   D   |      |       |   D   |
;;     |       |       |      |       |       |
;;     +-------+-------+      +-------+-------+
;;
;; Some cases are harder to decide:
;;
;;     +-------+-------+      +-------+-------+    +-------+-------+
;;     |   A*  |       |      |       A*      |    |       A*      |
;;     +-------|   C   |  ->  +---------------| OR +---------------|
;;     |   B   |       |      |   B   |   C   |    |       B       |
;;     +-------+-------+      +-------+-------+    +-------+-------+
;;
;; If I swallow right from A, I generally want to retain C; if I had
;; wanted both A and B to grow, I could have just deleted C. So that
;; is the default behavior. With `prefix-arg', though, swallowing will
;; delete C rather than resize it.

;;; Code:

(require 'windmove)

(defun swallow-window-info (window)
  (let ((edges (window-edges window)))
    `((height . ,(window-height window))
      (width  . ,(window-width  window))
      (left   . ,(nth 0 edges))
      (top    . ,(nth 1 edges))
      (right  . ,(nth 2 edges))
      (bottom . ,(nth 3 edges)))))

(defun swallow-window-grow-up (win top)
  (let* ((props (swallow-window-info win))
         (delta (- (cdr (assoc 'top props)) top)))
    (enlarge-window delta)))

(defun swallow-window-grow-left (win left)
  (let* ((props (swallow-window-info win))
         (delta (- (cdr (assoc 'left props)) left)))
    (enlarge-window delta t)))

(defun swallow-window-edges (target dir edge)
  (delete-window target)
  (when (not (one-window-p))
    (cond
     ((eq 'up dir)
      (swallow-window-grow-up (selected-window) edge))

     ((eq 'left dir)
      (swallow-window-grow-left (selected-window) edge)))))

;;;###autoload
(defun swallow-window (dir)
  (interactive)
  (let* ((target (windmove-find-other-window dir))
         (props  (and target (swallow-window-info target))))
    (cond
     ((one-window-p)
      (message "swallow-window: no other window to swallow"))

     ((not target)
      (message "swallow-window: no window there"))

     ((window-minibuffer-p target)
      (message "swallow-window: can't swallow minibuffer"))

     ((eq 'up dir)
      (swallow-window-edges target dir (cdr (assoc 'top props))))

     ((eq 'down dir)
      (swallow-window-edges target dir (cdr (assoc 'bottom props))))

     ((eq 'left dir)
      (swallow-window-edges target dir (cdr (assoc 'left props))))

     ((eq 'right dir)
      (swallow-window-edges target dir (cdr (assoc 'right props))))

     (t (error "No such direction: %s" dir)))))

;;;###autoload
(defun swallow-window-up ()
  (interactive)
  (swallow-window 'up))

;;;###autoload
(defun swallow-window-down ()
  (interactive)
  (swallow-window 'down))

;;;###autoload
(defun swallow-window-left ()
  (interactive)
  (swallow-window 'left))

;;;###autoload
(defun swallow-window-right ()
  (interactive)
  (swallow-window 'right))

(provide 'swallow-window)
;;; swallow-window.el ends here
