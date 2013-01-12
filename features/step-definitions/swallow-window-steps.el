(Before
 (setq sw/current-layout nil
       sw/current-windows nil)
 (dolist (buf (buffer-list))
   (when (s-matches? "^sw/" (buffer-name buf))
     (kill-buffer buf))))

(defun sw/window-named (name)
  (nth 1 (assoc name sw/current-windows)))

(defun sw/window-props (name)
  (nth 2 (assoc name sw/current-windows)))

(defun sw/orig-prop (name prop)
  (cdr (assoc prop (sw/window-props name))))

(Given "^I delete all other windows$"
       (lambda () (delete-other-windows)))

(Given "^the window layout:$"
       (lambda (layout)
         (setq sw/current-layout  (sw/read-layout layout)
               sw/current-windows (sw/cache-win-infos (sw/mk-layout sw/current-layout)))
         (should (= (length (window-list))
                    (sw/win-count sw/current-layout)))))

(When "^I swallow-window \\(.+\\)$"
       (lambda (dir) (swallow-window (intern dir))))

(When "^I select window \\([A-Z]\\)$"
       (lambda (name)
         (select-window (sw/window-named name))))

(Then "^window \\([A-Z]\\) should be the only window in the frame$"
       (lambda (name)
         (should (equal (window-list) (list (sw/window-named name))))))

(Then "^window \\([A-Z]\\) should be deleted$"
       (lambda (name)
         (let ((win (sw/window-named name)))
           (should (not (window-live-p win))))))

(Then "^window \\([A-Z]\\) should be the only window on the \\(.+\\)$"
      (lambda (name side)
        (let ((win (sw/window-named name))
              (dirs (cond
                     ((string= "top" side) '(up left right))
                     ((string= "bottom" side) '(down left right))
                     ((string= "left" side) '(up down right))
                     ((string= "right" side) '(up down left)))))
          (select-window win)
          (dolist (dir dirs)
            (unless (window-minibuffer-p (windmove-find-other-window dir))
              (should-not (windmove-find-other-window dir)))))))

(Then "^window \\([A-Z]\\) should be the same size$"
      (lambda (name)
        (let ((win   (sw/window-named name))
              (props (sw/window-props name)))
          (should (= (window-height win) (sw/orig-prop name 'height)))
          (should (= (window-width win) (sw/orig-prop name 'width))))))

(Then "^window \\([A-Z]\\) should be the full frame \\(height\\|width\\)$"
      (lambda (name dim)
        (if (string= dim "width")
            (should (window-full-width-p (sw/window-named name)))
          (should (window-full-height-p (sw/window-named name))))))

(Then "^window \\([A-Z]\\) should be \\(shorter\\|narrower\\)$"
      (lambda (name measurement)
        (let ((win   (sw/window-named name))
              (props (sw/window-props name)))
          (should (window-live-p win))
          (cond
           ((string= measurement "shorter")
            (should (< (window-height win) (sw/orig-prop name 'height))))
           ((string= measurement "narrower")
            (should (< (window-width win) (sw/orig-prop name 'width))))))))

(Then "^window \\([A-Z]\\) should be the same \\(width\\|height\\) as window \\([A-Z]\\)$"
      (lambda (a dim b)
        (let ((a (sw/window-named a))
              (b (sw/window-named b)))
          (if (string= dim "width")
              (should (= (window-width a) (window-width b)))
            (should (= (window-height a) (window-height b)))))))

;; for sw/read-layout
(When "^I read the window layout:$"
      (lambda (layout)
        (setq sw/current-layout (sw/read-layout layout))))

(Then "^the layout should be \\(.+\\)$"
      (lambda (expected)
        (should (equal sw/current-layout (read expected)))))
