;; Code for reading and constructing the layouts used in the cucumber
;; features. This is remarkably convoluted code; probably moreso than
;; the implementation will end up being.

;; Reading a layout
(defun sw/lines (s)
  "Split into lines, retaining final newlines"
  (split-string s "^" 'omit-nulls))

(defun sw/join  (ss)
  "Join list of strings SS with newlines."
  (s-join "\n" ss))

(defun sw/leaf? (s)
  "t if S is a tree leaf (eg, a window, not an edge)"
  (not (s-matches-p "[-+|]" s)))

(defun sw/branch (s fn)
  "If S is a leaf, return it string value trimmed.
Otherwise, call FN with S as its only argument."
  (if (sw/leaf? s)
      (s-trim s)
    (funcall fn s)))

(defun sw/rm-outer-edge (layout)
  "Drop the outer border from a layout string."
  (let ((lines (split-string layout "\n" 'omit-nulls)))
    (--reduce-from (replace-regexp-in-string it "" acc)
                   (s-join "\n" (butlast (cdr lines)))
                   '("^[-+|]" "[-+|]$"))))

(defun sw/only-edges? (lines col)
  "t if column COL in an edge character in every line in LINES."
  (--all? (s-matches-p "[+|]" (substring it col (1+ col)))
          lines))

(defun sw/edges (lines)
  "Returns the indexes of all character columns in LINES which
consist solely of edges."
  (let ((width (length (car lines))))
    `(-1
      ,@(--select (sw/only-edges? lines it)
                  (number-sequence 0 (1- width)))
      0)))

(defun sw/collect-windows (lines left right)
  "Extract all windows named between columns LEFT and RIGHT."
  (--map (s-chomp (substring it (1+ left) (1- right))) lines))

(defun sw/map-cons (fn seq)
  "Call FN with each sequential pairing of elements in SEQ."
  (flet ((iter (rest) (when (> (length rest) 1)
                        (funcall fn (car rest) (cadr rest)))))
    (delete nil (maplist 'iter seq))))

(defun sw/split-cols (raw)
  "Collect all named windows or inner rows in RAW."
  (let* ((lines (sw/lines raw))
         (edges (sw/edges lines)))
    (-map 'sw/join
          (sw/map-cons (lambda (left right)
                         (sw/collect-windows lines left right))
                        edges))))

(defun sw/read-cols (raw)
  "Read the column elements within RAW."
  (let ((cols (--map (sw/branch it 'sw/read-rows)
                     (sw/split-cols raw))))
    (cons 'cols cols)))

(defun sw/split-rows (raw)
  "Split RAW into rows the full width of RAW."
  (split-string raw "^[-+]*\n"))

(defun sw/read-rows (raw)
  "Read the row elements within RAW."
  (let ((rows (--map (sw/branch it 'sw/read-cols)
                     (sw/split-rows raw))))
    (cons 'rows rows)))

(defun sw/read-layout (layout)
  "Given a string containing an ASCII representation of a window layout,
returns a data structure that represents it."
  (sw/read-rows (sw/rm-outer-edge layout)))

(defun sw/win-count (layout)
  "Returns the number of windows specified by LAYOUT."
  (length (sw/win-names layout)))

(defun sw/win-names (layout)
  (-select 'stringp (-flatten layout)))


;; Constructing a layout once it's been read.
(defun sw/anon-win-name ()
  "Return a random window name for an anonymous window. Currently just a
dumb use of `random'."
  (concat "parent-" (number-to-string (random 999999))))

(defun sw/split-fns (type count)
  (let ((split (if (eq 'cols type) 'split-window-right 'split-window-below)))
    (cons 'selected-window (-repeat (1- count) split))))

(defun sw/win-queue (type wins &optional split-from queue)
  "Returns a queue of windows to be constructed.
Each element is (NAME SPLIT-FROM-NAME FN INNER-WINS). Anonymous parent
windows will be assigned a randomly generated name."
  (let ((splits (-interleave wins (sw/split-fns type (length wins)))))
    (while (not (= 0 (length splits)))
      (let* ((win    (car splits))
             (fn     (cadr splits))
             (name   (if (stringp win) win (sw/anon-win-name)))
             (parent (or split-from (caar (last queue)))))
        (setq queue (append queue (list (list name parent fn win))))
        (setq split-from nil)
        (setq splits (cddr splits))))

    (-each (-reject (lambda (win) (or (null (car (last win))) (stringp (car (last win))))) queue)
           (lambda (win)
             (let ((contents (car (last win))))
               (setf (car (last win)) nil)
               (setq queue (sw/win-queue (car contents) (cdr contents) (car win) queue)))))

    queue))

(defun sw/mk-win (name fn)
  (let ((win (funcall fn)))
    (unless (s-matches? "^parent-" name)
      (with-selected-window win
        (switch-to-buffer (generate-new-buffer (concat "sw/" name)))))
    win))

(defun sw/mk-wins (queue)
  "Given a queue created by `sw/queue-wins', apply the splits.
Returns an alist mapping window names to their windows."
  (let ((wins))
    (dolist (item queue)
      (let ((name (car item))
            (from (nth 1 item))
            (fn   (nth 2 item)))
        (when from (select-window (cdr (assoc from wins))))
        (!cons (cons name (sw/mk-win name fn)) wins)))
    (nreverse wins)))

(defun sw/mk-layout (layout)
  "Given a window layout (see `sw/read-layout'), actually
create that in the current frame."
  (sw/mk-wins (sw/win-queue 'rows (cdr layout))))

(defun sw/win-info (win)
  "Return an alist of interesting information about WIN."
  `((height . ,(window-height win))
    (width  . ,(window-width  win))))

(defun sw/cache-win-infos (wins)
  "Given the output of `sw/mk-layout', stores position
information about all of the windows therein."
  (-map (lambda (elem)
          (let* ((name (car elem))
                 (win  (cdr elem))
                 (info (sw/win-info win)))
            (list name win info)))
        wins))
