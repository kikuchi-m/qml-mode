;; QML Mode

(defvar qml-mode-hook nil)

(let ((qml-highlight-blue "MediumBlue")      ;#0000cd
      (qml-highlight-orchid "DarkOrchid")    ;#9932cc
      (qml-highlight-olive "OliveDrab")      ;#6b8e23
      (qml-highlight-red "red4")             ;#8b0000
      )

  (defface qml-specifier-face
    `((t :foreground ,qml-highlight-blue))
    "Face for element specifier.")
  (defvar qml-specifier-face 'qml-specifier-face)

  (defface qml-preprocessor-face
    `((t :foreground ,qml-highlight-orchid))
    "Face for preprocessor.")
  (defvar qml-preprocessor-face 'qml-preprocessor-face)

  (defface qml-package-face
    `((t :foreground ,qml-highlight-olive))
    "Face for package name.")
  (defvar qml-package-face 'qml-package-face)

  (defface qml-package-version-face
    `((t :foreground ,qml-highlight-red))
    "Face for package version.")
  (defvar qml-package-version-face 'qml-package-version-face)
  )

(defvar qml-syntax-table
  (let ((qml-st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" qml-st)
    (modify-syntax-entry ?* ". 23" qml-st)
    (modify-syntax-entry ?\n "> b" qml-st)
    (modify-syntax-entry ?' "\"" qml-st)
    qml-st)
  "Syntax table for qml-mode.")

(defvar qml-font-lock-keywords
  (let* ((separator "\\|")
         (qml-directive-kwd
          (mapconcat 'identity '("import" "using") separator))
         )
    (list
     ;; preprocessor
     (list (concat "^[ \t]*\\(" qml-directive-kwd "\\)[ \t]+"
                   "\\("
                   "\\(\\(\\([a-zA-Z][a-zA-Z0-9]*\\)\\.?\\)*\\([a-zA-Z][a-zA-Z0-9]*\\)\\)" ;; packages
                   "[ \t]+"
                   "\\(\\(\\([0-9]+\\)\\.?\\)*\\([0-9]+\\)\\)" ;; version
                   "\\|"
                   "\\(\"[^ ]+\"\\)" ;; directory
                   "\\)"
                   "[ \t]*;?$")
           '(1 qml-preprocessor-face nil t)
           '(3 qml-package-face nil t)
           '(7 qml-package-version-face nil t))

     ;; specifiers
     (list (concat "\\(^\\|\\*/\\|:[ \t]*\\)"
                   "[ \t]*"
                   "\\([A-Z][a-zA-Z0-9]*\\)[ \t]*\\({\\|$\\)")
           2 qml-specifier-face)

     ;; props
     (list (concat "\\(^[ \t]*\\|;[ \t]*\\|{[ \t]*\\)"
                   "\\(\\([a-zA-Z][a-zA-Z0-9]*\\)\\.\\)?\\([a-z][a-zA-Z0-9]*\\)"
                   "[ \t]*[:{]")
           '(3 font-lock-variable-name-face nil t)
           '(4 font-lock-variable-name-face nil t))
     )))

(defun qml-beginning-of-block-internal (prev cur)
  "This function used in `qml-beginning-of-block' function.
If want the position of beginning of block, use it."
  (if (not prev) nil
    (let* ((prev1
            (save-excursion
              (goto-char prev)
              (if (search-backward-regexp "\\({\\|\\[\\)[ \t]*$" nil t)
                  (match-beginning 1)
                nil)))
           (end-of-prev1 (if (not prev1) nil
                   (condition-case nil
                       (save-excursion (goto-char prev1)
                                       (forward-sexp)
                                       (point))
                     (error nil)))))
      (if (not prev1) nil
        (if (or (not end-of-prev1)
                (and (> cur prev1)
                     (< (save-excursion
                          (goto-char cur) (end-of-line) (point))
                        end-of-prev1)))
            prev1
          (qml-beginning-of-block-internal prev1 cur))))))

(defun qml-beginning-of-block (&optional cur)
  "Get position of beginning of block.
Return the position of beginning parenthesis \"{\" if found it, otherwise return nil.
If CUR is nil, try find beginning of block from current position.
In the line CUR is in, an item is declared single line (like that
\"Item { id:item1 ... }\" is in a line.), return the position
of beginning of \"item1\"'s parent block."
  (let* ((p (if (numberp cur) cur (point)))
         (qbob (qml-beginning-of-block-internal
                (save-excursion (goto-char p)
                                (beginning-of-line)
                                (point))
                p)))
    (if (not qbob)
        (display-message-or-buffer "Not found beginning of block"))
    qbob))

(defvar qml-indent-offset 2)

(defun qml-indent-line ()
  "Indent current line according to QML indentation rule."
  (interactive)
  (let* ((qbob (qml-beginning-of-block (point)))
         (pi (if (not qbob) nil
               (save-excursion
                 (goto-char qbob)
                 (current-indentation)))))
    (if (not pi) (indent-line-to 0)
      (let ((ni (+ pi qml-indent-offset))
            (ci (current-indentation)))
        (if (not (eq ni ci)) (save-excursion (indent-line-to ni)))
        (let ((lbp (line-beginning-position)))
          (if (< (- (point) lbp) ni) (goto-char (+ lbp ni))))))
    ))

(define-derived-mode qml-mode fundamental-mode "QML"
  "Mejor mode for Qt declarative UI (simple mode)"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table qml-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(qml-font-lock-keywords))
  (set (make-local-variable 'tab-width) qml-indent-offset)
  (set (make-local-variable 'indent-line-function) 'qml-indent-line)
  (setq major-mode 'qml-mode)
  (setq mode-name "QML")
  (run-hooks 'qml-mode-hook)
  )

(provide 'qml-mode)
