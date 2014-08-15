;; QML mark up supoprt tools
;; for Qt ver. 5.x

;; QML preprocessors
;;
;; `qml-insert-preprocessors' function inserts preprocessors of importing
;; package (like "import QtQtuick 2.2") at current position.
;;
;; Preprocessors are defined by `qml-preprocessor-alist',
;; default values are below:
;;     ("QtQuick" . "2.2")
;;     ("QtQuick.Controls" . "1.1")
;;     ("QtQuick.Layouts" . "1.1")
;;
;; (each element must be tuple)
;;
;; If you want to add a package, call `qml-add-preprocessor' function,
;; which require two arguments (package and version by string).
;; When called this, sorts `qml-preprocessor-alist' by package name with asc.
;; Also, want to remove one, call `qml-remove-preprocessor' function.
;; ex:
;;     (qml-add-preprocessor "QtQuick.Controls.Styles" "1.1")
;;     (qml-remove-preprocessor "QtQuick.Controls")
;;
;; Basic packages instead default:
;;     QtQuick.Controls.Styles 1.1
;;     QtQuick.Dialogs 1.1
;;     QtQuick.Window 2.0

(defvar qml-preprocessor-alist '())

(defun qml-add-preprocessor (package version)
  (let ((elm (cons package version)))
    (if (and (stringp package)
             (stringp version)
             (not (member elm qml-preprocessor-alist)))
        (setq qml-preprocessor-alist
              (sort (add-to-list 'qml-preprocessor-alist elm)
                    (lambda (a b)
                      (string< (car a) (car b))))))))

(defun qml-remove-preprocessor (package)
  (if (stringp package)
      (setq qml-preprocessor-alist
            (remove-if (lambda (p)
                         (string-equal (car p) package))
                       qml-preprocessor-alist))))

(let ((preprocessor-list
       (list
        '("QtQuick"                   . "2.2")
        '("QtQuick.Controls"          . "1.1")
        '("QtQuick.Layouts"           . "1.1")
        )))
  (mapcar (lambda (p)
            (qml-add-preprocessor (car p) (cdr p)))
          preprocessor-list))

(defun qml-insert-preprocessors (&optional not-sort)
  (interactive)
  (mapcar (lambda (preprocessor)
            (insert (format "import %s %s\n" (car preprocessor) (cdr preprocessor))))
          qml-preprocessor-alist))


;; QML Test
;;
;; Stabs for testing custom Qml type.
;;
;; Main use of interactive functions:
;;   `qml-test-insert-test-function'
;;     prompt: function name without "test_" prefix
;;
;;   `qml-test-insert-signal-spy'
;;     prompt: target object (id) and siganal name to spy
;;

;; common - insert stab and indent region
(defun -insert-stab (stab)
  (let ((begin (point)))
    (insert stab)
    (indent-region begin (point))))

(defvar qml-import-dir-list '())

;; common - reading string prompt which does not allow empty by default
(defun -string-prompt (prompt &optional allow-empty default-value)
  (if allow-empty
      (read-string prompt default-value)
    (let ((cont t) (in) (prompt1 prompt))
      (while cont
        (setq in (read-string prompt1 default-value))
        (if (setq cont (string-equal in ""))
            (setq prompt1 (concat "Cannot set empty. " prompt))))
      in)))

;; test function
(defun qml-test-function-stab (fun-name)
  (concat (format "function test_%s() {\n" fun-name)
          "setup()\n\n"
          "teardown()\n}\n"))

(defun qml-test-insert-test-function (&optional name)
  "Snippet of QML test function.
If optional argument NAME is nil, prompt ask funtion name."
  (interactive)
  (-insert-stab (qml-test-function-stab
                 (if (stringp name) name
                   (-string-prompt "Function name (instead \"test_\" prefix): "))))
  (beginning-of-line -2)
  (indent-for-tab-command))

;; signal spy
(defun qml-test-signal-spy-stab (target signal)
  (concat "SignalSpy {\n"
          (format "id: %s%sSpy\n" target
                  (concat (capitalize (substring signal 0 1))
                          (if (> (length signal) 1) (substring signal 1) "")))
          (format "target: %s\n" target)
          (format "signalName: \"%s\"\n}\n" signal)))

(defun qml-test-insert-signal-spy (&optional target signal-name)
  "Snippet of static declaration of signal spy.
If both optional arguments TARGET and SIGNAL-NAME are non-nil,
insert declaration without asking itsm, otherwise, ask target and signal name."
  (interactive)
  (let* ((tar (if (stringp target) target nil))
         (sig (if (stringp signal-name) signal-name nil))
         (tar-name (if (and tar sig) (cons tar sig)
                     (message "%s %s" tar sig)
                     (cons (-string-prompt "Taget: " nil tar)
                           (-string-prompt "Signal name: " nil sig)))))
    (-insert-stab (qml-test-signal-spy-stab (car tar-name) (cdr tar-name)))))

(provide 'qml-tools)
