;; QML mark up supoprt tools
;; for Qt ver. 5.2

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
;;   `qml-test-insert-property-declaration'
;;     prompt: number of properies, and each Qml type and name
;;
;;   `qml-test-insert-setup'
;;     prompt: number of properies to set up, and each name
;;
;;   `qml-test-insert-teardown'
;;     prompt: number of properies to tear down, and each name
;;
;;   `qml-test-insert-setup-teardown'
;;     prompt: as well as two above
;;
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
      (read-string prompt nil default-value)
    (let ((cont t) (in) (prompt1 prompt))
      (while cont
        (setq in (read-string prompt1 nil default-value))
        (if (setq cont (string-equal in ""))
            (setq prompt1 (concat "Cannot set empty. " prompt))))
      in)))

;; property
(defun qml-test-property-stab (qml-type prop-name dir &optional no-factory)
  (let ((prop
         (format "property %s %s" qml-type prop-name)))
    (if no-factory
        (concat prop "\n")
      (concat prop "\n"
              (format "property var %sFactory: Qt.createComponent(\"%s%s.qml\")\n"
                      prop-name (if dir (concat dir "/") "") qml-type)))))

(defun qml-test-property-prompt (&optional num no-qml-type with-dir)
  (let* ((num
          (if (and (numberp num) (> num 0))
              num
            (let ((cont t) (num) (prompt "How many properties?: "))
              (while cont
                (setq num (read-number prompt 1))
                (if (setq cont (<= num 0))
                    (setq prompt "Must be over 0: ")))
              num)))
         (type-name-list
          (let ((i 0) (tn))
            (while (< i num)
              (setq i (1+ i))
              (let ((dir (when with-dir
                           (let* ((n (1- (if qml-import-dir-list
                                             (read-number
                                              (concat (format "Directory(%s)  " i)
                                                      "0: (input new) / "
                                                      (let ((i 0))
                                                        (mapconcat
                                                         (lambda (d)
                                                           (let ((index (number-to-string (setq i (1+ i)))))
                                                             (concat index ": " d)))
                                                         qml-import-dir-list " / ")))
                                              1)
                                           0)))
                                  (d (if (> 0 n) nil (nth n qml-import-dir-list))))
                             (if d d
                               (let ((in (read-string (format "Directory(%s)  new directory: " i))))
                                 (if (string-match "^[ \t]*$" in) nil
                                   (setq qml-import-dir-list
                                         (cons in qml-import-dir-list))
                                   (message "%s" qml-import-dir-list)
                                   in))))))
                    (type (if no-qml-type
                              nil
                            (-string-prompt (format "Qml type(%s): " i))))
                    (name (-string-prompt (format "propety name(%s): " i) nil (format "prop%s" i))))
                (message "%s %s %s" dir type name)
                (add-to-list 'tn (list type name dir))))
            (reverse tn))))
    type-name-list))

(defun qml-test-insert-property-declaration (&optional num no-factory with-dir)
  (interactive "P")
  (let* ((tn-list (qml-test-property-prompt num nil with-dir)))
    (-insert-stab
     (reduce (lambda (acc tn)
               (concat acc
                       (qml-test-property-stab (car tn) (cadr tn) (caddr tn) no-factory)
                       "\n"))
             tn-list
             :initial-value ""))
    tn-list))

(defun qml-test-insert-property-declaration1 (&optional num no-factory)
  (interactive "P")
  (qml-test-insert-property-declaration num no-factory t))

(defun qml-test-insert-property-declaration-no-factory (&optional num)
  (interactive "P")
  (qml-test-insert-property-declaration num t))

;; setup / teardown
(defun qml-test-setup-property-stab (prop-name)
  (concat (format "if (%s != null) {\n" prop-name)
          "teardown()\n}\n"
          (format "verify(%sFactory != null)\n" prop-name)
          (format "compare(%sFactory.status, Component.Ready)\n" prop-name)
          (format "%s = %sFactory.createObject(this)\n" prop-name prop-name)))

(defun qml-test-teardown-property-stab (prop-name)
  (concat (format "if (%s != null) {\n" prop-name)
          (format "%s.destroy()\n" prop-name)
          (format "%s = null\n}\n" prop-name)))

(defun qml-test-insert-setup-teardown-internal (property-name-list fun-format stab-fun &optional num prompt)
  (let* ((name-list (if property-name-list property-name-list
                      (when (or num prompt)
                        (mapcar 'cadr (qml-test-property-prompt num t))))))
    (-insert-stab
     (format fun-format
             (mapconcat stab-fun name-list "\n")))
    name-list))

(defun qml-test-insert-setup (&optional n property-name-list)
  (interactive "P")
  (qml-test-insert-setup-teardown-internal property-name-list
                                           "function setup() {\n%s}\n"
                                           'qml-test-setup-property-stab
                                           n n))

(defun qml-test-insert-teardown (&optional n property-name-list)
  (interactive "P")
  (qml-test-insert-setup-teardown-internal property-name-list
                                           "function teardown() {\n%s}\n"
                                           'qml-test-teardown-property-stab
                                           n n))

(defun qml-test-insert-setup-teardown (&optional n property-name-list)
  (interactive "P")
  (let* ((property-name-list (qml-test-insert-setup n property-name-list)))
    (insert "\n")
    (qml-test-insert-teardown n property-name-list)))

;; test function
(defun qml-test-function-stab (fun-name)
  (concat (format "function test_%s() {\n" fun-name)
          "setup()\n\n"
          "teardown()\n}\n"))

(defun qml-test-insert-test-function ()
  (interactive)
  (-insert-stab (qml-test-function-stab
                 (-string-prompt "Function name (instead \"test_\" prefix): ")))
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

(defun qml-test-insert-signal-spy ()
  (interactive)
  (-insert-stab (qml-test-signal-spy-stab
                 (-string-prompt "Taget: ")
                 (-string-prompt "Signal name: "))))

(provide 'qml-tools)
