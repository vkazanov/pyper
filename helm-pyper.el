;; A Pyper interface for Helm
;;

(require 'pyper)
(require 'helm)

(defvar helm-pyper-candidate-cache nil
  "Latest candidate cache")

(defvar helm-pyper-source
  '((name . "Helm Pyper")
    (candidates . helm-pyper-candidate-cache)
    (action . (("Go to" . goto-char))))
  "Basic Helm source using the Pyper parse tree")

(defun helm-pyper-build-candidates ()
  (let ((tree (pyper-parse)))
    (pyper-tree-walk tree 'helm-pyper-tag-visitor)))

(defun helm-pyper-sort-candidates ()
  (setq helm-pyper-candidate-cache
        (sort helm-pyper-candidate-cache
              (lambda (left right) (< (cdr left)
                                      (cdr right))))))

(defun helm-pyper-build-full-name (tag parents)
  (let* ((parents-names nil)
         (name (helm-pyper-tag-type-to-name tag)))
    (setq parents-names
          (mapcar
           (lambda (parent) (concat (helm-pyper-tag-type-to-name parent) "."))
           parents))
    (dolist (p-name parents-names)
      (setq name (concat p-name name)))
    (concat (format "%-9s"(symbol-name (pyper-tag-type tag)))
            " " name) ))

(defun helm-pyper-tag-type-to-name (tag)
  (let* ((type (pyper-tag-type tag))
         (face (cond
                ((eq type 'function) font-lock-function-name-face)
                ((eq type 'var) font-lock-variable-name-face)
                ((eq type 'class) font-lock-type-face)
                (t font-lock-string-face)))
         (name (pyper-tag-name tag)))
    (propertize name 'font-lock-face face)))

(defun helm-pyper-tag-visitor (tag parents)
  (let* ((pos (car (pyper-tag-pos tag)))
         (name (helm-pyper-build-full-name tag parents))
         (candidate (cons name pos)))
    (setq helm-pyper-candidate-cache
          (cons candidate helm-pyper-candidate-cache))))

(defun helm-pyper (prefix)
  "List Python definitions using Helm"
  (interactive "P")
  (setq helm-pyper-candidate-cache nil)
  (helm-pyper-build-candidates)
  (helm-pyper-sort-candidates)
  (let (input)
    (when prefix (setq input (thing-at-point 'word)))
    (save-restriction
      (helm
       :sources helm-pyper-source
       :buffer "*Outline*"
       :prompt "Name: "
       :candidate-number-limit nil
       :input input))))

(provide 'helm-pyper)
