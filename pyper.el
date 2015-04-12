;; Pyper (Python Primitive Parser) - a primitive recursive decent Python parser
;;
;; Incomplete, with no proper tokenization, full of hacks, workarounds and dead
;; kitten. Fast enough.

(defconst pyper-max-parse-depth 3
  "Control the depth of recursion, the lower is the value, the
  faster goes the parsing")

(defconst pyper-python-keywords
  '("try" "if" "except" "else" "finally" "raise" "yield" "import" "from" "pass")
  "A list of keyword used only to workaround all the complicated cases")

(defvar pyper-whitespaces "[:blank:]"
  "Whitespace char group for skipping")

(defvar pyper-parse-done-hook nil
  "A hook for functions catching parse results. Such a function
  should receive a parse tree and a full file path as it's
  arguments")

(defvar-local pyper-current-indent-level 0
  "Current indentation level used in parsing")

(defvar-local pyper-indent-string nil
  "Indentation string (a tab or just a number of whitespaces).
  Will be set on the fly")

;;
;; Interface functions
;;

(defun pyper-parse ()
  "Parser entry point. Parses the buffer, returns the parse tree."
  (save-excursion
    (beginning-of-buffer)
    (let ((tree (pyper-consume)))
      (run-hook-with-args 'pyper-parse-done-hook tree buffer-file-name)
      tree)))

(defun pyper-tree-walk (tree tag-visitor)
  "Walks (pre-order tree traversal) the pyper parse tree. Tag
  visitor(usually a lambda) is a function accepting a tag with a
  list of it's parents"
  (dolist (tag tree)
    (pyper-tree-walk-impl tag tag-visitor)))

(defun pyper-tree-walk-impl (tag tag-visitor &optional tag-parents)
  ;; visit parent
  (funcall tag-visitor tag tag-parents)
  ;; visit extract children, visit them in turn
  (let ((tag-children (pyper-tag-children tag))
        (tag-new-parents (cons tag tag-parents)))
    (dolist (tag-child tag-children)
      (pyper-tree-walk-impl tag-child tag-visitor tag-new-parents))))

;; NOTE: maybe a vector would be more reasonable? in theory, it should have a
;; faster element access time, less GC problems
(defun pyper-tag-construct (type name pos-start pos-end &optional children)
  (list type name pos-start pos-end children))

(defun pyper-tag-type (tag)
  (nth 0 tag))

(defun pyper-tag-name (tag)
  (nth 1 tag))

(defun pyper-tag-pos (tag)
  (list (nth 2 tag) (nth 3 tag)))

(defun pyper-tag-children (tag)
  (nth 4 tag))

;;
;; The parser itself
;;

(defun pyper-consume (&optional depth)
  (let ((depth (if (not depth) 0 depth))
        (res))
    (while (and (<= depth pyper-max-parse-depth)
                (pyper-consume-indent)
                (not (equal (point) (point-max))))
      (let ((cur-token (current-word))
            (cur-char (char-after))
            (next-char (char-after (1+ (point)))))
            (cond
             ;; skip an innner block

             ;; skip inner blocks and closing braces and parentheses
             ;; TODO: a dirty hack
             ((member cur-char '(?\t ?\ ?\} ?\] ?\)))
              (forward-line))

             ;; opening braces
             ;; TODO: a dirty hack
             ((member cur-char '(?\( ?\{ ?\[))
              (forward-sexp))

             ;; skip docstrings/strings/unicode/raw strings
             ((or (member cur-char '(?\" ?\'))
                  (and (member cur-char '(?r ?u)) (member next-char '(?\" ?\'))))
              (forward-sexp)
              (forward-line))

             ;; skip comments
             ((equal cur-char ?#)
              (forward-line))

             ;; skip decorators
             ((equal cur-char ?@)
              (forward-line))

             ((equal cur-token "return")
              (pyper-consume-str "return")
              (pyper-consume-whitespace)
              (cond ((equal (char-after) ?\n)
                     (forward-line))
                    (t (forward-sexp))))

             ;; parse a class, add the tag to the result list
             ((equal cur-token "class")
              (let ((class-tag (pyper-consume-class depth)))
                (cond (class-tag (setq res (cons class-tag res))))))

             ;; parse a function, add to the result list
             ((equal cur-token "def")
              (let ((def-tag (pyper-consume-def depth)))
                (cond (def-tag (setq res (cons def-tag res))))))

             ;; skip the keywords (if/try/except/else/finally/etc) for now
             ;; (rather easy to parse, but no need probably)
             ((member cur-token pyper-python-keywords)
              (forward-line))

             ;; something else, like a declaration...
             ;; TODO: need a proper decl parser
             ((let* ((pos-start (point))
                    (name (pyper-consume-name))
                    (names (list (cons name pos-start)))
                    (pos-end (point-at-eol))
                    (continue t))
                (while continue
                  (cond ((pyper-consume-char ?=)
                         (dolist (name names)
                           (setq res (cons (pyper-tag-construct 'var (car name) (cdr name) pos-end) res)))
                         (setq continue nil))
                        ((pyper-consume-char ?,)
                         (if (not (pyper-consume-char ?= t))
                             (let ((pos-start (point))
                                   (name (pyper-consume-name)))
                               (setq names (cons (cons name pos-start) names)))))
                        (t (setq continue nil))))))

             ;; skip everything else
             (t
              (forward-line)))))
    res))

(defun pyper-consume-whitespace ()
  (skip-chars-forward pyper-whitespaces))

(defun pyper-consume-indent ()
  (let ((indents-to-skip pyper-current-indent-level)
        (continue t)
        (res t))
    (while continue
      (cond
       ;; just ignore newlines
       ((equal (char-after) ?\n)
        (forward-char)
        (setq indents-to-skip pyper-current-indent-level))
       ;; pyper-consume one indentation level
       ((and (or (equal (char-after) ?\t)
                 (equal (char-after) ?\ ))
             (> indents-to-skip 0))
        (pyper-consume-indent-level)
        (setq indents-to-skip (1- indents-to-skip)))
       ;; pyper-consumed the right number of levels -> the block is still not over
       ((= indents-to-skip 0)
        (setq continue nil))
       ;; could not pyper-consume enough levels? the block is over
       (t
        (beginning-of-line)
        (setq continue nil
              res nil))))
    res))

(defun pyper-consume-indent-level ()
  (cond
   ;; indent style not yet known? Let's find out
   ((not pyper-indent-string)
    (cond
     ;; just a tab
     ((equal (char-after) ?\t)
      (setq pyper-indent-string "\t"))
     ;; count spaces and build an indent string
     ((equal (char-after) ?\ )
      (let ((space-num 0))
        (while (equal (char-after) ?\ )
          (forward-char)
          (setq space-num (1+ space-num)))
        (setq pyper-indent-string (make-string space-num ?\ ))))
     (error "Wrong indentation sybmol"))))
  (pyper-consume-str pyper-indent-string t))

(defun pyper-consume-char (char &optional no-consume-char)
  (pyper-consume-whitespace)
  (cond ((equal (char-after) char)
         (if (not no-consume-char) (forward-char)) t)
        (t nil)))

(defun pyper-consume-str (str &optional no-consume-whitespace)
  "Pyper-Consume a string if possible"
  (cond ((not no-consume-whitespace) (pyper-consume-whitespace)))
  (let ((cur-num 0)
        (len (length str))
        (continue t)
        (res t))
    (while (and continue (< cur-num len))
      (cond ((equal (aref str cur-num) (char-after))
             (setq cur-num (1+ cur-num))
             (forward-char))
            (t
             (setq continue nil
                   res nil))))
    res))

(defun pyper-consume-name ()
  (pyper-consume-whitespace)
  (let ((cur-token (current-word)))
    (forward-symbol 1)
    (if cur-token
        cur-token
      (error "%s" "Failed to parse a symbol"))))

(defun pyper-consume-body (depth)
    (pyper-consume depth))

(defun pyper-consume-suite (depth)
  (pyper-consume-whitespace)
  (let ((cur-char (char-after))
        (res))
    (cond
     ;; TODO: a dirty hack
     ((not (equal cur-char ?\n))
      (forward-line))
     (t
      (forward-char)
      (setq pyper-current-indent-level (1+ pyper-current-indent-level))
      (setq res (pyper-consume-body depth))
      (setq pyper-current-indent-level (1- pyper-current-indent-level))))
    res))

(defun pyper-consume-class (depth)
  (pyper-consume-whitespace)
  (pyper-consume-str "class")
  (let ((pos-start (point))
        (name (pyper-consume-name)))
    (pyper-consume-whitespace)
    ;; we do not care about inheritance
    (cond ((equal (char-after) ?\( )
           (forward-sexp)))
    (pyper-consume-char ?:)
    ;; construct a tag
    (let ((children (pyper-consume-suite (1+ depth)))
          (pos-end (point)))
      (pyper-tag-construct 'class name pos-start pos-end children))))

(defun pyper-consume-def (depth)
  (pyper-consume-whitespace)
  (pyper-consume-str "def")
  (let ((pos-start (point))
        (name (pyper-consume-name)))
    ;; let's just skip args for now
    (forward-sexp)
    (pyper-consume-char ?:)
    ;; construct a tag
    (let ((children (pyper-consume-suite (1+ depth)))
          (pos-end (point)))
      (pyper-tag-construct 'function name pos-start pos-end children))))

(provide 'pyper)
