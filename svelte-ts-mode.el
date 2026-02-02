;;; svelte-ts-mode.el --- Emacs major mode for Svelte -*- lexical-binding:t -*-
;; Copyright (C) 2026 Leaf.

;; Author: Leaf <leafvocation@gmail.com>
;; Author: Meow King <mr.meowking@posteo.com>
;; Author: jeff-phil <jeff@jeffphil.com>
;; Created: 5 Dec 2024
;; Keywords: svelte languages tree-sitter
;; Homepage: https://github.com/leafOfTree/svelte-ts-mode
;; Version: 1.2.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.
;; You can redistribute it and/or modify it under the terms of
;; the GNU Lesser General Public License v3.0.

;;; Commentary:

;; This major mode includes typescript-ts-mode and css-mode
;; to support basic treesit svelte syntax and indent via the
;; standard tree-sitter-grammars/tree-sitter-svelte grammar.

;;; Code:

(require 'treesit)

(defgroup svelte-ts nil
  "`svelte-ts-mode'."
  :group 'languages)

(defcustom svelte-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `svelte-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'svelte-ts
  :package-version '(svelte-ts-mode . "1.0.0"))

(defconst svelte-ts-mode-language-source-alist
  '((svelte . ("https://github.com/tree-sitter-grammars/tree-sitter-svelte"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil
                   "typescript/src"))
    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
    (css . ("https://github.com/tree-sitter/tree-sitter-css")))
  "Language source for `svelte-ts-mode'.")

(defcustom svelte-ts-mode-enable-comment-advice (< emacs-major-version 31)
  "Enable language-wise comment inside `svlete-ts-mode' buffer.
Note only Emacs < 31 needs to set this option."
  :type 'boolean
  :safe 'booleanp
  :group 'svelte-ts
  :package-version '(svelte-ts-mode . "1.0.0"))

(defcustom svelte-ts-mode-dont-bind-keys nil
  "Non-nil means do not bind keys in svelte-ts-mode."
  :type 'boolean
  :safe 'booleanp
  :group 'svelte-ts
  :package-version '(svelte-ts-mode . "1.2.0"))

(defconst svelte-ts-mode--indent-close-types-regexp
  (concat
   "\\`"
   (regexp-opt
    '(">" "/>" "}"
      "end_tag"
      ;; structural closing tags
      "if_end" "key_end" "each_end" "await_end" "snippet_end"
      ;; intermediate blocks that visually closes previous scope
      "else_start" "else_if_start" "then_start" "catch_start"
      ))
   "\\'"))

(defconst svelte-ts-mode--container-node-types-regexp
  (concat
   "\\`"
   (regexp-opt
    '(; html
      "element" "script_element" "style_element"
      "start_tag" ;; needed for multi-line attributes

      ;; svelte structural containers
      "if_statement" "if_start" "if_end"
      "else_block" "else_start"
      "else_if_block" "else_if_start"

      ;; key
      "key_statement" "key_start" "key_end"

      ;; each
      "each_statement" "each_start" "each_end"

      ;; await
      "await_statement" "await_start" "await_end"
      "then_block" "then_start"
      "catch_block" "catch_start"

      ;; snippet
      "snippet_statement" "snippet_start" "snippet_end"

      ;; misc expression (attach is newer/future)
      "expression" "html_tag" "const_tag" "render_tag" "debug_tag" "attach_tag"))
   "\\'"))

(defvar svelte-ts-mode-map
  (let* ((map (make-sparse-keymap)))
    (unless svelte-ts-mode-dont-bind-keys
      (set-keymap-parent map prog-mode-map)
      (keymap-set map "C-c /" #'svelte-ts-mode-close-tag)
      (keymap-set map "C-c C-s" #'svelte-ts-mode-mark-element)
      (keymap-set map "C-c C-SPC" #'svelte-ts-mode-mark-element)
      (keymap-set map "C-c C-d" #'svelte-ts-mode-delete-element)
      (keymap-set map "C-c C-n" #'svelte-ts-mode-move-next-tag)
      (keymap-set map "C-c C-p" #'svelte-ts-mode-move-prev-tag)
      (keymap-set map "C-c C-f" #'svelte-ts-mode-move-next-same-tag)
      (keymap-set map "C-c C-b" #'svelte-ts-mode-move-prev-same-tag)
      (keymap-set map "C-c <left>" #'svelte-ts-mode-skip-tag-backward)
      (keymap-set map "C-c C-a" #'svelte-ts-mode-skip-tag-backward)
      (keymap-set map "C-c <right>" #'svelte-ts-mode-skip-tag-forward)
      (keymap-set map "C-c C-e" #'svelte-ts-mode-skip-tag-forward)
      map))
  "Svelte Tree-Sitter Mode command keys.  Set `svelte-ts-mode-dont-bind-keys' to
non-nil to not setup keys.")

(defvar svelte-ts-mode-prettify-symbols-alist
  '(("&nbsp;" . ?␠)  ; show this as an underline, or gets lost
    ("&lt;" . ?<)
    ("&gt;" . ?>)
    ("&quot;" . ?\")
    ("&apos;" . ?\')
    ("&amp;" . ?&)
    ("&copy;" . ?©)
    ("&trade;" . ?™)
    ("&bull;" . ?•)
    ("&middot;" . ?·)
    ("&rarr;" . ?→)
    ("&larr;" . ?←)
    ("&uarr;" . ?↑)
    ("&darr;" . ?↓)
    ("&harr;" . ?↔)
    ("&varr;" . ?↕)
    ("&lambda;" . ?λ)
    ("&pi;" . ?π)
    ("&phi;" . ?φ)
    ("&le;" . ?≤)
    ("&ge;" . ?≥)
    ("&ne;" . ?≠)
    ("=>" . ?⇒))
  "Prettify symbols for Svelte.")

(defconst svelte-ts-mode-font-lock-features-list
  '((error definition comment keyword)
    (property string)
    (punctuation)
    ()))

(defvar-local svelte-ts-mode--treesit-buffer-ready
    '((svelte . nil)
      (javascript . nil)
      (typescript . nil)
      (css . nil))
  "Memoized treesit-ready-p output for required modes in a buffer local alist.")

(defun svelte-ts-mode--treesit-buffer-ready (mode)
  "Returns memoized value of treesit-ready-p."
  (let ((cell (assoc mode svelte-ts-mode--treesit-buffer-ready)))
    (if cell
        (or (cdr cell) (setcdr cell (treesit-ready-p mode)))
      (error "Unsupported treesit mode"))))

(defun svelte-ts-mode--resolve-keywords (node-type known-keywords &optional newer-keywords)
  "Return a vector of keywords valid for NODE-TYPE. Always returns KNOWN-KEYWORDS.
Only includes NEWER-KEYWORDS if the current grammar library supports them."
  (apply #'vector
         (append
          known-keywords
          (when (treesit-ready-p 'svelte)
            (seq-filter
             (lambda (k)
               ;; Try to compile a query with this specific keyword.
               ;; If it fails (returns nil), the grammar doesn't support it.
               (ignore-errors
                 (treesit-query-compile 'svelte `((,node-type ,k @test)) t)
                 t))
             newer-keywords)))))

(defun svelte-ts-mode--font-lock-settings ()
  "Generate font-lock settings for svelte language."
  ;;This is a function so it can check for grammar support at runtime
  (treesit-font-lock-rules
   :language 'svelte
   :feature 'error
   '((erroneous_end_tag_name) @font-lock-warning-face)

   :language 'svelte
   :feature 'definition
   '((tag_name) @font-lock-function-name-face
     (snippet_name) @font-lock-function-name-face)

   :language 'svelte
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'svelte
   :feature 'keyword
   `((block_start_tag ["if" "each" "key" "snippet" "await"] @font-lock-keyword-face)
     (block_end_tag   ["if" "each" "key" "snippet" "await"] @font-lock-keyword-face)
     (block_tag       ["else" "else if" "then" "catch"]     @font-lock-keyword-face)

     ;;  NOTE: - can future keywords like below with above as needed...
     ;; This is to support future attach keyword, when ready by parser lib see
     ;; https://github.com/tree-sitter-grammars/tree-sitter-svelte/pull/20
     ;; and/or can go ahead and apply patch and use and it will work
     (expression_tag
      ,(svelte-ts-mode--resolve-keywords
        'expression_tag
        '("html" "debug" "render" "const") ; known list
        '("attach"))                       ; new/future list
      @font-lock-keyword-face)

     "as" @font-lock-keyword-face
     "doctype" @font-lock-keyword-face)

   :language 'svelte
   :feature 'punctuation
   '(["{" "}"] @font-lock-bracket-face
     "=" @font-lock-operator-face
     ["<" ">" "</" "/>" "<!"] @font-lock-delimiter-face
     ["#" ":" "/" "@"] @font-lock-misc-punctuation-face ; separate these out as special
     (expression ["{" "}"] @font-lock-misc-punctuation-face)
     (entity) @font-lock-constant-face) ; html char entities, e.g. &nbsp; &copy;

   :language 'svelte
   :feature 'property
   '((attribute_name) @font-lock-property-name-face)

   :language 'svelte
   :feature 'string
   :override t
   '((expression ["{" "}"] @font-lock-misc-punctuation-face)
     (quoted_attribute_value ["\"" "'"] @font-lock-string-face)
     (quoted_attribute_value (attribute_value) @font-lock-string-face)
     ((expression) @svelte-ts-mode--unfontify) ;; see calls this named func
     (attribute (attribute_value) @font-lock-string-face)))) ;; unquoted: `type=checkbox'

(defun svelte-ts-mode--unfontify (node override start end &rest _)
  "Remove face and font-lock-face properties from the range of NODE."
  ;; Used for to remove the specific faces while keeping other properties safe
  ;; for {expression} text in svelte string rules to allow capture of js expressions
  ;; in strings to still show in string override.
  (remove-text-properties
   (treesit-node-start node) (treesit-node-end node) '(face nil font-lock-face nil)))

(defun svelte-ts-mode--indent-inside-container-nodes-p (_node parent _bol)
  "Whether the ancestor node(also itself) of PARENT is of container node type.
NODE, PARENT and BOL see `treesit-simple-indent-rules'."
  (treesit-parent-until
   ;; NODE can be nil (hit return), so we use PARENT
   parent
   (lambda (node)
     (string-match-p
      svelte-ts-mode--container-node-types-regexp
      (treesit-node-type node)))
   t))


(defun svelte-ts-mode--indent-ancestor-container-nodes-bol (node parent bol)
  "Return the beginning of line position of the closest ancestor container node.
NODE, PARENT and BOL see `treesit-simple-indent-rules'."
  (save-excursion
    (goto-char
     (treesit-node-start
      (svelte-ts-mode--indent-inside-container-nodes-p node parent bol)))
    (back-to-indentation)
    (point)))

(defun svelte-ts-mode--indent-close-type-find-matching-parent-bol (node parent bol)
  (let* ((node-type (treesit-node-type node))
         (parent-node-raw-regexp  ; `nil' means direct parent is the matching parent
          (pcase node-type
            ;; if
            ("else_if_start" "if_statement")
            ("else_start" "if_statement")
            ("if_end" "if_statement")

            ;; key
            ("key_end" "key_statement")

            ;; each
            ("each_end" "each_statement")

            ;; await
            ("then_start" "await_statement")
            ("catch_start" "await_statement")
            ("await_end" "await_statement")

            ;; snippet
            ("snippet_end" "snippet_statement")

            ;; Fallback for older variations or nested structure
            (_ nil)))
         (parent-node-raw-regexp
          (when parent-node-raw-regexp
            (concat "\\`" parent-node-raw-regexp "\\'"))))
    (save-excursion
      (goto-char
       (treesit-node-start
        (if (not parent-node-raw-regexp)
            parent
          (treesit-parent-until
           parent
           (lambda (node)
             (string-match-p parent-node-raw-regexp (treesit-node-type node)))
           t))))
      (back-to-indentation)
      (point))))


(defvar svelte-ts-mode--indent-rules
  `((svelte
     ;; ((lambda (node parent bol)
     ;;    (message "%s: %s %s %s %s %s"
     ;;             (point) node parent
     ;;             (treesit-node-parent parent)
     ;;             (treesit-node-parent (treesit-node-parent parent)) bol)
     ;;    nil)
     ;;  parent-bol 0)

     ((parent-is "document") column-0 0)
     ((parent-is "comment") prev-adaptive-prefix 0)

     ((node-is ,svelte-ts-mode--indent-close-types-regexp)
      svelte-ts-mode--indent-close-type-find-matching-parent-bol 0)

     (svelte-ts-mode--indent-inside-container-nodes-p
      svelte-ts-mode--indent-ancestor-container-nodes-bol
      svelte-ts-mode-indent-offset)))
  "Tree-sitter indent rules.")

;; NOTE it's a must to prefix features in Emacs 30. Otherwise embedded CSS code
;; cannot be highlighted correctly. See
;; https://github.com/leafOfTree/svelte-ts-mode/issues/7
(defun svelte-ts-mode--prefix-font-lock-features (prefix settings)
  "Prefix with PREFIX the font lock features in SETTINGS."
  (cl-loop for i from 0 below (length settings)
           collect
           (mapcar (lambda (f) (intern (format "%s-%s" prefix f)))
                   (nth i settings))))

(defun svelte-ts-mode--prefix-font-lock-settings-features-name (prefix settings)
  "Prefix with PREFIX the font lock features in SETTINGS."
  (mapcar (lambda (setting)
            (pcase setting
              (`(,lang ,query ,feature . ,rest)
               `(,lang ,query ,(intern (format "%s-%s" prefix feature)) . ,rest))))
          settings))

(defun svelte-ts-mode--merge-font-lock-features (a b)
  "Merge `treesit-font-lock-feature-list' A with B."
  (let* ((len-a (length a))
         (len-b (length b))
         (max-len (max len-a len-b)))
    (cl-loop for i from 0 below max-len
             collect (seq-uniq
                      (append
                       (and (< i len-a) (nth i a) (nth i a))
                       (and (< i len-b) (nth i b) (nth i b)))))))

;; Copied from Emacs 31's `treesit-simple-indent-modify-rules'
(defun svlete-ts-mode--simple-indent-modify-rules (lang new-rules rules &optional how)
  "Pick out rules for LANG in RULES, and modify it according to NEW_RULES.

RULES should have the same form as `treesit-simple-indent-rules', i.e, a
list of (LANG RULES...).  Return a new modified rules in the form
of (LANG RULES...).

If HOW is omitted or :replace, for each rule in NEW-RULES, find the old
rule that has the same matcher, and replace it.

If HOW is :prepend, just prepend NEW-RULES to the old rules; if HOW is
:append, append NEW-RULES."
  (cond
   ((not (alist-get lang rules))
    (error "No rules for language %s in RULES" lang))
   ((not (alist-get lang new-rules))
    (error "No rules for language %s in NEW-RULES" lang))
   (t (let* ((copy-of-rules (copy-tree rules))
             (lang-rules (alist-get lang copy-of-rules))
             (lang-new-rules (alist-get lang new-rules)))
        (cond
         ((eq how :prepend)
          (setf (alist-get lang copy-of-rules)
                (append lang-new-rules lang-rules)))
         ((eq how :append)
          (setf (alist-get lang copy-of-rules)
                (append lang-rules lang-new-rules)))
         ((or (eq how :replace) t)
          (let ((tail-new-rules lang-new-rules)
                (tail-rules lang-rules)
                (new-rule nil)
                (rule nil))
            (while (setq new-rule (car tail-new-rules))
              (while (setq rule (car tail-rules))
                (when (equal (nth 0 new-rule) (nth 0 rule))
                  (setf (car tail-rules) new-rule))
                (setq tail-rules (cdr tail-rules)))
              (setq tail-new-rules (cdr tail-new-rules))))))
        copy-of-rules))))


(defconst svelte-ts-mode--query-get-script-element-attrs
  (when (treesit-ready-p 'svelte)
    (treesit-query-compile 'svelte '((start_tag (attribute) @attr)))))

(defun svelte-ts-mode--treesit-language-at-point (pos)
  (let* ((node (treesit-node-at pos 'svelte))
         (parent (treesit-node-parent node))
         (js-ready (svelte-ts-mode--treesit-buffer-ready 'javascript))
         (ts-ready (svelte-ts-mode--treesit-buffer-ready 'typescript))
         (css-ready (svelte-ts-mode--treesit-buffer-ready 'css)))
    (cond
     ((and node parent
           (equal (treesit-node-type node) "raw_text")
           (equal (treesit-node-type parent) "script_element"))
      (if (and js-ready ts-ready)
          (if (null (treesit-query-capture
                     parent svelte-ts-mode--query-get-script-element-attrs))
              'javascript
            'typescript)
        (if ts-ready
            'typescrpt
          (if js-ready
              'javscript
            'svelte))))
     ((and node parent
           (equal (treesit-node-type node) "raw_text")
           (equal (treesit-node-type parent) "style_element"))
      (if css-ready
          'css
        'svelte))
     (t 'svelte))))

;; reference: mhtml-ts-mode--js-css-tag-bol
(defun svelte-ts-mode--script-style-tag-bol (_node _parent &rest _)
  "Find the first non-space characters of html tags <script> or <style>.
Return `line-beginning-position' when `treesit-node-at' is svelte.
NODE and PARENT are ignored."
  (if (or (eq (treesit-language-at (point)) 'svelte))
      (line-beginning-position)
    (save-excursion
      (re-search-backward "<script.*>\\|<style.*>" nil t))))

(defvar-local svelte-ts-mode--comment-current-lang nil)

(defun svelte-ts-mode--comment-setup ()
  (let ((lang (treesit-language-at (point))))
    (unless (eq svelte-ts-mode--comment-current-lang lang)
      (setq svelte-ts-mode--comment-current-lang lang)
      (pcase lang
        ('html
         (setq-local comment-start "<!-- ")
         (setq-local comment-start-skip nil)
         (setq-local comment-end " -->")
         (setq-local comment-end-skip nil))
        ('css
         (setq-local comment-start "/*")
         (setq-local comment-start-skip "/\\*+[ \t]*")
         (setq-local comment-end "*/")
         (setq-local comment-end-skip "[ \t]*\\*+/"))
        ((or 'typescript 'javascript)
         (c-ts-common-comment-setup))))))

(defun svelte-ts-mode--adivce--comment-normalize-vars (fun &rest args)
  "Advice for `comment-normalize-vars'.
FUN: `comment-normalize-vars'.
ARGS: rest args for `comment-normalize-vars'."
  (if (not (equal major-mode 'svelte-ts-mode))
      (apply fun args)

    ;; unset used variables to not affects other languages in svelte-ts-mode buffer
    (setq-local
     comment-start nil
     comment-end nil
     comment-start-skip nil
     comment-end-skip nil
     adaptive-fill-mode nil
     adaptive-fill-first-line-regexp "\\`[ \t]*\\'"
     paragraph-start "\f\\|[ \t]*$"
     paragraph-separate "[ \t\f]*$"
     fill-paragraph-function nil
     comment-line-break-function #'comment-indent-new-line
     comment-multi-line nil

     comment-indent-function #'comment-indent-default
     comment-line-break-function #'comment-indent-new-line)

    (pcase (treesit-language-at (point))
      ((or 'typescript 'javascript)
       (c-ts-common-comment-setup))
      ('css
       (setq-local comment-start "/*")
       (setq-local comment-start-skip "/\\*+[ \t]*")
       (setq-local comment-end "*/")
       (setq-local comment-end-skip "[ \t]*\\*+/"))
      ('svelte
       (setq-local comment-start "<!-- ")
       (setq-local comment-end " -->")
       (setq-local comment-indent-function #'sgml-comment-indent)
       (setq-local comment-line-break-function #'sgml-comment-indent-new-line))
      (_ (error "Invalid language symbol!")))

    (apply fun args)))

(cl-defmacro  svelte-ts-mode--navigation-wrapper (fn-name &key pred backward fn-description)
  "Create a navigation function named FN-NAME with PRED.  Searches forward, unless
BACKWARD is non-nil. FN-DESCRIPTION is interactive description for generated function."
  (let* ((fn-description (format "%s" (or fn-description (concat "Jump to " fn-name)))))
    `(cons ,fn-description
           . ((defun ,(intern (format "svelte-ts-mode-%s" fn-name)) ()
                ,fn-description
                (interactive)
                (let ((node (svelte-ts-mode--find-tag-node (treesit-node-at (point)))))
                  (unless
                      (when node
                        (let* ((tag (treesit-node-text node))
                               (next-node
                                (treesit-search-forward
                                 (treesit-node-parent node)
                                 ,pred
                                 ,backward
                                 nil)))
                          (when next-node
                            (push-mark nil t)
                            (goto-char (treesit-node-start next-node)))))
                    (message "%s" (if node "No more tags" "Not in a tag")))))))))

(svelte-ts-mode--navigation-wrapper
 "move-next-tag"
 :fn-description "Go to the next svelte/html tag of any type"
 :pred (lambda (n)
         (and (equal (treesit-node-type n) "tag_name")
              (equal (treesit-node-type (treesit-node-parent n)) "start_tag"))))

(svelte-ts-mode--navigation-wrapper
 "move-prev-tag"
 :fn-description "Go to the previous svelte/html tag of any type"
 :backward t
 :pred (lambda (n)
         (and (equal (treesit-node-type n) "tag_name")
              (equal (treesit-node-type (treesit-node-parent n)) "start_tag"))))

(svelte-ts-mode--navigation-wrapper
 "move-next-same-tag"
 :fn-description "Go to the next svelte/html tag of same high-lighted type"
 :pred (lambda (n)
         (and (equal (treesit-node-type n) "tag_name")
              (equal (treesit-node-text n) tag)
              (equal (treesit-node-type (treesit-node-parent n)) "start_tag"))))

(svelte-ts-mode--navigation-wrapper
 "move-prev-same-tag"
 :fn-description "Go to the next svelte/html tag of same high-lighted type"
 :backward t
 :pred (lambda (n)
         (and (equal (treesit-node-type n) "tag_name")
              (equal (treesit-node-text n) tag)
              (equal (treesit-node-type (treesit-node-parent n)) "start_tag"))))

(defun svelte-ts-mode-skip-tag-forward ()
  "Skip forward over entire svelte/html tag element and children to end of tag element"
  (interactive)
  (let ((node (svelte-ts-mode--find-element-node (treesit-node-at (point)))))
    (unless
        (when node
          (push-mark nil t)
          (goto-char (treesit-node-end node))
          (skip-chars-forward "[:space:]"))  ; tree-grammars svelte has gaps in elements
      (message "%s" (if node "No more tags" "Not in a tag")))))

(defun svelte-ts-mode-skip-tag-backward ()
  "Skip backward over entire svelte/html tag element and children to start of tag"
  (interactive)
  (skip-chars-backward "[:space:]") ; tree-grammars svelte has gaps in elements, skip
  (let* ((start (point))
         (node (svelte-ts-mode--find-element-node (treesit-node-at start) start)))
    (unless
        (when node
          (push-mark nil t)
          (goto-char (treesit-node-start node)))
      (message "%s" (if node "No more tags" "Not in a tag")))))

(defun svelte-ts-mode--find-tag-node (node)
  "Return the tree-sitter html/svelte tag node for the NODE, or nil"
  (let ((child (treesit-node-child
                (svelte-ts-mode--find-element-node node) 0 "start_tag")))
    (if child
        (treesit-node-child child 0 "tag_name")
      child)))

(defun svelte-ts-mode--find-element-node (node &optional start-point)
  "Return the tree-sitter html/svelte element node for the NODE, or nil. Optional
START-POINT instead of using default cursor's current `(point)`, to use if caller is
removing padding, or other reason not to look at current cursor to walk up to parent."
  ;; If we are in whitespace before a node, back up until we are "inside" something.
  (while (and node (> (treesit-node-start node) (or start-point (point))))
    (setq node (treesit-node-parent node)))
  ;; if node is `document' type, then we're not in a node we want to use
  (when (not (equal (treesit-node-type node) "document"))
    ;; Try to find a strictly enclosing parent element
    (let ((return-node (treesit-parent-until
                        node
                        (lambda (n)
                          (member (treesit-node-type n)
                                  '("element")))
                        t)))
      (if return-node
          return-node
        (treesit-node-child node -1 "element")))))

(defun svelte-ts-mode-finish-incomplete-start-tag ()
  "Check if inside an incomplete start_tag or ERROR node and insert ending '>'."
  (interactive)
  ;; Trim leading space, to ensure get broken tag if there is one based on start
  (let* ((start (save-excursion
                  (skip-chars-backward "[:space:]")
                  (point)))
         (node (treesit-node-at start))
         ;; Look back for 'start_tag' or 'ERROR' happens when syntax broken
         (parent-node (treesit-parent-until
                       node
                       (lambda (n)
                         (member (treesit-node-type n) '("start_tag" "ERROR")))
                       t)))
    (when parent-node
      (cond
       ;; Is it a standard start_tag missing a '>',
       ((and (equal (treesit-node-type parent-node) "start_tag")
             (not (string-suffix-p ">" (treesit-node-text parent-node))))
        (push-mark)
        (goto-char (treesit-node-end parent-node))
        (skip-chars-backward "[:space:]")
        (insert ">")
        ;; if error in tree, like not having a closed tag will result on cursor
        ;; being on next tag, back up a bit.
        (when (member (treesit-node-type (treesit-node-at (point))) '("<" "start_tag"))
          (backward-char)))
       ;; Or more likely deemed an ERROR node that looks like an open tag
       ((equal (treesit-node-type parent-node) "ERROR")
        (let ((children (treesit-node-children parent-node)))
          (when (and (seq-find (lambda (n)
                                 (equal (treesit-node-type n) "<"))
                               children)
                     (seq-find (lambda (n)
                                 (member (treesit-node-type n) '("tag_name" "name")))
                               children)
                     (not (seq-find (lambda (n)
                                      (equal (treesit-node-type n) ">"))
                                    children)))
            (insert ">"))))))))

(defun svelte-ts-mode-mark-element ()
  "Mark current html/svelte tag element including all children. Expands region with
successive calls."
  (interactive)
  (let* ((expanded-start (max 1 (1- (region-beginning))))
         (node
          (if (not (region-active-p))
              (svelte-ts-mode--find-element-node (treesit-node-at (point)))
            (svelte-ts-mode--find-element-node
             (treesit-node-at expanded-start)
             expanded-start)))
         (start (treesit-node-start node))
         (end (treesit-node-end node)))
    (if (or (not node)
            (not (<= start (point) end))) ;; extra check
        (message "%s" (if (region-active-p) "Can't expand more" "Not in a tag"))
      (push-mark (treesit-node-start node) t t)
      (goto-char (treesit-node-end node)))))

(defun svelte-ts-mode-delete-element ()
  "Delete current html/svelte tag element including all children."
  (interactive)
  ;; Should complete open tag first
  (save-excursion
    (svelte-ts-mode-finish-incomplete-start-tag))
  (let* ((node (svelte-ts-mode--find-element-node (treesit-node-at (point))))
         (start (treesit-node-start node))
         (end (treesit-node-end node)))
    (if (or (not node)
            (not (<= start (point) end))) ;; extra check
        (message "Not in a tag")
      (goto-char start)
      (skip-chars-backward "[:space:]")
      (setq start (point))
      (goto-char end)
      (skip-chars-forward "[:space:]")
      (delete-region  start (point))
      (newline)
      (indent-according-to-mode))))

(defun svelte-ts-mode-close-tag ()
  "Close current or previous open svelte/html element."
  (interactive)
  (svelte-ts-mode-finish-incomplete-start-tag)
  (let* ((node (treesit-node-at (point)))
         (start-tag-node (svelte-ts-mode--find-tag-node node))
         (start-tag-element-end (treesit-node-end (treesit-node-parent start-tag-node)))
         (start-tag-name (treesit-node-text start-tag-node)))
    (unless
        (when (and node start-tag-node
                   ;; HTML5 these tags are "self-closing" and should not have close tags
                   (not (member start-tag-name
                                '("area" "base" "br" "col" "embed" "hr"
                                  "img" "input" "link" "meta" "param"
                                  "source" "track" "wbr"))))
          ;; Get out of the middle of current tag before closing
          (when (> start-tag-element-end (point))
            (push-mark)
            (goto-char start-tag-element-end))
          (insert "</" start-tag-name ">")))))

(defun svelte-ts-mode-clean-advice ()
  (interactive)
  (advice-remove #'comment-normalize-vars
                 #'svelte-ts-mode--adivce--comment-normalize-vars))

;;;###autoload
(define-derived-mode svelte-ts-mode prog-mode "Svelte"
  "A mode for editing Svelte templates."
  (unless (svelte-ts-mode--treesit-buffer-ready 'svelte)
    (error "Tree-sitter for svelte isn't available"))

  (setq treesit-primary-parser (treesit-parser-create 'svelte))

  ;; Comment.
  (if (>= emacs-major-version 31)
      (progn
        (setq-local comment-multi-line t)
        (setq-local comment-setup-function #'svelte-ts-mode--comment-setup))

    (when svelte-ts-mode-enable-comment-advice
      ;; advice-add won't add it twice
      (advice-add #'comment-normalize-vars :around
                  #'svelte-ts-mode--adivce--comment-normalize-vars)))

  ;; Font-lock.
  (setq-local treesit-font-lock-settings (svelte-ts-mode--font-lock-settings))
  (setq-local treesit-font-lock-feature-list svelte-ts-mode-font-lock-features-list)

  ;; ;; Indent.
  (setq-local treesit-simple-indent-rules svelte-ts-mode--indent-rules)


  (when (svelte-ts-mode--treesit-buffer-ready 'javascript)
    (require 'js)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        (svelte-ts-mode--prefix-font-lock-settings-features-name
                         "javascript" (if (>= emacs-major-version 31)
                                          (js--treesit-font-lock-settings)
                                        js--treesit-font-lock-settings))))
    (setq-local treesit-simple-indent-rules
                (append treesit-simple-indent-rules
                        (svlete-ts-mode--simple-indent-modify-rules
                         'javascript
                         '((javascript ((parent-is "program")
                                        svelte-ts-mode--script-style-tag-bol
                                        svelte-ts-mode-indent-offset)))
                         (if (>= emacs-major-version 31)
                             (js--treesit-indent-rules)
                           js--treesit-indent-rules)
                         :replace)))
    (setq-local treesit-font-lock-feature-list
                (svelte-ts-mode--merge-font-lock-features
                 treesit-font-lock-feature-list
                 ;; Emacs 29 doesn't have `js--treesit-font-lock-feature-list'
                 (svelte-ts-mode--prefix-font-lock-features
                  "javascript"
                  '((comment document definition) (keyword string)
                    (assignment constant escape-sequence jsx number pattern string-interpolation)
                    (bracket delimiter function operator property)))))
    (treesit-parser-create 'javascript))

  (when (svelte-ts-mode--treesit-buffer-ready 'typescript)
    (require 'typescript-ts-mode)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        (svelte-ts-mode--prefix-font-lock-settings-features-name
                         "typescript"
                         (typescript-ts-mode--font-lock-settings 'typescript))))
    (setq-local treesit-simple-indent-rules
                (append treesit-simple-indent-rules
                        (svlete-ts-mode--simple-indent-modify-rules
                         'typescript
                         '((typescript ((parent-is "program")
                                        svelte-ts-mode--script-style-tag-bol
                                        svelte-ts-mode-indent-offset)))
                         (typescript-ts-mode--indent-rules 'typescript)
                         :replace)))
    (setq-local treesit-font-lock-feature-list
                (svelte-ts-mode--merge-font-lock-features
                 treesit-font-lock-feature-list
                 (svelte-ts-mode--prefix-font-lock-features
                  "typescript"
                  '((comment declaration)
                    (keyword string escape-sequence)
                    (constant expression identifier number pattern property)
                    (operator function bracket delimiter)))))
    (treesit-parser-create 'typescript))

  (when (svelte-ts-mode--treesit-buffer-ready 'css)
    (require 'css-mode)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        (svelte-ts-mode--prefix-font-lock-settings-features-name
                         "css" css--treesit-settings)))
    (setq-local treesit-simple-indent-rules
                (append treesit-simple-indent-rules
                        (svlete-ts-mode--simple-indent-modify-rules
                         'css
                         '((css ((parent-is "stylesheet")
                                 svelte-ts-mode--script-style-tag-bol
                                 svelte-ts-mode-indent-offset)))
                         css--treesit-indent-rules
                         :prepend)))
    (setq-local treesit-font-lock-feature-list
                (svelte-ts-mode--merge-font-lock-features
                 ;; Emacs 29 doesn't have `css--treesit-font-lock-feature-list'
                 treesit-font-lock-feature-list
                 (svelte-ts-mode--prefix-font-lock-features
                  "css"
                  '((selector comment query keyword) (property constant string)
                    (error variable function operator bracket)))))
    (treesit-parser-create 'css))


  (let ((js-ready (svelte-ts-mode--treesit-buffer-ready 'javascript))
        (ts-ready (svelte-ts-mode--treesit-buffer-ready 'typescript))
        (css-ready (svelte-ts-mode--treesit-buffer-ready 'css)))
    (setq-local treesit-range-settings
                (nconc
                 treesit-range-settings
                 (if (and js-ready ts-ready)
                     (treesit-range-rules
                      :embed 'typescript
                      :host 'svelte
                      '(
                        ;; Scripts with attributes (e.g. lang="ts")
                        (script_element (start_tag (attribute)) (raw_text) @capture)
                        ;; template expressions { ... }
                        ((svelte_raw_text) @capture)
                        )

                      :embed 'javascript
                      :host 'svelte
                      '(
                        ;; Scripts without attributes (assumed vanilla JS)
                        (script_element (start_tag (tag_name) :anchor ">")  (raw_text) @capture)
                        ))

                   (if ts-ready
                       ;; only typescript is installed.
                       (treesit-range-rules
                        :embed 'typescript
                        :host 'svelte
                        '(
                          (script_element (raw_text) @capture)
                          ((svelte_raw_text) @capture)
                          ))

                     (when js-ready
                       ;; fallback when only javascript is installed
                       (treesit-range-rules
                        :embed 'javascript
                        :host 'svelte
                        '(
                          (script_element (raw_text) @capture)
                          ((svelte_raw_text) @capture)
                          )))))

                 (when css-ready
                   (treesit-range-rules
                    :embed 'css
                    :host 'svelte
                    '((style_element (raw_text) @capture)))))))

  (setq-local treesit-language-at-point-function #'svelte-ts-mode--treesit-language-at-point)
  (setq-local prettify-symbols-alist svelte-ts-mode-prettify-symbols-alist)

  (treesit-major-mode-setup))

(if (fboundp 'derived-mode-add-parents)
    (derived-mode-add-parents 'svelte-ts-mode '(html-ts-mode html-mode svelte-mode)))

(when (treesit-ready-p 'svelte)
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-ts-mode)))

(provide 'svelte-ts-mode)

;;; svelte-ts-mode.el ends here
