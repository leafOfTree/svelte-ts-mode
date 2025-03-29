;;; svelte-ts-mode.el --- Emacs major mode for Svelte -*- lexical-binding:t -*-
;; Copyright (C) 2025 Leaf.

;; Author: Leaf <leafvocation@gmail.com>
;; Author: Meow King <mr.meowking@posteo.com>
;; Created: 5 Dec 2024
;; Keywords: svelte languages tree-sitter
;; Homepage: https://github.com/leafOfTree/svelte-ts-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.
;; You can redistribute it and/or modify it under the terms of
;; the GNU Lesser General Public License v3.0.

;;; Commentary:

;; This major mode includes typescript-ts-mode and css-mode
;; to support basic treesit svelte syntax and indent


;; Note this mode advises `comment-normalize-vars' to create special behavior
;; for `svelte-ts-mode' only.  You can set `svelte-ts-mode-enable-comment-advice'
;; to nil to avoid advice.

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

(defcustom svelte-ts-mode-enable-comment-advice t
  "Enable language-wise comment inside `svlete-ts-mode' buffer."
  :type 'boolean
  :safe 'booleanp
  :group 'svelte-ts
  :package-version '(svelte-ts-mode . "1.0.0"))

(defconst svelte-ts-mode-language-source-alist
  '((svelte . ("https://github.com/Himujjal/tree-sitter-svelte"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil
                   "typescript/src"))
    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
    (css . ("https://github.com/tree-sitter/tree-sitter-css")))
  "Language source for `svelte-ts-mode'.
User can easily add it to their `treesit-language-source-alist'.
In the future, we may pin a version.")

(defconst svelte-ts-mode--indent-close-types-regexp
  (concat
   "\\`"
   (regexp-opt
    '(">" "/>" "}"
      "end_tag"
      "else_if_statement" "else_statement" "if_end_expr"  ; if
      "key_end_expr"  ; key
      "else_statement" "each_end_expr"  ; each
      "then_statement" "catch_statement" "await_end_expr"  ; await
      "snippet_end_expr"  ; snippet
      ))
   "\\'"))

(defconst svelte-ts-mode--container-node-types-regexp
  (concat
   "\\`"
   (regexp-opt
    '(; html
      "element" "script_element" "style_element"

      ;; svelte
      ;; if
      "if_statement" "if_start_expr"
      "else_if_statement" "else_if_expr"
      "else_statement" "else_expr"
      "if_end_expr"

      ;; key
      "key_statement" "key_start_expr" "key_end_expr"

      ;; each
      "each_statement" "each_start_expr"
      "else_each_statement" "else_expr"
      "each_end_expr"
      
      ;; await
      "await_statement" "await_start_expr"
      "then_statement" "then_expr"
      "catch_statement" "catch_expr"
      "await_end_expr"

      ;; snippet
      "snippet_statement" "snippet_start_expr" "snippet_end_expr"

      ;; misc expression
      "expression" "html_expr" "const_expr" "render_expr" "debug_expr"))
   "\\'"))


(defvar svelte-ts-mode-font-lock-settings
  (treesit-font-lock-rules
   :language 'svelte
   :feature 'error
   '((erroneous_end_tag_name) @font-lock-warning-face)

   :language 'svelte
   :feature 'definition
   '((tag_name) @font-lock-function-name-face)

   
   :language 'svelte
   :feature 'comment
   '((comment) @font-lock-comment-face)
   
   :language 'svelte
   :feature 'keyword
   '([(special_block_keyword)
      (then)
      (as)]
     @font-lock-keyword-face)

   :language 'svelte
   :feature 'punctuation
   '(["{" "}"] @font-lock-bracket-face
     "=" @font-lock-operator-face
     ["<" ">" "</" "/>" "#" ":" "/" "@"] @font-lock-delimiter-face)
   
   :language 'svelte
   :feature 'property
   '((attribute_name) @font-lock-property-name-face)

   :language 'svelte
   :feature 'string
   '([(attribute_value)
      (quoted_attribute_value)]
     @font-lock-string-face)))

(defconst svelte-ts-mode-font-lock-features-list
  '((error definition comment keyword)
    (property string)
    (punctuation)
    ()))


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
            ("else_if_statement" "if_statement")
            ("if_end_expr" "if_statement")

            ;; key
            ("key_end_expr" "key_statement")
            
            ;; each
            ("each_end_expr" "each_statement")

            ;; await
            ("then_statement" "await_statement")
            ("catch_statement" "await_statement")
            ("await_end_expr" "await_statement")

            ;; snippet
            ("snippet_end_expr" "snippet_statement")

            ;; common
            ("else_statement" (regexp-opt '("if_statement" "each_statement")))))
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
            (list (nth 0 setting)
                  (nth 1 setting)
                  (intern (format "%s-%s" prefix (nth 2 setting)))
                  (nth 3 setting)))
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

(defconst svelte-ts-mode--query-get-script-element-attrs
  (when (treesit-ready-p 'svelte)
    (treesit-query-compile 'svelte '((start_tag (attribute) @attr)))))

(defun svelte-ts-mode--treesit-language-at-point (pos)
  (let* ((node (treesit-node-at pos 'svelte))
         (parent (treesit-node-parent node))
         (js-ready (treesit-ready-p 'javascript))
         (ts-ready (treesit-ready-p 'typescript))
         (css-ready (treesit-ready-p 'css)))
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

(defun svelte-ts-mode-clean-advice ()
  (interactive)
  (advice-remove #'comment-normalize-vars
                 #'svelte-ts-mode--adivce--comment-normalize-vars))


(define-derived-mode svelte-ts-mode prog-mode "Svelte"
  "A mode for editing Svelte templates."
  (unless (treesit-ready-p 'svelte)
    (error "Tree-sitter for svelte isn't available"))

  (setq treesit-primary-parser (treesit-parser-create 'svelte))
  
  ;; Font-lock.
  (setq-local treesit-font-lock-settings svelte-ts-mode-font-lock-settings)
  (setq-local treesit-font-lock-feature-list svelte-ts-mode-font-lock-features-list)

  ;; Indent.
  (setq-local treesit-simple-indent-rules svelte-ts-mode--indent-rules)


  (when (treesit-ready-p 'javascript)
    (require 'js)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        (svelte-ts-mode--prefix-font-lock-settings-features-name
                         "javascript" js--treesit-font-lock-settings)))
    (setq-local treesit-simple-indent-rules
                (append treesit-simple-indent-rules
                        (if (>= emacs-major-version 31)
                            (treesit-simple-indent-modify-rules
                             'javascript
                             '((javascript ((parent-is "program")
                                            svelte-ts-mode--script-style-tag-bol
                                            svelte-ts-mode-indent-offset)))
                             js--treesit-indent-rules
                             :replace)
                          js--treesit-indent-rules)))
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

  (when (treesit-ready-p 'typescript)
    (require 'typescript-ts-mode)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        (svelte-ts-mode--prefix-font-lock-settings-features-name
                         "typescript"
                         (typescript-ts-mode--font-lock-settings 'typescript))))
    (setq-local treesit-simple-indent-rules
                (append treesit-simple-indent-rules
                        (if (>= emacs-major-version 31)
                            (treesit-simple-indent-modify-rules
                             'typescript
                             '((typescript ((parent-is "program")
                                            svelte-ts-mode--script-style-tag-bol
                                            svelte-ts-mode-indent-offset)))
                             (typescript-ts-mode--indent-rules 'typescript)
                             :replace)
                          (typescript-ts-mode--indent-rules 'typescript))))
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
  
  (when (treesit-ready-p 'css)
    (require 'css-mode)
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        (svelte-ts-mode--prefix-font-lock-settings-features-name
                         "css" css--treesit-settings)))
    (setq-local treesit-simple-indent-rules
                (append treesit-simple-indent-rules
                        (if (>= emacs-major-version 31)
                            (treesit-simple-indent-modify-rules
                             'css
                             '((css ((parent-is "stylesheet")
                                     svelte-ts-mode--script-style-tag-bol
                                     svelte-ts-mode-indent-offset)))
                             css--treesit-indent-rules
	                           :prepend)
                          css--treesit-indent-rules)))
    (setq-local treesit-font-lock-feature-list
                (svelte-ts-mode--merge-font-lock-features
                 ;; Emacs 29 doesn't have `css--treesit-font-lock-feature-list'
                 treesit-font-lock-feature-list
                 (svelte-ts-mode--prefix-font-lock-features
                  "css"
                  '((selector comment query keyword) (property constant string)
                    (error variable function operator bracket)))))
    (treesit-parser-create 'css))


  (when svelte-ts-mode-enable-comment-advice
    ;; advice-add won't add it twice
    (advice-add #'comment-normalize-vars :around
                #'svelte-ts-mode--adivce--comment-normalize-vars))

  (let ((js-ready (treesit-ready-p 'javascript))
        (ts-ready (treesit-ready-p 'typescript))
        (css-ready (treesit-ready-p 'css)))
    (setq-local treesit-range-settings
                (nconc
                 treesit-range-settings
                 (if (and js-ready ts-ready)
                     ;; NOTE in Emacs 31 :embed can be a function
                     (treesit-range-rules
                      :embed 'typescript
                      :host 'svelte
                      '((script_element (start_tag (attribute)) (raw_text) @capture))

                      :embed 'javascript
                      :host 'svelte
                      '((script_element (start_tag (tag_name) :anchor ">")  (raw_text) @capture)))
                   (if ts-ready
                       (treesit-range-rules
                        :embed 'typescript
                        :host 'svelte
                        '((script_element (raw_text) @capture)))
                     (when js-ready
                       (treesit-range-rules
                        :embed 'javascript
                        :host 'svelte
                        '((script_element (raw_text) @capture))))))
                 (when css-ready
                   (treesit-range-rules
                    :embed 'css
                    :host 'svelte
                    '((style_element (raw_text) @capture)))))))

  (setq-local treesit-language-at-point-function #'svelte-ts-mode--treesit-language-at-point)

  (treesit-major-mode-setup))

(when (treesit-ready-p 'svelte)
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-ts-mode)))

(provide 'svelte-ts-mode)

;;; svelte-ts-mode.el ends here
