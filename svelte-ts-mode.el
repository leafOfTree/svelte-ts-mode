;;; svelte-ts-mode.el --- Emacs major mode for Svelte -*- lexical-binding:t -*-
;; Copyright (C) 2024 Leaf.

;; Author: Leaf <leafvocation@gmail.com>
;; Created: 5 Dec 2024
;; Keywords: languages
;; Homepage: https://github.com/leafOfTree/svelte-ts-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "29"))

;; This file is NOT part of GNU Emacs.
;; You can redistribute it and/or modify it under the terms of
;; the GNU Lesser General Public License v3.0.

;;; Commentary:

;; This major mode includes typescript-ts-mode and css-mode
;; to support basic treesit svelte syntax and indent

;;; Code:

(require 'treesit)
(require 'typescript-ts-mode)
(require 'css-mode)

(defgroup svelte ()
  "Major mode for editing Svelte templates."
  :group 'languages)

(defcustom svelte-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `svelte-ts-mode'."
  :type 'integer
  :group 'svelte
  :package-version '(svelte-ts-mode . "1.0.0"))

(defvar svelte-ts-mode--indent-rules
  `((svelte
     ((parent-is "document") column-0 0)
     ((node-is "/>") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((node-is "end_tag") parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)
     ((parent-is "element") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "script_element") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "style_element") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "start_tag") parent-bol svelte-ts-mode-indent-offset)
     ((parent-is "self_closing_tag") parent-bol svelte-ts-mode-indent-offset))
    (css . ,(append (alist-get 'css css--treesit-indent-rules)
                    '(((parent-is "style_element") parent-bol 0))))
    (typescript . ,(alist-get 'typescript (typescript-ts-mode--indent-rules 'typescript))))
  "Tree-sitter indentation rules for `svelte-ts-mode'.")

(defun svelte-ts-mode--prefix-font-lock-features (prefix settings)
  "Prefix with PREFIX the font lock features in SETTINGS."
  (mapcar (lambda (setting)
            (list (nth 0 setting)
                  (nth 1 setting)
                  (intern (format "%s-%s" prefix (nth 2 setting)))
                  (nth 3 setting)))
          settings))

(defvar svelte-ts-mode--font-lock-settings
  (append
   (svelte-ts-mode--prefix-font-lock-features
    "typescript"
    (typescript-ts-mode--font-lock-settings 'typescript))
   (svelte-ts-mode--prefix-font-lock-features
     "css"
     css--treesit-settings)

   (treesit-font-lock-rules

      :language 'svelte
      :feature 'svelte-tag
      '((tag_name) @font-lock-function-name-face)

      :language 'svelte
      :feature 'svelte-keyword
      '((special_block_keyword) @font-lock-keyword-face)

      ;; :language 'svelte
      ;; :feature 'svelte-keyword
      ;; '("#if" @font-lock-keyword-face)
;; 
      :language 'svelte
      :feature 'svelte-string
      '((quoted_attribute_value) @font-lock-string-face)

      :language 'svelte
      :feature 'svelte-attribute
      '((attribute
          (attribute_name) @font-lock-keyword-face
          (:match "\\(on\\|bind\\):" @font-lock-keyword-face)))

      ;; :language 'svelte
      ;; :feature 'svelte-attribute
      ;; '((attribute_name) @svelte-highlight-attribute-prefix)
;; 
      ;; :language 'svelte
      ;; :feature 'svelte-bracket
      ;; '((["<" ">" "</" "/>" "{" "}"]) @font-lock-bracket-face)
   )
   )
  "Tree-sitter font-lock settings for `svelte-ts-mode'.")

(defun svelte-highlight-attribute-prefix (node override start end &rest args)
(let* ((name (treesit-node-text node))
       (case-fold-search nil))
  (message "Processing node: '%s' at %d-%d" name start end)
  (when (or (string-prefix-p "on:" name)
            (string-prefix-p "bind:" name))
    (let ((prefix-end (if (string-match "^\\(on\\|bind\\):" name)
                         (match-end 1)
                       0)))
      (message "Applying face at %d-%d for prefix: '%s'"
              start (+ start prefix-end)
              (substring name 0 prefix-end))
      (with-silent-modifications
        ;; (remove-text-properties start end '(face nil))
        (put-text-property start (+ start prefix-end)
                          'face 'font-lock-keyword-face))
      t))))

(defvar svelte-ts-mode--range-settings
  (treesit-range-rules
    ;; CSS in style tags
    :embed 'css
    :host 'svelte
    '((style_element 
        (raw_text) @cap))

    ;; TypeScript embedding
    :embed 'typescript
    :host 'svelte
    '(
      ;; In script tags
      (script_element 
        (raw_text) @cap)

      ;; Any expression
      (expression) @cap

      )))
      
(defun svelte-ts-mode--treesit-language-at-point (point)
  "Determine the language at POINT in a Svelte file."
  (let ((node (treesit-node-at point 'svelte)))
    (pcase (treesit-node-type (treesit-node-parent node))
      ("script_element" 'typescript)
      ("style_element" 'css)
      (_ 'svelte))))

;;;###autoload
(define-derived-mode svelte-ts-mode prog-mode "Svelte"
  "Major mode for editing Svelte templates, powered by tree-sitter."
  :group 'svelte

  (unless (treesit-ready-p 'svelte)
    (error "Tree-sitter grammar for Svelte isn't available"))

  (unless (treesit-ready-p 'css)
    (error "Tree-sitter grammar for CSS isn't available"))

  (unless (treesit-ready-p 'typescript)
    (error "Tree-sitter grammar for Typescript/TSX isn't available"))

  (treesit-parser-create 'svelte)
  (treesit-parser-create 'css)
  (treesit-parser-create 'typescript)

  ;; Indentation rules
  (setq-local
   treesit-simple-indent-rules svelte-ts-mode--indent-rules
   css-indent-offset svelte-ts-mode-indent-offset)

  ;; Font locking
  (setq-local
   treesit-font-lock-settings svelte-ts-mode--font-lock-settings
   treesit-font-lock-feature-list
   '((;; Svelte features
      svelte-attribute
      svelte-bracket
      svelte-comment
      svelte-keyword
      svelte-string
      svelte-tag
      
      ;; CSS features
      css-bracket
      css-comment
      css-constant
      css-error
      css-function
      css-keyword
      css-operator
      css-property
      css-query
      css-selector
      css-string
      css-variable
      
      ;; TypeScript features
      typescript-bracket
      typescript-comment
      typescript-constant
      typescript-declaration
      typescript-delimiter
      typescript-escape-sequence
      typescript-expression
      typescript-function
      typescript-identifier
      typescript-keyword
      typescript-number
      typescript-pattern
      typescript-property
      typescript-string)))

  ;; Embedded languages
  (setq-local
   treesit-range-settings svelte-ts-mode--range-settings
   treesit-language-at-point-function #'svelte-ts-mode--treesit-language-at-point)

  (treesit-major-mode-setup))

(if (treesit-ready-p 'svelte)
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-ts-mode)))

(provide 'svelte-ts-mode)
;;; svelte-ts-mode.el ends here
