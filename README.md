# Svelte tree-sitter mode

An Emacs tree-sitter major mode for `.svelte` files.

It requires

- Emacs `(>= emacs-major-version 29)` with tree-sitter support.

- Svelte language grammer `M-x treesit-install-language-grammar`

  You can use https://github.com/Himujjal/tree-sitter-svelte

## Features

It combines the following major modes:

- html-mode
- css-mode
- typescript-ts-mode

## Installation

> [!CAUTION]
> Still in early development.

- [MELPA][2]

  Not ready yet.

- Manually

  ```bash
  git clone https://github.com/leafOfTree/svelte-ts-mode --depth 1
  ```

  ```lisp
  ; ~/.emacs
  (add-to-list 'load-path "/path/to/svelte-ts-mode")
  (require 'svelte-ts-mode)
  ```

  For [Spacemacs][1], put them inside `dotspacemacs/user-config`.

  ```lisp
  ; ~/.spacemacs
  (defun dotspacemacs/user-config ()
        
      (add-to-list 'load-path "/path/to/svelte-ts-mode")
      (require 'svelte-ts-mode)
  ```

## Credits

Inspired by https://github.com/mickeynp/html-ts-mode

[1]: https://github.com/syl20bnr/spacemacs
[2]: https://melpa.org/#/svelte-mode

