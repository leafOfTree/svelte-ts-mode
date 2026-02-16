# Svelte tree-sitter mode

An Emacs tree-sitter major mode for `.svelte` files.

It requires

- Emacs `(>= emacs-major-version 30)` with tree-sitter support.

Upstream grammars:

- [tree-sitter-svelte][3]: mandatory (NOTE: Please change to tree-sitter-grammars library, if not done already.)
- [tree-sitter-typescript][4]: optional
- [tree-sitter-javascript][5]: optional
- [tree-sitter-css][6]: optional

Example `language-source-alist` for `treesit-language-source-alist` can be found
in `svelte-ts-mode-language-source-alist` constant in [svelte-ts-mode.el](./svelte-ts-mode.el)

You can use command `M-x treesit-install-language-grammar` to install these grammars
once you have configured them in your `treesit-language-source-alist`.

## Installation

NOTE, for Emacs master user, please use the `emacs-master` branch (i.e. you should add
option `:branch "emacs-master"` in your elpaca configuration or clone the `emacs-master`
branch locally).

> [!CAUTION]
> Still in early development.

- [MELPA][2]

  Not ready yet.

- Elpaca

  ```emacs-lisp
  (use-package svelte-ts-mode
    :ensure (:host github :repo "leafOfTree/svelte-ts-mode"))
  ```

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

## Example Configuration

```emacs-lisp
(use-package svelte-ts-mode
  :after eglot
  :ensure (:host github :repo "leafOfTree/svelte-ts-mode")
  :config
  (add-to-list 'eglot-server-programs '(svelte-ts-mode . ("svelteserver" "--stdio"))))
```

## Tips

### Set project-wide indentation offset for different languages

Create `.dir-locals.el` at the project root directory, and write content like the
following into it:

```emacs-lisp
;;; Directory Local Variables         -*- no-byte-compile: t; -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((typescript-ts-mode-indent-offset . 4)
         (js-indent-level . 4)
         (css-indent-offset . 4)
         (svelte-ts-mode-indent-offset . 2))))
```

This ensures indentation for a specific language(e.g. typescript) between its major mode
and `svelte-ts-mode` are the same.

## Key commands

Several keys are set similar to legacy sgml/mhtml keys to help with navigation and editing.  If you do not want these keys mapped automatically then customize `svelte-ts-mode-dont-bind-keys` to non-nil value.

|Keymap | Command |
|---|---|
|C-c /       | svelte-ts-mode-close-tag          |
|C-c C-s     | svelte-ts-mode-mark-element       |
|C-c C-SPC   | svelte-ts-mode-mark-element       |
|C-c C-d     | svelte-ts-mode-delete-element     |
|C-c C-n     | svelte-ts-mode-move-next-tag      |
|C-c C-p     | svelte-ts-mode-move-prev-tag      |
|C-c C-f     | svelte-ts-mode-move-next-same-tag |
|C-c C-b     | svelte-ts-mode-move-prev-same-tag |
|C-c \<left\>  | svelte-ts-mode-skip-tag-backward  |
|C-c C-a     | svelte-ts-mode-skip-tag-backward  |
|C-c \<right\> | svelte-ts-mode-skip-tag-forward   |
|C-c C-e     | svelte-ts-mode-skip-tag-forward   |

## Credits

Inspired by `mhtml-ts-mode`

[1]: https://github.com/syl20bnr/spacemacs
[2]: https://melpa.org/#/svelte-mode
[3]: https://github.com/tree-sitter-grammars/tree-sitter-svelte
[4]: https://github.com/tree-sitter/tree-sitter-typescript
[5]: https://github.com/tree-sitter/tree-sitter-javascript
[6]: https://github.com/tree-sitter/tree-sitter-css
