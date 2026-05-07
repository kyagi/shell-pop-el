# shell-pop - Easily toggle a shell window in Emacs with a single keystroke
[![MELPA](https://melpa.org/packages/shell-pop-badge.svg)](https://melpa.org/#/shell-pop)
[![MELPA Stable](https://stable.melpa.org/packages/shell-pop-badge.svg)](https://stable.melpa.org/#/shell-pop)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

The **shell-pop** Emacs package provides on-demand access to a terminal through a single, configurable key binding.

The package supports multiple terminal implementations, including `term`, `eshell` and `ansi-term`, and ensures your original window configuration is restored when the terminal is hidden.

## Installation

### Installation with use-package

To install **shell-pop** from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code to your Emacs init file to install **shell-pop** from MELPA:

```emacs-lisp
(use-package shell-pop
  :commands shell-pop)
```

### Manual installation

You can install `shell-pop.el` from [MELPA](https://melpa.org/) using `package.el`.
```elisp
M-x package-install RET shell-pop RET
```

Make sure to place shell-pop.el somewhere in the load-path and add the following lines to your init file (e.g., `~/.emacs.d/init.el`):

```lisp
(add-to-list 'load-path "/path/to/shell-pop")
(require 'shell-pop)
```

## Customization

### Configuration Options

#### `shell-pop-window-position` (Default: `"bottom"`)

The position of the popup window. You can choose `"bottom"`, `"top"`, `"right"`, `"left"`, or `"full"`. Use the entire frame when this value is set to `"full"`.

#### `shell-pop-window-size` (Default: `30`)

The percentage of the frame used for the shell buffer window size.

#### `shell-pop-full-span`

This option allows you to generate the shell window with the same width as the current Emacs frame. It is beneficial when you use multiple side-by-side windows in Emacs. For more details, see [Issue #21](https://github.com/kyagi/shell-pop-el/pull/21#issuecomment-48876673).

### Hooks

- `shell-pop-in-hook`: Runs before the shell buffer pops up.
- `shell-pop-in-after-hook`: Runs after the shell buffer pops up.
- `shell-pop-out-hook`: Runs before the shell buffer pops out.
- `shell-pop-process-exit-hook`: Runs when the shell's process exits.

### M-x Customize

Use `M-x customize-variable RET shell-pop-shell-type RET` to customize the shell to use. Four pre-set options are: `shell`, `terminal`, `ansi-term`, and `eshell`. You can also set your custom shell if you use other configuration.

For the `terminal` and `ansi-term` options, you can set the underlying shell by customizing `shell-pop-term-shell`. By default, `shell-file-name` is used, but you can also specify paths like `/bin/tcsh`, `/bin/bash`, or `/bin/zsh`.

Use `M-x customize-group RET shell-pop RET` to set further options such as hotkeys, window height, and position.

After saving the customized values, your init file will look similar to the following.
```lisp
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(shell-pop-default-directory "/Users/kyagi/git")
 '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-size 30)
 '(shell-pop-full-span t)
 '(shell-pop-window-position "bottom")
 '(shell-pop-autocd-to-working-dir t)
 '(shell-pop-restore-window-configuration t)
 '(shell-pop-cleanup-buffer-at-process-exit t))
 ```

## Usage

Hit the hotkey you customized for `shell-pop-universal-key`. For example, press `C-t` based on the configuration above.

You can open multiple shell buffers by running `shell-pop` with universal arguments. For example, press `C-u 2 C-t`. You can increase the number (`2`, `3`, `4`, etc.) as much as you want to open different shell buffers.

## Screenshot

![screenshot](https://raw.github.com/kyagi/shell-pop-el/master/screenshot.png)

## License

The **shell-pop** Emacs package is maintained by [Kazuo YAGI](http://github.com/kyagi) (original author) and [James Cherti](https://www.jamescherti.com/).

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

## Links

- [shell-pop @GitHub](https://github.com/kyagi/shell-pop-el)
- [shell-pop @MELPA](http://melpa.org/#/shell-pop)
- [shell-pop @MELPA-Stable](http://stable.melpa.org/#/shell-pop)
