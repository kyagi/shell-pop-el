# shell-pop - Easily toggle a shell window in Emacs with a single keystroke
[![MELPA](https://melpa.org/packages/shell-pop-badge.svg)](https://melpa.org/#/shell-pop)
[![MELPA Stable](https://stable.melpa.org/packages/shell-pop-badge.svg)](https://stable.melpa.org/#/shell-pop)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

The **shell-pop** Emacs package provides on-demand access to a terminal through a single, configurable key binding.

The package supports multiple terminal implementations, including `term`, `eshell`, `ansi-term`, `vterm`, and `eat`, and ensures your original window configuration is restored when the terminal is hidden.

## Installation

### Installation with use-package

To install **shell-pop** from MELPA:

1. If you haven't already done so, [add MELPA repository to your Emacs configuration](https://melpa.org/#/getting-started).

2. Add the following code to your Emacs init file to install **shell-pop** from MELPA:

```emacs-lisp
(use-package shell-pop
  ;; :bind automatically sets up the keybinding AND tells Emacs to lazy-load the
  ;; package the moment the key is pressed.
  :bind (("C-c t" . shell-pop))
  :custom
  ;; The key sequence used to toggle the shell window.
  (shell-pop-universal-key "C-c t")

  ;; Sets the screen position where the shell popup appears.
  ;; You can choose "bottom", "top", "right", "left", or "full".
  (shell-pop-window-position "bottom")

  ;; If non-nil, the window stretches across the entire frame width.
  (shell-pop-full-span nil)

  ;; The path to the shell executable used by the terminal emulator
  ;; (e.g., "/usr/bin/env bash").
  (shell-pop-term-shell shell-file-name)

  ;; The height or width of the window as a percentage of the frame.
  (shell-pop-window-size 30)

  ;; Defines the terminal implementation and its initialization logic.
  (shell-pop-shell-type '("ansi-term" "*ansi-term*"
                          (lambda ()
                            (ansi-term shell-pop-term-shell))))

  ;; Setting this to non-nil sends commands to the shell. This is not always
  ;; desirable, as it can send commands to any prompt.
  (shell-pop-autocd-to-working-dir nil))
```

### Manual installation

You can install `shell-pop.el` from [MELPA](https://melpa.org/) using `package.el`.
```elisp
M-x package-install RET shell-pop RET
```

Then add the following to your init file (e.g., `~/.emacs.d/init.el`):

```lisp
(require 'shell-pop)
```

## Customization

### Configuring the terminal

Here are the exact configurations for the most popular Emacs shells. Simply copy and paste your preferred option into your init file:

#### ansi-term
*Note: This will use the shell defined in your `shell-file-name` variable (e.g., bash, zsh).*
```elisp
(with-eval-after-load 'shell-pop
  (setopt shell-pop-shell-type '("ansi-term" "*ansi-term*"
                                 (lambda ()
                                   (ansi-term shell-pop-term-shell)))))
```

#### term
```elisp
(with-eval-after-load 'shell-pop
  (setopt shell-pop-shell-type '("terminal" "*terminal*"
                                 (lambda ()
                                   (term shell-pop-term-shell)))))
```

#### vterm

*Note: Requires the `vterm` package to be installed.*

```elisp
(with-eval-after-load 'shell-pop
  (setopt shell-pop-shell-type '("vterm" "*vterm*"
                                 (lambda ()
                                   (when (fboundp 'vterm)
                                     (let ((vterm-shell shell-pop-term-shell))
                                       (vterm)))))))
```

#### eat

*Note: Requires the `eat` package to be installed.*

```elisp
(with-eval-after-load 'shell-pop
  (setopt shell-pop-shell-type '("eat" "*eat*"
                                 (lambda ()
                                   (when (fboundp 'eat)
                                     (eat shell-pop-term-shell))))))
```

#### Shell
```elisp
(with-eval-after-load 'shell-pop
  (setopt shell-pop-shell-type '("shell" "*shell*"
                                 (lambda ()
                                   (shell)))))
```

#### Eshell
```elisp
(with-eval-after-load 'shell-pop
  (setopt shell-pop-shell-type '("eshell" "*eshell*"
                                 (lambda ()
                                   (eshell)))))
```


### Configuration Options

#### `shell-pop-window-position` (Default: `"bottom"`)

The position of the popup window. You can choose `"bottom"`, `"top"`, `"right"`, `"left"`, or `"full"`. Use the entire frame when this value is set to `"full"`.

#### `shell-pop-window-size` (Default: `30`)

The percentage of the frame used for the shell buffer window size.

#### `shell-pop-full-span`

This option allows you to generate the shell window with the same width as the current Emacs frame. It is beneficial when you use multiple side-by-side windows in Emacs. For more details, see [Issue #21](https://github.com/kyagi/shell-pop-el/pull/21#issuecomment-48876673).

#### `shell-pop-default-directory` (Default: `nil`)

If non-nil, changes the current directory to this specific path when first starting the shell.

#### `shell-pop-term-shell` (Default: `explicit-shell-file-name`, the `ESHELL` environment variable, or `shell-file-name`)

The path to the shell executable used by the `term` and `ansi-term` implementations (e.g., `"/bin/bash"` or `"/bin/zsh"`).

#### `shell-pop-autocd-to-working-dir` (Default: `t`)

If non-nil, automatically changes the directory of the shell buffer to match the working directory from which `shell-pop` was invoked.

#### `shell-pop-restore-window-configuration` (Default: `t`)

If non-nil, restores the window configuration when the `shell-pop` buffer is closed. The shell window is deleted in any case. This variable has no effect when the `shell-pop-window-position` value is `"full"`.

#### `shell-pop-cleanup-buffer-at-process-exit` (Default: `t`)

If non-nil, cleans up the shell's buffer automatically after its underlying process exits.

### Hooks

- `shell-pop-in-hook`: Runs before the shell buffer pops up.
- `shell-pop-in-after-hook`: Runs after the shell buffer pops up.
- `shell-pop-out-hook`: Runs before the shell buffer pops out.
- `shell-pop-process-exit-hook`: Runs when the shell's process exits.

### M-x Customize

Use `M-x customize-variable RET shell-pop-shell-type RET` to customize the shell to use. Six pre-set options are: `shell`, `terminal`, `ansi-term`, `eshell`, `vterm`, and `eat`. You can also set your custom shell if you use other configuration.

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
