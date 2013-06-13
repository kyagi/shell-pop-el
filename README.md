shell-pop-el
============
shell-pop.el helps you to use shell easily on Emacs. Only one key action to work.

Installation
----------
Make sure to place shell-pop.el somewhere in the load-path and add the following lines to your .emacs file.

    (require 'shell-pop)

Customization
----------

Use `M-x customize-variable RET shell-pop-shell-type RET` to customize the shell to use.  Four pre-set options are: `shell`, `terminal`, `ansi-term`, and `eshell`.  You can also set your custom shell if you use other configuration.

For `terminal` and `ansi-term` options, you can set the underlying shell by customizing `shell-pop-term-shell`.  By default, `/bin/bash` is used, but you can also use `/bin/tcsh`, `/bin/zsh` or others.

Use `M-x customize-group RET shell-pop RET` to set further options such as hotkey, window height and position.

Usage
----------
Just hit the key you bind to `shell-pop-set-universal-key'. For example, Ctrl+t above.

Screenshot
----------
![screenshot](https://raw.github.com/kyagi/shell-pop-el/master/screenshot.png)

EmacsWiki (No longer updated)
----------
http://www.emacswiki.org/emacs/ShellPop
