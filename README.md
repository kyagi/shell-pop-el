shell-pop-el
============
shell-pop.el helps you to use shell easily on Emacs. Only one key action to work.

Installation
----------
Make sure to place shell-pop.el somewhere in the load-path and add the following lines to your .emacs file.

```lisp
(require 'shell-pop)
```

Customization
----------

Use `M-x customize-variable RET shell-pop-shell-type RET` to customize the shell to use.  Four pre-set options are: `shell`, `terminal`, `ansi-term`, and `eshell`.  You can also set your custom shell if you use other configuration.

For `terminal` and `ansi-term` options, you can set the underlying shell by customizing `shell-pop-term-shell`.  By default, `/bin/bash` is used, but you can also use `/bin/tcsh`, `/bin/zsh` or others.

Use `M-x customize-group RET shell-pop RET` to set further options such as hotkey, window height and position.
You can get a full frame layout by setting `full` to `shell-pop-window-position`.

After saving the customized values, your .emacs file will be like as follows. 

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
 '(shell-pop-window-height 30)
 '(shell-pop-window-position "bottom"))
 ```

Usage
----------
Just hit the hot key you customized for `shell-pop-universal-key`.  
For example, `C-t` for the above case.  

You can have multiple shell buffers if you run shell-pop with universal-arguments.  
For example, `C-u 2 C-t`. You can increase the number(`2`, `3`, `4`, ...) as much as you want for different shell buffers.

Screenshot
----------
![screenshot](https://raw.github.com/kyagi/shell-pop-el/master/screenshot.png)

EmacsWiki (No longer updated)
----------
http://www.emacswiki.org/emacs/ShellPop
