;;; shell-pop.el --- helps you to use shell easily on Emacs. Only one key action to work.

;; Copyright (C) 2009, 2010, 2011, 2012  Kazuo Yagi

;; Author:        Kazuo YAGI <kazuo.yagi@gmail.com>
;; Maintainer:    Kazuo YAGI <kazuo.yagi@gmail.com>
;; Created:       2009-05-31 23:57:08
;; Last-Updated:  2012-10-24 17:37:48
;; Keywords:      shell, terminal, tools
;; Compatibility: GNU Emacs 23.x, 24.x

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a utility which helps you pop up and pop out shell buffer
;; window easily. Just do M-x shell-pop, and it is strongly recommended
;; to assign one hot-key to this function.
;;
;; I hope this is useful for you, and ENJOY YOUR HAPPY HACKING!

;;; Configuration:
;;
;; You can choose your favorite internal mode such as `shell', `terminal',
;; `ansi-term', and `eshell'. Also you can use any shell such as
;; `/bin/bash', `/bin/tcsh', `/bin/zsh' as you like.
;;
;; A configuration sample for your .emacs is as follows.
;;
;; (require 'shell-pop)
;; ; The default key-bindings to run shell-pop.
;; (shell-pop-set-universal-key (kbd "\C-t"))
;; ; You can choose the internal mode from "shell", "terminal", "ansi-term", and "eshell".
;; (shell-pop-set-internal-mode "ansi-term")
;; ; You can choose your favorite shell to run.
;; (shell-pop-set-internal-mode-shell "/bin/zsh")
;; ; The number for the percentage for selected window.
;; ; If 100, shell-pop use the whole of selected window, not spliting. 
;; (shell-pop-set-window-height 60)
;; ; The position for shell-pop window. You can choose "top" or "bottom". 
;; (shell-pop-set-window-position "bottom")
;; ; The default directory when shell-pop invokes
;; (shell-pop-set-default-directory "/Users/kyagi/git")

;;; Code:
(require 'term)

(defvar shell-pop-last-buffer nil)
(defvar shell-pop-last-window nil)
(defvar shell-pop-window-height 30) ; percentage for shell-buffer window height
(defvar shell-pop-window-position "bottom")
(defvar shell-pop-default-directory nil)
(defvar shell-pop-universal-key nil)

(defvar shell-pop-internal-mode "shell")
(defvar shell-pop-internal-mode-buffer "*shell*")
(defvar shell-pop-internal-mode-func '(lambda () (shell)))
(defvar shell-pop-internal-mode-shell "/bin/bash")

(defvar shell-pop-internal-mode-list
  (list
    ; mode, buffer, function
    '("shell"     "*shell*"     '(lambda () (shell)))
    '("terminal"  "*terminal*"  '(lambda () (term shell-pop-internal-mode-shell)))
    '("ansi-term" "*ansi-term*" '(lambda () (ansi-term shell-pop-internal-mode-shell)))
    '("eshell"    "*eshell*"    '(lambda () (eshell)))))

(defun shell-pop-set-window-height (number)
  (interactive "nInput the number for the percentage of \
selected window height (10-100): ")
  (setq shell-pop-window-height number))

(defun shell-pop-set-window-position (position)
  (interactive "sInput the position for shell-pop (top|bottom): ")
  (setq shell-pop-window-position position))

(defun shell-pop-set-internal-mode (mode)
  (interactive "sInput your favorite mode (shell|terminal|ansi-term|eshell): ")
  (setq shell-pop-internal-mode mode)
  (if (string= shell-pop-internal-mode "ansi-term")
    (define-key term-raw-map shell-pop-universal-key 'shell-pop))
  (if (catch 'found
        (dolist (l shell-pop-internal-mode-list)
          (if (string-match mode (car l))
              (progn
                (setq shell-pop-internal-mode-buffer (nth 1 l))
                (setq shell-pop-internal-mode-func (nth 2 l))
                (throw 'found t)))))
      t
    nil))

(defun shell-pop-set-internal-mode-shell (shell)
  (interactive (list (read-from-minibuffer "Input your favorite shell:"
                                           shell-pop-internal-mode-shell)))
  (setq shell-pop-internal-mode-shell shell))

(defun shell-pop-check-internal-mode-buffer ()
  (if (get-buffer shell-pop-internal-mode-buffer)
    (if (term-check-proc shell-pop-internal-mode-buffer)
        shell-pop-internal-mode-buffer
      (kill-buffer shell-pop-internal-mode-buffer)
      nil))
  shell-pop-internal-mode-buffer)

(defun shell-pop-get-internal-mode-buffer-window ()
  (get-buffer-window (shell-pop-check-internal-mode-buffer)))

(defun shell-pop ()
  (interactive)
  (if (equal (buffer-name) shell-pop-internal-mode-buffer)
      (shell-pop-out)
    (shell-pop-up)))

(defun shell-pop-up ()
  (let ((w (shell-pop-get-internal-mode-buffer-window)))
    (if w
        (select-window w)
      (progn ; save shell-pop-last-buffer and shell-pop-last-window to return
        (setq shell-pop-last-buffer (buffer-name))
        (setq shell-pop-last-window (selected-window))
        (if (not (eq shell-pop-window-height 100))
            (progn
              (split-window (selected-window)
                            (if (string= shell-pop-window-position "bottom")
                                (round (* (window-height)
                                          (/ (- 100 shell-pop-window-height) 100.0)))
                              (round (* (window-height) (/ shell-pop-window-height 100.0)))))
              (if (string= shell-pop-window-position "bottom")
                  (other-window 1))))
        (if (and shell-pop-default-directory (file-directory-p shell-pop-default-directory))
            (cd shell-pop-default-directory))
        (if (not (get-buffer shell-pop-internal-mode-buffer))
            (funcall (eval shell-pop-internal-mode-func))
          (switch-to-buffer shell-pop-internal-mode-buffer))))))

(defun shell-pop-out ()
  (if (not (eq shell-pop-window-height 100))
      (progn
        (delete-window)
        (if (string= shell-pop-window-position "bottom")
            (select-window shell-pop-last-window))))
  (switch-to-buffer shell-pop-last-buffer))

(defun shell-pop-window-position-p ()
  shell-pop-window-position)

(defun shell-pop-set-universal-key (key)
  (interactive)
  (global-set-key key 'shell-pop)
  (setq shell-pop-universal-key key))

(defun shell-pop-set-default-directory (path)
  (interactive)
  (setq shell-pop-default-directory path))

(provide 'shell-pop)

;;; shell-pop.el ends here.
