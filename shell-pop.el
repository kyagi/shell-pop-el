;;; shell-pop.el --- helps you to use shell easily on Emacs. Only one key action to work.

;; Copyright (C) 2009, 2010, 2011, 2012, 2013  Kazuo Yagi

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
;; window easily.  Just do M-x shell-pop, and it is strongly recommended
;; to assign one hot-key to this function.
;;
;; I hope this is useful for you, and ENJOY YOUR HAPPY HACKING!

;;; Configuration:
;;
;; Use M-x customize-variable RET `shell-pop-shell-type' RET to
;; customize the shell to use.  Four pre-set options are: `shell',
;; `terminal', `ansi-term', and `eshell'.  You can also set your
;; custom shell if you use other configuration.

;; For `terminal' and `ansi-term' options, you can set the underlying
;; shell by customizing `shell-pop-term-shell'.  By default, `/bin/bash'
;; is used, but you can also use `/bin/tcsh', `/bin/zsh' or others.
;;
;; Use M-x customize-group RET shell-pop RET to set further options
;; such as hotkey, window height and position.

;;; Code:

(require 'term)

(defgroup shell-pop ()
  "Shell-pop"
  :group 'shell)

;; internal{
(defvar shell-pop-internal-mode "shell")
(defvar shell-pop-internal-mode-buffer "*shell*")
(defvar shell-pop-internal-mode-func '(lambda () (shell)))
(defvar shell-pop-last-buffer nil)
(defvar shell-pop-last-window nil)
;; internal}

(defcustom shell-pop-window-height 30
  "Percentage for shell-buffer window height."
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (and (integerp x)
                            (<= x 100)
                            (<= 0 x)))))
  :group 'shell-pop)

(defcustom shell-pop-window-position "bottom"
  "Position of the popped buffer."
  :type '(choice
          (const "top")
          (const "bottom")
          (const "full"))
  :group 'shell-pop)

(defcustom shell-pop-default-directory nil
  "If non-nil, when first starting the shell, cd to this directory."
  :type 'directory
  :group 'shell-pop)

(defun shell-pop--set-shell-type (symbol value)
  (set-default symbol value)
  (setq shell-pop-internal-mode (nth 0 value))
  (setq shell-pop-internal-mode-buffer (nth 1 value))
  (setq shell-pop-internal-mode-func (nth 2 value))
  (when (and (string= shell-pop-internal-mode "ansi-term")
             shell-pop-universal-key)
    (define-key term-raw-map (read-kbd-macro shell-pop-universal-key) 'shell-pop)))

(defcustom shell-pop-shell-type '("shell" "*shell*" (lambda () (shell)))
  "Type of shell that is launched when first popping into a shell.

The value is a list with these items:
 - Internal name of the shell type.  This should be unique \"id\".
 - Name of the buffer this shell opens.
 - A function that launches the shell."
  :type '(choice
          (list :tag "Custom" string string function)
          (const :tag "shell"
                 ("shell" "*shell*" (lambda () (shell))))
          (const :tag "terminal"
                 ("terminal" "*terminal*" (lambda () (term shell-pop-term-shell))))
          (const :tag "ansi-term"
                 ("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
          (const :tag "eshell"
                 ("eshell" "*eshell*" (lambda () (eshell)))))
  :set 'shell-pop--set-shell-type
  :group 'shell-pop)

(defcustom shell-pop-term-shell "/bin/bash"
  "Shell used in `term' and `ansi-term'."
  :type 'string
  :group 'shell-pop)

(defcustom shell-pop-autocd-to-working-dir t
  "If non-nil, automatically `cd' to working directory of the
buffer from which the `shell-pop' command was invoked."
  :type 'boolean
  :group 'shell-pop)

(defun shell-pop--set-universal-key (symbol value)
  (set-default symbol value)
  (when value (global-set-key (read-kbd-macro value) 'shell-pop))
  (when (and (string= shell-pop-internal-mode "ansi-term")
             shell-pop-universal-key)
    (define-key term-raw-map (read-kbd-macro value) 'shell-pop)))

;;;###autoload
(defcustom shell-pop-universal-key nil
  "Key binding used to pop in and out of the shell.

The input format is the same as that of `kbd'."
  :type '(choice string (const nil))
  :set 'shell-pop--set-universal-key
  :group 'shell-pop)

(defun shell-pop-check-internal-mode-buffer ()
  (when (get-buffer shell-pop-internal-mode-buffer)
    (if (or (term-check-proc shell-pop-internal-mode-buffer)
            (string= shell-pop-internal-mode "eshell"))
        shell-pop-internal-mode-buffer
      (kill-buffer shell-pop-internal-mode-buffer)
      nil))
  shell-pop-internal-mode-buffer)

(defun shell-pop-get-internal-mode-buffer-window ()
  (get-buffer-window (shell-pop-check-internal-mode-buffer)))

;;;###autoload
(defun shell-pop ()
  (interactive)
  (if (equal (buffer-name) shell-pop-internal-mode-buffer)
      (shell-pop-out)
    (shell-pop-up)))

(defun shell-pop-up ()
  (let ((w (shell-pop-get-internal-mode-buffer-window))
        (cwd (replace-regexp-in-string "\\\\" "/" default-directory)))
    (when (string= shell-pop-window-position "full")
      (window-configuration-to-register :shell-pop)
      (delete-other-windows))
    (if w
        (select-window w)
      ;; save shell-pop-last-buffer and shell-pop-last-window to return
      (setq shell-pop-last-buffer (buffer-name))
      (setq shell-pop-last-window (selected-window))
      (when (and (not (= shell-pop-window-height 100))
                 (not (string= shell-pop-window-position "full")))
        (split-window (selected-window)
                      (if (string= shell-pop-window-position "bottom")
                          (round (* (window-height)
                                    (/ (- 100 shell-pop-window-height) 100.0)))
                        (round (* (window-height) (/ shell-pop-window-height 100.0)))))
        (if (string= shell-pop-window-position "bottom")
            (other-window 1)))
      (when (and shell-pop-default-directory (file-directory-p shell-pop-default-directory))
        (cd shell-pop-default-directory))
      (if (not (get-buffer shell-pop-internal-mode-buffer))
          (funcall (eval shell-pop-internal-mode-func))
        (switch-to-buffer shell-pop-internal-mode-buffer)))
    (when (and shell-pop-autocd-to-working-dir
               (not (equal cwd default-directory)))
      (if (string= shell-pop-internal-mode "eshell")
          (progn
            (insert (concat "cd " cwd))
            (eshell-send-input)
            (let ((inhibit-read-only t))
              (delete-region
               (save-excursion
                 (goto-char eshell-last-input-start)
                 (beginning-of-line)
                 (point))
               eshell-last-input-end)))
        (term-send-raw-string (concat "cd " cwd "\n"))
        (term-send-raw-string "\C-l")))))

(defun shell-pop-out ()
  (if (string= shell-pop-window-position "full")
      (jump-to-register :shell-pop)
    (when (not (= shell-pop-window-height 100))
      (delete-window)
      (when (string= shell-pop-window-position "bottom")
        (select-window shell-pop-last-window)))
    (switch-to-buffer shell-pop-last-buffer)))

(provide 'shell-pop)

;;; shell-pop.el ends here
