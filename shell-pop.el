;;; shell-pop.el --- helps you to use shell easily on Emacs. Only one key action to work.

;; Copyright (C) 2009, 2010, 2011, 2012, 2013  Kazuo Yagi

;; Author:        Kazuo YAGI <kazuo.yagi@gmail.com>
;; Maintainer:    Kazuo YAGI <kazuo.yagi@gmail.com>
;; URL:           http://github.com/kyagi/shell-pop-el
;; Version:       0.3
;; Created:       2009-05-31 23:57:08
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

(eval-when-compile
  (defvar shell-pop-universal-key)
  (defvar eshell-last-input-start)
  (defvar eshell-last-input-end))

(declare-function eshell-send-input "esh-mode")

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
(defvar shell-pop-last-shell-buffer-index 1)
(defvar shell-pop-last-shell-buffer-name "")
;; internal}

(defcustom shell-pop-window-height 30
  "Percentage for shell-buffer window height."
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (and (integerp x)
                            (<= x 100)
                            (<= 0 x)))))
  :group 'shell-pop)

(defcustom shell-pop-full-span nil
  "If non-nil, the shell spans full width of a window"
  :type 'boolean
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
  (setq shell-pop-internal-mode (nth 0 value)
        shell-pop-internal-mode-buffer (nth 1 value)
        shell-pop-internal-mode-func (nth 2 value))
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

(defcustom shell-pop-in-hook nil
  "Hook run when buffer pop-up"
  :type 'hook
  :group 'shell-pop)

(defcustom shell-pop-out-hook nil
  "Hook run when buffer pop-out"
  :type 'hook
  :group 'shell-pop)

(defun shell-pop--shell-buffer-name (index)
  (if (string-match "*\\'" shell-pop-internal-mode-buffer)
      (replace-regexp-in-string
       "*\\'" (format "-%d*" index) shell-pop-internal-mode-buffer)
    (format "%s-%d" shell-pop-internal-mode-buffer index)))

(defun shell-pop-check-internal-mode-buffer (index)
  (let ((bufname (shell-pop--shell-buffer-name index)))
    (when (get-buffer bufname)
      (if (or (term-check-proc bufname)
              (string= shell-pop-internal-mode "eshell"))
          bufname
        (kill-buffer bufname)
        nil))
    bufname))

(defun shell-pop-get-internal-mode-buffer-window (index)
  (get-buffer-window (shell-pop-check-internal-mode-buffer index)))

;;;###autoload
(defun shell-pop (arg)
  (interactive "P")
  (if (string= (buffer-name) shell-pop-last-shell-buffer-name)
      (shell-pop-out)
    (shell-pop-up (or arg shell-pop-last-shell-buffer-index))))

(defun shell-pop--cd-to-cwd-eshell (cwd)
  (insert (concat "cd " cwd))
  (eshell-send-input)
  (let ((inhibit-read-only t))
    (delete-region
     (save-excursion
       (goto-char eshell-last-input-start)
       (beginning-of-line)
       (point))
     eshell-last-input-end)))

(defun shell-pop--cd-to-cwd-shell (cwd)
  (comint-kill-input)
  (insert (concat "cd " cwd))
  (let ((comint-process-echoes t))
    (comint-send-input))
  (recenter 0))

(defun shell-pop--cd-to-cwd-term (cwd)
  (term-send-raw-string (concat "cd " cwd "\n"))
  (term-send-raw-string "\C-l"))

(defun shell-pop--cd-to-cwd (cwd)
  (cond ((string= shell-pop-internal-mode "eshell")
         (shell-pop--cd-to-cwd-eshell cwd))
        ((string= shell-pop-internal-mode "shell")
         (shell-pop--cd-to-cwd-shell cwd))
        (t
         (shell-pop--cd-to-cwd-term cwd))))

(defsubst shell-pop--calculate-window-size ()
  (let ((height (if shell-pop-full-span
                    (window-height (frame-root-window))
                  (window-height))))
    (if (or (string= shell-pop-window-position "bottom")
            shell-pop-full-span)
        (round (* height (/ (- 100 shell-pop-window-height) 100.0)))
      (round (* height (/ shell-pop-window-height 100.0))))))

(defun shell-pop--switch-to-shell-buffer (index)
  (let ((bufname (shell-pop--shell-buffer-name index)))
    (if (get-buffer bufname)
        (switch-to-buffer bufname)
      (funcall (eval shell-pop-internal-mode-func))
      (rename-buffer bufname))
    (setq shell-pop-last-shell-buffer-name bufname
          shell-pop-last-shell-buffer-index index)))

(defun shell-pop--translate-position (pos)
  (cond
    ((string= pos "top") 'above)
    ((string= pos "bottom") 'below)))

(defun shell-pop-get-unused-internal-mode-buffer-window ()
  (let ((finish nil)
        (index 1)
        bufname)
    (while (not finish)
      (setq bufname (shell-pop--shell-buffer-name index))
      (if (get-buffer bufname)
          (setq index (1+ index))
        (setq finish t)))
    (cons index (get-buffer-window bufname))))

(defun shell-pop-up (index)
  (run-hooks 'shell-pop-up-hook)
  (let ((w (if (listp index)
               (let ((ret (shell-pop-get-unused-internal-mode-buffer-window)))
                 (setq index (car ret))
                 (cdr ret))
             (shell-pop-get-internal-mode-buffer-window index)))
        (cwd (replace-regexp-in-string "\\\\" "/" default-directory)))
    (when (string= shell-pop-window-position "full")
      (window-configuration-to-register :shell-pop)
      (delete-other-windows))
    (if w
        (select-window w)
      ;; save shell-pop-last-buffer and shell-pop-last-window to return
      (setq shell-pop-last-buffer (buffer-name)
            shell-pop-last-window (selected-window))
      (when (and (not (= shell-pop-window-height 100))
                 (not (string= shell-pop-window-position "full")))
        (let ((new-window (shell-pop-split-window)))
          (when (or (string= shell-pop-window-position "bottom")
                    shell-pop-full-span)
            (select-window new-window))))
      (when (and shell-pop-default-directory (file-directory-p shell-pop-default-directory))
        (cd shell-pop-default-directory))
      (shell-pop--switch-to-shell-buffer index))
    (when (and shell-pop-autocd-to-working-dir
               (not (string= cwd default-directory)))
      (shell-pop--cd-to-cwd cwd))))

(defun shell-pop-out ()
  (run-hooks 'shell-pop-out-hook)
  (if (string= shell-pop-window-position "full")
      (jump-to-register :shell-pop)
    (when (not (= shell-pop-window-height 100))
      (delete-window)
      (when (string= shell-pop-window-position "bottom")
        (select-window shell-pop-last-window)))
    (switch-to-buffer shell-pop-last-buffer)))

(defun shell-pop-split-window ()
  (unless (string= shell-pop-window-position "full")
    (cond
     (shell-pop-full-span
      (split-window
       (frame-root-window) ; window
       (shell-pop--calculate-window-size) ; size
       (shell-pop--translate-position shell-pop-window-position))) ; side
     (t
      (split-window (selected-window) (shell-pop--calculate-window-size))))))

(provide 'shell-pop)

;;; shell-pop.el ends here
