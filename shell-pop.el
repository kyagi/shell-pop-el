;;; shell-pop.el --- helps you to use shell easily on Emacs. Only one key action to work. -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017  Kazuo Yagi

;; Author:        Kazuo YAGI <kazuo.yagi@gmail.com>
;; Maintainer:    Kazuo YAGI <kazuo.yagi@gmail.com>
;; URL:           http://github.com/kyagi/shell-pop-el
;; Version:       0.64
;; Created:       2009-05-31 23:57:08
;; Keywords:      shell, terminal, tools
;; Compatibility: GNU 24.x
;; Package-Requires: ((emacs "26.1"))

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
;; shell by customizing `shell-pop-term-shell'.  By default,
;; `shell-file-name' is used.
;;
;; Use M-x customize-group RET shell-pop RET to set further options
;; such as hotkey, window height and position.

;;; Code:

(eval-when-compile
  (defvar shell-pop-universal-key)
  (defvar eshell-last-input-start)
  (defvar eshell-last-input-end))

(declare-function eshell-send-input "esh-mode")
(declare-function eshell-reset "esh-mode")
(declare-function eshell-process-interact "esh-proc")

(require 'term)

(defgroup shell-pop ()
  "Shell-pop"
  :group 'shell)

;; internal{
(defvar shell-pop-internal-mode "shell")
(defvar shell-pop-internal-mode-buffer "*shell*")
(defvar shell-pop-internal-mode-func '(lambda () (shell)))
(defvar shell-pop-last-shell-buffer-index 1)
(defvar-local shell-pop--is-shell-buffer nil
  "Non-nil if the current buffer is managed by shell-pop.")
;; internal}

(defcustom shell-pop-window-size 30
  "Percentage for shell-buffer window size."
  :type '(restricted-sexp
          :match-alternatives
          ((lambda (x) (and (integerp x)
                            (<= x 100)
                            (<= 0 x)))))
  :group 'shell-pop)
(defvaralias 'shell-pop-window-height 'shell-pop-window-size)

(defcustom shell-pop-full-span nil
  "If non-nil, the shell spans full width of a window"
  :type 'boolean
  :group 'shell-pop)

(defcustom shell-pop-window-position "bottom"
  "Position of the popped buffer."
  :type '(choice
          (const "top")
          (const "bottom")
          (const "left")
          (const "right")
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

(defcustom shell-pop-term-shell (or explicit-shell-file-name
                                    (getenv "ESHELL")
                                    shell-file-name)
  "Shell used in `term' and `ansi-term'."
  :type 'string
  :group 'shell-pop)

(defcustom shell-pop-autocd-to-working-dir t
  "If non-nil, automatically `cd' to working directory of the
buffer from which the `shell-pop' command was invoked."
  :type 'boolean
  :group 'shell-pop)

(defcustom shell-pop-restore-window-configuration t
  "If non-nil, restore the original window configuration when
shell-pop is closed.

shell-pop's window is deleted in any case. This variable has no
effect when `shell-pop-window-position' value is \"full\"."
  :type 'boolean
  :group 'shell-pop)

(defcustom shell-pop-cleanup-buffer-at-process-exit t
  "If non-nil, cleanup the shell's buffer after its process exits."
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
  "Hook run before buffer pop-up."
  :type 'hook
  :group 'shell-pop)

(defcustom shell-pop-in-after-hook nil
  "Hook run after buffer pop-up."
  :type 'hook
  :group 'shell-pop)

(defcustom shell-pop-out-hook nil
  "Hook run before buffer pop-out"
  :type 'hook
  :group 'shell-pop)

(defcustom shell-pop-process-exit-hook nil
  "Hook run when the shell's process exits."
  :type 'hook
  :group 'shell-pop)

(defun shell-pop--shell-buffer-name (index)
  (if (string-match-p "*\\'" shell-pop-internal-mode-buffer)
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
  (if (or shell-pop--is-shell-buffer
          (window-parameter nil 'shell-pop-is-window))
      (if (null arg)
          (shell-pop-out)
        (shell-pop--switch-to-shell-buffer (prefix-numeric-value arg)))
    (shell-pop-up (or arg shell-pop-last-shell-buffer-index))))

(defun shell-pop--cd-to-cwd-eshell (cwd)
  (if (eshell-process-interact 'process-live-p)
      (message "Won't change CWD because of running process.")
    (setq default-directory cwd)
    (eshell-reset)))

(defun shell-pop--cd-to-cwd-shell (cwd)
  (goto-char (point-max))
  (comint-kill-input)
  (insert (concat "cd " (shell-quote-argument cwd)))
  (let ((comint-process-echoes t))
    (comint-send-input))
  (recenter 0))

(defun shell-pop--cd-to-cwd-term (cwd)
  (term-send-raw-string (concat "cd " (shell-quote-argument cwd) "\n"))
  (term-send-raw-string "\C-l"))

(defun shell-pop--cd-to-cwd (cwd)
  (let ((abspath (expand-file-name cwd)))
    (cond ((string= shell-pop-internal-mode "eshell")
           (shell-pop--cd-to-cwd-eshell abspath))
          ((string= shell-pop-internal-mode "shell")
           (shell-pop--cd-to-cwd-shell abspath))
          (t
           (shell-pop--cd-to-cwd-term abspath)))))

(defsubst shell-pop--full-p ()
  (or (string= shell-pop-window-position "full")
      (>= shell-pop-window-height 100)))

(defsubst shell-pop--split-side-p ()
  (member shell-pop-window-position '("left" "right")))

(defun shell-pop--calculate-window-size ()
  (let* ((win (and shell-pop-full-span (frame-root-window)))
         (size (if (shell-pop--split-side-p)
                   (window-width)
                 (window-height win))))
    (round (* size (/ (- 100 shell-pop-window-height) 100.0)))))

(defun shell-pop--kill-and-delete-window ()
  ;; Explicitly target the buffer's window to prevent deleting the user's active
  ;; window if shell exits in the background.
  (let ((win (get-buffer-window (current-buffer))))
    (when (and (window-live-p win) (eq (window-deletable-p win) t))
      (delete-window win))))

(defun shell-pop--set-exit-action ()
  (if (string= shell-pop-internal-mode "eshell")
      (add-hook 'eshell-exit-hook 'shell-pop--kill-and-delete-window nil t)
    (let ((process (get-buffer-process (current-buffer))))
      (when process
        (set-process-sentinel
         process
         (lambda (proc change)
           (when (string-match-p "\\(?:finished\\|exited\\)" change)
             (run-hooks 'shell-pop-process-exit-hook)

             (let* ((proc-buf (process-buffer proc))
                    ;; Safely get the window ONLY on the current frame
                    (proc-win (when (buffer-live-p proc-buf)
                                (get-buffer-window proc-buf)))
                    ;; Safely grab the target buffer BEFORE we kill the process
                    ;; buffer since killing it might cause Emacs to destroy
                    ;; proc-win entirely.
                    (target-buf (when (window-live-p proc-win)
                                  (window-parameter proc-win
                                                    'shell-pop-last-buffer))))

               (when (and shell-pop-cleanup-buffer-at-process-exit
                          (buffer-live-p proc-buf))
                 (kill-buffer proc-buf))

               ;; Only manipulate the window if it was visible on THIS frame
               (when (window-live-p proc-win)
                 ;; FIX: Clear identity to prevent "zombie" toggles if window survives
                 (set-window-parameter proc-win 'shell-pop-is-window nil)
                 (if (eq (window-deletable-p proc-win) t)
                     (delete-window proc-win)
                   ;; If it's the last window, safely swap the buffer
                   (set-window-buffer
                    proc-win
                    (if (and target-buf (buffer-live-p target-buf))
                        target-buf
                      (if (fboundp 'get-scratch-buffer-create)
                          (get-scratch-buffer-create)
                        (get-buffer-create "*scratch*"))))))))))))))

(defun shell-pop--switch-to-shell-buffer (index)
  (let ((bufname (shell-pop--shell-buffer-name index)))
    (if (get-buffer bufname)
        (switch-to-buffer bufname)
      (funcall (eval shell-pop-internal-mode-func))
      (rename-buffer bufname)
      (shell-pop--set-exit-action))
    (setq shell-pop--is-shell-buffer t)
    (setq shell-pop-last-shell-buffer-index index)))

(defun shell-pop--translate-position (pos)
  (cond
   ((string= pos "top") 'above)
   ((string= pos "bottom") 'below)
   ((string= pos "left") 'left)
   ((string= pos "right") 'right)))

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
  (run-hooks 'shell-pop-in-hook)
  (let* ((w (if (listp index)
                (let ((ret (shell-pop-get-unused-internal-mode-buffer-window)))
                  (setq index (car ret))
                  (cdr ret))
              (shell-pop-get-internal-mode-buffer-window index)))
         (cwd (replace-regexp-in-string "\\\\" "/" default-directory))
         (last-buf (current-buffer))
         (last-win (selected-window))
         ;; Only save full config in full mode so we don't accidentally revert
         ;; independent frame layout changes when closing a standard split
         (win-conf (when (shell-pop--full-p)
                     (list (current-window-configuration) (point-marker)))))

    (when (shell-pop--full-p)
      (delete-other-windows))

    (if w
        (select-window w)
      (unless (shell-pop--full-p)
        (let ((new-window (shell-pop-split-window)))
          (select-window new-window)))
      (when (and shell-pop-default-directory (file-directory-p shell-pop-default-directory))
        (cd shell-pop-default-directory))
      (shell-pop--switch-to-shell-buffer index))

    (set-window-parameter nil 'shell-pop-is-window t)
    (set-window-parameter nil 'shell-pop-last-window last-win)
    (set-window-parameter nil 'shell-pop-last-buffer last-buf)
    (when (shell-pop--full-p)
      (set-window-parameter nil 'shell-pop-window-config win-conf))

    (when (and shell-pop-autocd-to-working-dir
               (not (string= cwd default-directory)))
      (shell-pop--cd-to-cwd cwd))
    (run-hooks 'shell-pop-in-after-hook)))

(defun shell-pop-out ()
  (run-hooks 'shell-pop-out-hook)
  (let* ((win (selected-window))
         (last-win (window-parameter win 'shell-pop-last-window))
         (last-buf (window-parameter win 'shell-pop-last-buffer))
         (win-conf (window-parameter win 'shell-pop-window-config)))
    ;; Strip the identity so the window doesn't trap toggles if it survives
    (set-window-parameter win 'shell-pop-is-window nil)

    (if (and (shell-pop--full-p)
             win-conf
             (window-configuration-p (car win-conf)))
        (progn
          (set-window-configuration (car win-conf))
          (when (marker-buffer (cadr win-conf))
            (goto-char (cadr win-conf))))
      ;; For non-full splits: Always bury the shell buffer out of sight
      (bury-buffer)

      (when (eq (window-deletable-p win) t)
        (delete-window win)
        (when (window-live-p last-win)
          (select-window last-win)))

      ;; The variable is named "restore-window-configuration", but its
      ;; intended behavior is actually to just restore the originating buffer.
      (when shell-pop-restore-window-configuration
        (when (and last-buf (buffer-live-p last-buf))
          (set-window-buffer (selected-window) last-buf))))))

(defun shell-pop-split-window ()
  (unless (shell-pop--full-p)
    (cond
     (shell-pop-full-span
      (split-window
       (window-main-window) ; window
       (shell-pop--calculate-window-size) ; size
       (shell-pop--translate-position shell-pop-window-position))) ; side
     (t
      (split-window (selected-window) (shell-pop--calculate-window-size)
                    (shell-pop--translate-position shell-pop-window-position))))))

(provide 'shell-pop)

;;; shell-pop.el ends here
