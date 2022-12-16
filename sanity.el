;;; sanity.el --- Minimal Emacs config for my own sanity
;;;
;;; Commentary:
;;; This is a minimal configuration for Emacs, that has the most important
;;; (to me) bits and pieces from exordium and my config that decreases a
;;; possibility of me loosing my sanity.
;;;
;;; Code:

(setf gc-cons-threshold most-positive-fixnum)

;; setup default dir to load file -> this will force package to use it
(setq user-init-file (or load-file-name (buffer-file-name))
      user-emacs-directory (file-name-directory user-init-file))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (gcmh iedit magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; setup package
(require 'package)
(let ((melpa (cons "melpa" "http://melpa.org/packages/")))
  (unless (member melpa package-archives)
    (add-to-list 'package-archives melpa)))
(package-initialize)
(let (refreshed
      (packages package-selected-packages))
  (dolist (package packages)
    (unless (package-installed-p package)
      (unless refreshed
        (package-refresh-contents)
        (setq refreshed t))
      (package-install package))))

;; Some sane defaults
(setq-default indent-tabs-mode nil
	          tab-width 4
	          fill-column 79)

(fset 'yes-or-no-p 'y-or-n-p)

(show-paren-mode t)
(menu-bar-mode -1)
(delete-selection-mode t)
(column-number-mode 1)

;; common keys
(global-set-key (kbd "C-`") #'kill-this-buffer)
(global-set-key (kbd "M-g") #'goto-line)
(global-set-key (kbd "C-z") #'undo)
(global-set-key (kbd "M-o") #'other-window)

(setq electric-pair-open-newline-between-pairs t)
(add-hook 'prog-mode-hook
          #'electric-pair-mode)

;; mouse setup in iterm
(require 'xt-mouse)
(xterm-mouse-mode)
(defmacro track-mouse (&rest _)
  "Don't track mouse.")

;; copy into iterm
(defun iterm-cut-base64 (text)
  "Take TEXT and send it to iterm to copy."
  (interactive)
  (let ((base-64 (base64-encode-string text :no-line-break)))
    (send-string-to-terminal (concat "\e]1337;Copy=:" base-64 "\a"))))
(setq interprogram-cut-function 'iterm-cut-base64)

;; duplicate line
(defun duplicate-line-or-region (arg)
  "Duplicate current line or region 2 or ARG times, leaving point in lower line."
  (interactive "*p")
  ;; Save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  (let ((bol (if mark-active (region-beginning)
               (save-excursion (beginning-of-line) (point))))
        eol
        (num-lines (if mark-active
                       (count-lines (region-beginning) (region-end))
                     1))
        (col (current-column)))
    (save-excursion
      (if mark-active
          (setq eol (region-end))
        (end-of-line)
        (setq eol (point)))
      ;; Disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t))
        ;; Insert the line arg times
        (dotimes (i (if (> arg 0) arg 1))
          (unless (string-suffix-p "\n" line)
            (newline))
          (insert line)))
      ;; Create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ;; Move the point to the lowest line
    (forward-line (* arg num-lines))
    (when (= num-lines 1)
      ;; Leave the cursor an the same column if we duplicated 1 line
      (move-to-column col))))
(global-set-key (kbd "C-c d") #'duplicate-line-or-region)

;;; line numbers
(defcustom exordium-inhibit-line-numbers-modes '(eshell-mode
                                                 shell-mode
                                                 help-mode
                                                 compilation-mode
                                                 iwyu-mode
                                                 Info-mode
                                                 calendar-mode
                                                 treemacs-mode
                                                 org-mode
                                                 rtags-rdm-mode
                                                 rtags-diagnostics-mode
                                                 eww-mode
                                                 dired-mode
                                                 image-mode)
  "List of modes for which line numbers should not be displayed."
  :group 'exordium
  :type 'list)

(defun exordium-inhibit-line-numbers-p ()
  "Return t if line numbers should be inhibited."
  (or (minibufferp)
      (cl-member major-mode exordium-inhibit-line-numbers-modes)
      (eq 0 (string-match "*" (buffer-name)))
      (> (buffer-size) (* 2 1024 1024))))

(require 'display-line-numbers)
;;;###autoload
(define-globalized-minor-mode exordium-global-display-line-numbers-mode
  display-line-numbers-mode
  (lambda () (unless (exordium-inhibit-line-numbers-p)
              (display-line-numbers-mode))))

(exordium-global-display-line-numbers-mode t)

;; fill column indicator
(when (not (version< emacs-version "27"))
  (add-hook 'prog-mode-hook
            #'display-fill-column-indicator-mode)
  (global-set-key (kbd "C-|") #'display-fill-column-indicator-mode))

;; magit keybindings
(global-set-key (kbd "C-c g s") #'magit-status)
(global-set-key (kbd "C-c g l") #'magit-log)
(global-set-key (kbd "C-c g b") #'magit-blame)

;; garbage collection based on DOOM setup
(defconst pk/gc-cons-threshold (* 16 1024 1024))
(defun pk/defer-garbage-collection ()
  "Use max value for gc, when in minibuffer.
It's so it won't slow expensive commands and completion frameworks."
  (setf gc-cons-threshold most-positive-fixnum))
(defun pk/restore-garbage-collection ()
  "Get back to the original gc threshold.
Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time
   1 nil (lambda () (setf gc-cons-threshold pk/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'pk/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook #'pk/restore-garbage-collection)

(require 'gcmh)
(setq gcmh-idle-delay 5
      gcmh-high-cons-threshold pk/gc-cons-threshold)
(gcmh-mode)

(provide 'sanity)
;;; sanity.el ends here
