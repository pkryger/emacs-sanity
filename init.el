;;; init.el --- Minimal Emacs config for my own sanity
;;;
;;; Commentary:
;;; This is a minimal configuration for Emacs, that has the most important
;;; (to me) bits and pieces from exordium and my config that decreases a
;;; possibility of me loosing my sanity.
;;;
;;; Code:

(setf gc-cons-threshold most-positive-fixnum)
(setq custom-file
      (expand-file-name (locate-user-emacs-file "emacs-custom.el")))

;; setup packages management
(require 'package)
(push (cons "melpa" "https://melpa.org/packages/")
      package-archives)
(package-initialize)

(let ((last-commit (string-to-number
                    (let ((default-directory user-emacs-directory))
                      (shell-command-to-string
                       "git log -1 --format=%ct | tr -d '\n'"))))
      (last-archive-contents
       (apply #'max
              (or
               (mapcar (lambda (archive)
                         (let ((contents-file (file-name-concat
                                               package-user-dir
                                               "archives"
                                               (car archive)
                                               "archive-contents")))
                           (if (file-exists-p contents-file)
                               (time-convert (file-attribute-modification-time
                                              (file-attributes contents-file))
                                             'integer)
                             0)))
                       package-archives)
               (list 0)))))
  (when (or
         (null package-archive-contents)
         (< last-archive-contents last-commit))
    (package-refresh-contents)))

(require 'use-package)
(use-package bind-key
  :ensure t)
(use-package diminish
  :ensure t)

;; custom functions
(defun pk/iterm-cut-base64 (text)
  "Take TEXT and send it to iterm to copy."
  (interactive)
  (let ((base-64 (base64-encode-string text :no-line-break)))
    (send-string-to-terminal (concat "\e]1337;Copy=:" base-64 "\a"))))
(setq interprogram-cut-function 'pk/iterm-cut-base64)

(defun pk/duplicate-line-or-region (arg)
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


;; Some sane defaults
(setq-default indent-tabs-mode nil
	          tab-width 4
	          fill-column 79)

(menu-bar-mode -1)
(show-paren-mode)
(delete-selection-mode)
(column-number-mode)
(global-display-line-numbers-mode)

;; common keys
(bind-keys
 ("C-`" . #'kill-this-buffer)
 ("M-g" . #'goto-line)
 ("C-z" . #'undo)
 ("C-|" . #'display-fill-column-indicator-mode)
 ("C-c d" . #'pk/duplicate-line-or-region))

(use-package prog-mode
  :hook (prog-mode . electric-pair-mode)
        (prog-mode . display-fill-column-indicator-mode))

(use-package xt-mouse
  :init
  (defmacro track-mouse (&rest _)
    "Don't track mouse.")
  :config
  (xterm-mouse-mode))

(use-package ace-window
  :ensure t
  :diminish
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-translate-char-function #'(lambda (c)
                                  (if (eq ?\M-o c) ?n c)))
  :bind ("M-o" . #'ace-window))

(use-package avy
  :custom
  (avy-all-windows 'all-frames)
  :bind ("C-c j" . #'avy-goto-word-or-subword-1))

(use-package gcmh
  :ensure t
  :custom
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold pk/gc-cons-threshold)
  :config
  (gcmh-mode))

(use-package iedit
  :ensure t
  :custom
  (iedit-toggle-key-default nil)
  :init
  (use-package isearch
    :ensure nil
    :defer t
    :bind
    (:map isearch-mode-map
     ("C-c ;" . #'iedit-mode-from-isearch)))
  (use-package help
    :ensure nil
    :defer t
    :bind
    (:map help-map
     ("C-;" . #'iedit-mode-toggle-on-function)))

  :bind
  (("C-c ;" . #'iedit-mode)
   :map esc-map
   ("C-;" . #'iedit-execute-last-modification)))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package magit
  :ensure t)

(provide 'init)

;;; init.el ends here
