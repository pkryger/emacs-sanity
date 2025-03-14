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
(require 'cl-lib)


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


(declare-function dbus-list-activatable-names nil)
(declare-function dbus-ignore-errors nil)
(declare-function dbus-call-method nil)
(declare-function w32-read-registry nil)

(defun pk/themes--is-dark-mode ()
  "Return non-nil if current system UI is in dark mode."
  (let ((script (concat "tell application \"System Events\" "
                        "to tell appearance preferences "
                        "to return dark mode")))
    (ignore-errors
      (cond
       ((fboundp 'mac-do-applescript)
        (string-equal "true" (mac-do-applescript script)))
       ((fboundp 'ns-do-applescript)
        (string-equal "true" (ns-do-applescript script)))
       ((eq system-type 'darwin)
        (string-equal "true" (string-trim
                       (shell-command-to-string
                        (format "osascript -e '%s'" script)))))
       ((eq system-type 'windows-nt)
        (eq 0 (w32-read-registry
               'HKCU
			   "Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize"
			   "AppsUseLightTheme")))
       ((and (eq system-type 'gnu/linux)
             (member 'dbus features)
             (member "org.freedesktop.portal.Desktop"
                     (dbus-list-activatable-names :session)))
        (eq 1 (caar (dbus-ignore-errors
                      (dbus-call-method
                       :session
                       "org.freedesktop.portal.Desktop"
                       "/org/freedesktop/portal/desktop"
                       "org.freedesktop.portal.Settings" "Read"
                       "org.freedesktop.appearance" "color-scheme")))))
       ((and (eq system-type 'gnu/linux)
             (member 'dbus features)
             (cl-search "termux-fix-shebang"
                        (shell-command-to-string
                         "command -v termux-fix-shebang")))
        (string-equal "Night mode: yes"
                      (shell-command-to-string
                       "echo -n $(cmd uimode night 2>&1 </dev/null)")))))))

(require-theme 'modus-themes t)
(setopt modus-themes-to-toggle '(modus-operandi modus-vivendi))

(defun pk/themes--custom-faces ()
  "Tune up some modus themes faces."
  (cl-flet ((foreground-color (face)
              (when-let* ((color (face-attribute face :foreground))
                          ((not (eq color 'unspecified))))
                (list :color color))))
   (modus-themes-with-colors
    (custom-theme-set-faces
     'user
     `(fill-column-indicator
       ((t (,@class :height 1.0
                    :background ,bg-main
                    :foreground ,bg-inactive))))
     `(iedit-occurrence
       ((t (,@class
            :inherit nil
            :box (:line-width -2 ,@(foreground-color
                                    'modus-themes-completion-match-0))))))
     `(iedit-read-only-occurrence
       ((t (,@class
            :inherit nil
            :box (:line-width -2 ,@(foreground-color
                                    'modus-themes-completion-match-1))))))
     `(aw-leading-char-face
       ((t (,@class :foreground ,red-intense
                    :bold t
                    :height 1.5))))))))

(add-hook 'modus-themes-after-load-theme-hook
          #'pk/themes--custom-faces)

(defun pk/themes--follow-system ()
  "Switch theme according to current system setting."
  (when-let* ((desired-theme (if (pk/themes--is-dark-mode)
                                 (cadr modus-themes-to-toggle)
                               (car modus-themes-to-toggle)))
              ((not (eq desired-theme (car custom-enabled-themes)))))
    (pcase desired-theme
      ('modus-operandi (modus-themes-load-operandi))
      ('modus-vivendi (modus-themes-load-vivendi)))))

(when (boundp 'mac-effective-appearance-change-hook)
  (add-hook 'mac-effective-appearance-change-hook
            #'pk/themes--follow-system))

(modus-themes-load-themes)
(pk/themes--follow-system)

;; custom functions
(defun pk/iterm-cut-base64 (text)
  "Take TEXT and send it to iterm to copy."
  (interactive)
  (let ((base-64 (base64-encode-string text :no-line-break)))
    (send-string-to-terminal (concat "\e]1337;Copy=:" base-64 "\a"))))

(setq interprogram-cut-function 'pk/iterm-cut-base64)


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

(use-package gcmh
  :ensure t
  :diminish
  :custom
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold pk/gc-cons-threshold)
  :config
  (gcmh-mode))


;; Some sane defaults
(setq-default indent-tabs-mode nil
	          tab-width 4
	          fill-column 79
              display-line-numbers-widen t)

(menu-bar-mode -1)

(show-paren-mode)
(delete-selection-mode)
(column-number-mode)
(transient-mark-mode)
(global-display-line-numbers-mode)
(global-hl-line-mode)

(bind-keys
 ("C-`" . kill-this-buffer)
 ("M-g" . goto-line)
 ("C-z" . undo)
 ("C-|" . display-fill-column-indicator-mode)
 ("C-c d" . duplicate-dwim)
 ("<f5>" . modus-themes-toggle))

(use-package completion-preview
  :when (version< "30.1" emacs-version)
  :bind
  (:map completion-preview-active-mode-map
   ("M-n" . #'completion-preview-next-candidate)
   ("M-p" . #'completion-preview-prev-candidate)
   ([remap forward-word] . #'completion-preview-insert-word)
   ([remap forward-sexp] . #'completion-preview-insert-sexp))
  :config
  (global-completion-preview-mode))

(use-package prog-mode
  :defer t
  :hook (prog-mode . electric-pair-mode)
        (prog-mode . display-fill-column-indicator-mode))

(use-package make-mode
  :defer t
  :hook (makefile-mode . indent-tabs-mode))

(use-package window
  :defer t
  :custom
  (split-height-threshold 90)
  (split-width-threshold 160))

(use-package xt-mouse
  :config
  (xterm-mouse-mode))

(use-package dired
  :defer t
  :custom
  (dired-chown-program (or (executable-find "gchown") "chown"))
  (dired-touch-program (or (executable-find "gtouch") "touch"))
  (dired-use-ls-dired 'unspecified)
  (dired-dwim-target t)
  :config
  (setq insert-directory-program (or (executable-find "gls") "ls")))

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package ace-window
  :ensure t
  :defer t
  :diminish
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-translate-char-function #'(lambda (c)
                                  (if (eq ?\M-o c) ?n c)))
  :bind ("M-o" . #'ace-window))

(use-package avy
  :ensure t
  :defer t
  :custom
  (avy-all-windows 'all-frames)
  :bind ("C-c j" . #'avy-goto-word-or-subword-1))

(use-package iedit
  :ensure t
  :defer t
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

(use-package magit
  :ensure t
  :defer t)

(use-package difftastic-bindings
  :ensure difftastic
  :config
  (difftastic-bindings-mode))

(provide 'init)

;;; init.el ends here
