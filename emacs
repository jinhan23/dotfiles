;;; package --- Summary
;;; Commentary:

;;; Code:
;;frame size
;; (add-to-list 'default-frame-alist '(height . 56))
;; (add-to-list 'default-frame-alist '(width . 85))
;; (set-frame-position (selected-frame) 0 0)

;;melpa settings
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize) ;; You might already have this line

;; for use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t) ;;when package install need

(xterm-mouse-mode t)



;;themes
(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))


;; lsp-mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurerec-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :config
  (which-key-mode))

;; clojure
(use-package clojure-mode)
(use-package cider)


;; python
(use-package pyvenv
  :init
  (setenv "WORKON_HOME" (getenv "CONDA_ENVS"))
  (pyvenv-mode 1))

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred



;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          ;; treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;;company
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 1))

;;flycheck
(use-package flycheck
  :config
  (global-flycheck-mode))

;;helm
(use-package helm
  :config
  (helm-mode 1))


;;lisp settings
(use-package paredit
  :config
  (mapc (lambda (mode)
          (let ((hook (intern (concat (symbol-name mode)
                                      "-mode-hook"))))
            (add-hook hook (lambda () (paredit-mode 1)))
            (add-hook hook (lambda () (electric-pair-mode nil)))))
        '(emacs-lisp inferior-lisp slime lisp-interaction scheme racket clojure)))

;;racket
(use-package racket-mode)

;;hylang settings
(use-package hy-mode
  :config
  (add-hook 'hy-mode-hook
            (lambda ()
              (global-set-key (kbd "C-c C-c") 'hy-shell-eval-buffer)
              (global-set-key (kbd "C-c C-r") 'hy-shell-eval-region)
              (global-set-key (kbd "C-x C-e") 'hy-shell-eval-last-sexp)
              (paredit-mode 1)
              (company-mode 1)
              (add-to-list 'company-backends '(company-hy company-dabbrev-code)))))



;;rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


;;isort and black formatting
(use-package py-isort
  :init
  ;; (add-hook 'before-save-hook 'py-isort-before-save)
  ;; (setq py-isort-options '("--lines=100"))
  )

(use-package python-black
  :demand t
  :after python
  :config
  (add-hook 'python-mode-hook (lambda () (python-black-on-save-mode 1))))


(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-M-N") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-M-P") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-M-L") 'mc/mark-all-like-this)
  )

;;UI settings
(add-hook 'prog-mode-hook 'electric-pair-mode) ;;parenthesis completion
(show-paren-mode 1) ;;show paren mode
(setq show-paren-delay 0)
(setq inhibit-startup-screen t) ;;skip startup screen and specify default mode
(setq initial-major-mode 'python-mode)
(setq initial-scratch-message "")
(display-time) ;;show time on status bar
(transient-mark-mode t) ;; show selected area
(global-linum-mode t) ;; show line numbers
(setq column-number-mode t) ;; show column number
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)

;;shortcuts
(global-set-key [C-kanji] 'set-mark-command) ;; for windows

(global-set-key [f5] 'compile)
(global-set-key [f6] 'gdb)

(global-set-key [f3] 'python-indent-shift-left)
(global-set-key [f4] 'python-indent-shift-right)
(global-set-key [f7] 'previous-buffer)
(global-set-key [f8] 'next-buffer)

(global-set-key [f12] 'shell)

;;emacs windows
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono Derivative Powerline" :foundry "unknown" :slant normal :weight normal :height 130 :width normal)))))

;;korean environment
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (vscode-dark-plus-theme conda multiple-cursors python-black py-isort rainbow-delimiters hy-mode racket-mode paredit helm company color-theme-sanityinc-tomorrow use-package))))
