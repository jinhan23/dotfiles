;;; package --- Summary
;;; Commentary:

;;; Code:
;;frame size
(add-to-list 'default-frame-alist '(height . 56))
(add-to-list 'default-frame-alist '(width . 85))
(set-frame-position (selected-frame) 0 0)

;;melpa settings
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (when (< emacs-major-version 24)
;;   ;; For important compatibility libraries like cl-lib
;;   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; for use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t) ;;when package install need

(xterm-mouse-mode t)

;;themes
(use-package color-theme-sanityinc-tomorrow
  :config
  (let* ((night-color (assoc 'night color-theme-sanityinc-tomorrow-colors))
         (selection-color (assoc 'selection night-color)))
    (setf (cdr selection-color) "#3a3a3a"))
  (load-theme 'sanityinc-tomorrow-night t))
;; (use-package seoul256-theme
;;   :config
;;   (load-theme 'seoul256 t))


;;company
(use-package company-irony)
(use-package company-c-headers)
(use-package company-jedi)
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "C-M-i") #'company-complete)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.01)
  (setq company-minimum-prefix-length 1)
  ;; (defun company-yasnippet-or-completion ()
  ;;   "Solve company yasnippet conflicts."
  ;;   (interactive)
  ;;   (let ((yas-fallback-behavior
  ;;          (apply 'company-complete-common nil)))
  ;;     (yas-expand)))
  ;; (with-eval-after-load 'company
  ;;   (dolist (key '("<return>" "RET"))
  ;;     (define-key company-active-map (kbd key)
  ;;       `(menu-item nil company-complete
  ;;                   :filter ,(lambda (cmd)
  ;;                              (when (company-explicit-action-p)
  ;;                                cmd)))))
  ;;   (define-key company-active-map (kbd "TAB") 'company-yasnippet-or-completion)
  ;;   (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  ;;   (define-key company-active-map (kbd "S-TAB") 'yas-expand)
  ;;   (define-key company-active-map (kbd "S-<tab>") 'yas-expand))
  )

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
        '(emacs-lisp inferior-lisp slime lisp-interaction scheme racket)))

;;racket
(use-package racket-mode)

;;hy settings
(use-package hy-mode
  :config
  (add-hook 'hy-mode-hook
            (lambda ()
              (global-set-key (kbd "C-c C-c") 'hy-shell-eval-buffer)
              (paredit-mode 1)
              (company-mode 1)
              (add-to-list 'company-backends '(company-hy :with company-dabbrev-code)))))



;;rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;python
  (add-hook 'inferior-python-mode-hook (lambda () (company-mode -1)))
  (defun company-jedi-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'company-jedi-hook)


;;elpy
(use-package elpy
  :init
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i")
  (add-hook 'python-mode-hook
            (lambda ()
              (setq gud-pdb-command-name "python -m pdb")
              (global-set-key [f6] 'pdb)))
  :config
  (elpy-enable)
  (defun ipy-clear ()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-backend "jedi")
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'prog-mode-hook 'highlight-indentation-mode)
  (add-hook 'prog-mode-hook 'highlight-indentation-current-column-mode)
  (set-face-background 'highlight-indentation-face "#262626")
  (set-face-background 'highlight-indentation-current-column-face "#444444")
  )

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

(use-package cl-lib
  :config
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (cl-letf (((symbol-function #'process-list) (lambda ())))
      ad-do-it)))

(use-package irony-eldoc)
(use-package irony
  :init
  (add-hook 'c-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s %s -o %s.out -g"
                             "gcc"
                             file
                             (file-name-sans-extension file))))))
  (add-hook 'c++-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s %s -o %s.out -g"
                             "g++"
                             file
                             (file-name-sans-extension file))))))
  :config
  (yas-global-mode 1)
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
  (mapc (lambda (mode)
          (let ((hook (intern (concat (symbol-name mode)
                                      "-mode-hook"))))
            ;;(add-hook hook (lambda () (autopair-mode 1)))
            (add-hook hook (lambda () (irony-mode 1) (irony-eldoc 1)))
            (add-hook hook (lambda () (add-to-list 'company-backends 'company-irony)))
            (add-hook hook (lambda () (global-set-key [f6] 'gdb)))))
        '(c c++))
  ;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

(use-package flycheck-pos-tip)
(use-package flycheck-popup-tip)
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (with-eval-after-load 'flycheck
    (if (display-graphic-p)
        (flycheck-pos-tip-mode)
      (flycheck-popup-tip-mode))))
(use-package flycheck-pyflakes)
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-M-N") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-M-P") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-M-L") 'mc/mark-all-like-this)
  )

;; (use-package smartparens
;;   :config
;;   (add-hook 'prog-mode-hook 'smartparens-mode))

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
(global-set-key [C-kanji] 'set-mark-command) ;;for windows

(global-set-key [f5] 'compile)
(global-set-key [f6] 'gdb)

(global-set-key [f3] 'python-indent-shift-left)
(global-set-key [f4] 'python-indent-shift-right)
;;(global-set-key [f7] 'previous-buffer)
;;(global-set-key [f8] 'next-buffer)

(global-set-key [f12] 'shell)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;emacs windows
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 130 :width normal)))))

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
    (helm python-black autopair company-jedi zenburn-theme use-package rainbow-delimiters python-environment paredit multiple-cursors monokai-theme moe-theme irony-eldoc flycheck-pyflakes epc elpy company-irony company-c-headers color-theme-sanityinc-tomorrow))))
