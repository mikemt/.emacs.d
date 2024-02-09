(require 'package)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)

(setq backup-inhibited t)
(setq ring-bell-function 'ignore)

(column-number-mode 1)
(global-display-line-numbers-mode t)

(setq-default indent-tabs-mode nil)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  (setq doom-modeline-height 22)
  (setq doom-modeline-column-zero-based nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-color-icon nil))

(use-package format-all
  :ensure t
  :hook (before-save . format-all-buffer))

(defun format-elisp-on-save ()
  (when (eq major-mode 'emacs-lisp-mode)
    (indent-region (point-min) (point-max))))
(add-hook 'before-save-hook 'format-elisp-on-save)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t)
  (doom-themes-org-config))

(use-package rust-mode
  :ensure t
  :config
  (rust-mode 1)
  (setq rust-format-on-save t))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper-isearch-backward))
  :config
  (setq swiper-action-recenter t)
  (setq swiper-include-line-number-in-search t))

(defun initialize-frame (frame)
  (if (display-graphic-p frame)
      (progn
        (tool-bar-mode 0)
        (scroll-bar-mode -1)
        (menu-bar-mode -1)
        (set-frame-font "CaskaydiaCove NFM-11" nil t)
        (set-frame-width frame 138)
        (set-frame-height frame 75)
        (set-frame-position frame 1 5)
        (setq display-line-numbers-width 3)
        (set-frame-parameter frame 'internal-border-width 2))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'initialize-frame)
  (initialize-frame (selected-frame)))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; keybinds
(global-set-key [(meta down)]  'move-line-down)
(global-set-key [(meta up)]  'move-line-up)

(setq custom-file
      (cond
       ((eq system-type 'darwin) "/dev/null") 
       ((eq system-type 'gnu/linux) "/dev/null")
       ((eq system-type 'windows-nt) "nul") 
       (t "/dev/null")))


