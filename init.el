(require 'package)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)

(column-number-mode 1)
(global-display-line-numbers-mode t)
(electric-pair-mode 1)

(setq
 inhibit-splash-screen t
 backup-inhibited t
 ring-bell-function 'ignore
 custom-file "~/.emacs.d/custom.el"
 ispell-program-name "aspell"
 ispell-dictionary "british")

(setq-default
 indent-tabs-mode nil
 display-line-numbers-width 3)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-set-leader 'motion (kbd "SPC"))
  (evil-define-key 'normal 'global
    (kbd "<leader>fs") #'save-buffer
    (kbd "<leader>ff") #'find-file
    (kbd "<leader>fl") #'load-file
    (kbd "<leader>cl") #'consult-line
    (kbd "<leader>cr") #'consult-ripgrep))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  (setq
   doom-modeline-height 22
   doom-modeline-column-zero-based nil
   doom-modeline-icon t
   doom-modeline-major-mode-color-icon nil))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-gruvbox t)
  (doom-themes-org-config)
  (setq doom-grubox-dark-variant "hard"))

(use-package format-all
  :ensure t
  :hook (before-save . format-all-buffer))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :init
  (setq
   completion-styles '(orderless)
   completion-category-overrides '((file (styles . (partial-completion))))))

(use-package company
  :ensure t
  :bind ("C-." . company-complete)
  :config
  (global-company-mode 1))

(use-package powershell
  :ensure t
  :mode ("\\.ps1\\'" . powershell-mode)
  :config
  (setq powershell-indent 2))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.1))

(use-package vertico
  :init
  (vertico-mode 1)
  :config
  (setq
   vertico-count 15
   vertico-scroll-margin 0
   vertico-resize nil
   vertico-cycle t))

(use-package consult
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package savehist
  :init
  (savehist-mode))

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
        (set-frame-parameter frame 'internal-border-width 2))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'initialize-frame)
  (initialize-frame (selected-frame)))

(defun format-elisp-on-save ()
  (when (eq major-mode 'emacs-lisp-mode)
    (indent-region (point-min) (point-max))))
(add-hook 'before-save-hook 'format-elisp-on-save)

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

(use-package general
  :ensure t
  :config
  (general-define-key
   "M-<down>" 'move-line-down
   "M-<up>" 'move-line-up))





