(require 'package)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)

(defun format-elisp-on-save ()
  (when (eq major-mode 'emacs-lisp-mode)
    (indent-region (point-min) (point-max))))

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

(defun find-file-emacs-init ()
  "Open the init.el file."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

(defun find-file-vscode-settings ()
  "Opens vscode settings.json"
  (interactive)
  (find-file (concat (getenv "APPDATA") "/Code/User/settings.json")))

(defun find-file-vscode-keybindings ()
  "Opens vscode keybindings.json."
  (interactive)
  (find-file (concat (getenv "APPDATA") "/Code/User/keybindings.json")))


(use-package emacs
  :config
  (global-display-line-numbers-mode t)
  (column-number-mode 1)
  (global-auto-revert-mode 1)
  (electric-pair-mode 1)

  (setq
   inhibit-splash-screen t
   backup-inhibited t
   auto-save-default nil
   custom-file "~/.emacs.d/custom.el"
   ring-bell-function 'ignore
   explicit-shell-file-name "pwsh.exe")

  (setq-default
   indent-tabs-mode nil
   display-line-numbers-width 3))


(use-package ispell
  :init
  (setq
   ispell-program-name "aspell"
   ispell-dictionary "british"))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-set-leader 'motion (kbd "SPC")) 
  (evil-define-key 'normal 'global
    (kbd "<leader>fe") #'eval-buffer     
    (kbd "<leader>fs") #'save-buffer     
    (kbd "<leader>ff") #'find-file
    (kbd "<leader>fl") #'load-file
    (kbd "<leader>fi") #'find-file-emacs-init
    (kbd "<leader>fp") #'package-install     
    (kbd "<leader>fr") #'revert-buffer
    (kbd "<leader>s")  #'isearch-forward
    (kbd "<leader>S")  #'isearch-backward
    (kbd "<leader>cl") #'consult-line
    (kbd "<leader>cr") #'consult-ripgrep))

(use-package format-all
  :ensure t
  :hook (before-save . format-all-buffer))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  (marginalia-mode))

(use-package corfu
  :ensure t
  :bind ("C-." . corfu-complete)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (setq
   corfu-auto t
   corfu-min-width 1
   corfu-quit-no-match 'separator
   corfu-auto-delay 0
   corfu-auto-prefix 1)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


(use-package nerd-icons-corfu
  :ensure t)

(use-package orderless
  :ensure t
  :init
  (setq
   completion-styles '(orderless partial-completion basic)
   completion-category-defaults nil
   completion-category-overrides '((file (styles . (partial-completion))))))

(use-package powershell
  :ensure t
  :mode ("\\.ps1\\'" . powershell-mode)
  :config
  (setq powershell-indent 2))

(use-package eglot
  :ensure t
  :hook ((rust-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :config
  (setq eglot-connect-timeout 5000)
  (add-to-list 'eglot-server-programs
               '(rust-mode .
                           ("rust-analyzer"
                            :initializationOptions (:check (:command "clippy"))))))

(use-package cape
  :bind (("C-c p p" . completion-at-point) 
         ("C-c p t" . complete-tag)        
         ("C-c p d" . cape-dabbrev)        
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  )

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
  :ensure t
  :config
  (setq consult-preview-buffer-height 15))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package python-mode
  :ensure t
  :config
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "ipython.exe")
  (set-face-attribute 'font-lock-doc-face nil :foreground "#7c6f64"))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/.minconda"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(use-package copilot
  :ensure nil 
  :hook ((prog-mode markdown-mode) . copilot-mode)
  :config
  (setq copilot-indent-offset-warning-disable t))

(use-package general
  :ensure t
  :config
  (general-define-key
   "C-e"        'end-of-line
   "C-c e"      'find-file-emacs-init
   "M-<up>"     'windmove-up
   "M-<down>"   'windmove-down
   "M-<left>"   'windmove-left
   "M-<right>"  'windmove-right
   "C-<down>"   'move-line-down
   "C-<up>"     'move-line-up
   "C-<return>" 'copilot-accept-completion
   "C-<tab>"    'copilot-accept-completion))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
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
  (setq doom-gruvbox-dark-variant "soft"))


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

(add-hook 'before-save-hook 'format-elisp-on-save)




