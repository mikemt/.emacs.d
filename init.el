;;; init.el --- an emacs init file
;;;
;;; Author: Michael Morris-Thomas <michael.morris-thomas@uwa.edu.au>
;;; URL: https://github.com/mikemt/.emacs.d
;;; Version: 1.0
;;; Keywords: configuration, emacs, init
;;; License: MIT
;;;
;;; Commentary:
;;; - an evil emacs init file 
;;; 
;;; Compatibility:
;;; - tested on GNU Emacs 29.2
;;;
;;; Usage:
;;; - clone the repository to ~/.emacs.d
;;; - start emacs
;;; - if the modeline doesn't render correctly, run `M-x all-the-icons-install-fonts`
;;;
;;; Code:

(setenv "LANG" "en_US.UTF-8")
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(server-start)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(unless package-archive-contents
  (package-refresh-contents))

(use-package exec-path-from-shell
  :ensure t
  :config
  (pcase system-type
    ('darwin
     (let ((brew-path (if (file-directory-p "/opt/homebrew/bin")
                         "/opt/homebrew/bin"
                       "/usr/local/bin")))
       (add-to-list 'exec-path brew-path)
       (setenv "PATH" (concat brew-path ":" (getenv "PATH")))
       (exec-path-from-shell-copy-envs '("PATH" "MANPATH" "PYTHONPATH"))))
    ('windows-nt
     (add-to-list 'exec-path (expand-file-name "~/scoop/shims")))))

(defun format-elisp-on-save ()
  (when (eq major-mode 'emacs-lisp-mode)
  (indent-region (point-min) (point-max))))

(defun _move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun _move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun _indent-to-next-tab-stop ()
  "Indent the current line or region to the next tab stop."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char start)
          (while (< (point) end)
            (indent-to-next-tab-stop-on-line)
            (forward-line 1))))
    (indent-to-next-tab-stop-on-line)))

(defun _indent-to-next-tab-stop-on-line ()
  "Indent the current line to the next tab stop."
  (let ((next-tab-stop (+ (current-indentation) (- tab-width (mod (current-indentation) tab-width)))))
    (save-excursion
      (move-beginning-of-line nil)
      (delete-horizontal-space)
      (insert (make-string next-tab-stop ?\s)))))

(defun _current-indentation ()
  "Return the current line's indentation."
  (save-excursion
    (move-beginning-of-line nil)
    (skip-chars-forward " \t")
    (current-column)))

(defun _find-file-emacs-init ()
  "Open the init.el file."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.el")))

(defun _find-file-vscode-settings ()
  "Opens vscode settings.json"
  (interactive)
  (find-file (concat (getenv "APPDATA") "/Code/User/settings.json")))

(defun _find-file-vscode-keybindings ()
  "Opens vscode keybindings.json."
  (interactive)
  (find-file (concat (getenv "APPDATA") "/Code/User/keybindings.json")))

(defun _find-file-pwsh-profile ()
  "Opens powershell.ps1"
  (interactive)
  (find-file
   (concat
    (getenv "OneDrive")
    "/Documents/PowerShell/Microsoft.PowerShell_profile.ps1")))

(defun _copilot-turn-on-unless-buffer-read-only ()
  "Turn on `copilot-mode' if the buffer is writable."
  (unless (or buffer-read-only (not (buffer-file-name (current-buffer))))
    (copilot-mode 1)))

(use-package general
  :ensure t
  :config
  (general-define-key
   "C-c e"      '_find-file-emacs-init
   "M-<up>"     'windmove-up
   "M-<down>"   'windmove-down
   "M-<left>"   'windmove-left
   "M-<right>"  'windmove-right
   "C-<down>"   '_move-line-down
   "C-<up>"     '_move-line-up
   "M-i"        '_indent-to-next-tab-stop))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package recentf
  :ensure nil 
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 100) 
  (recentf-auto-cleanup 'never) 
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory)) 
  :config
  (setq recentf-exclude '("\\.git.*" "node_modules" "/tmp/"))
  (run-at-time nil (* 5 60) 'recentf-save-list)
  :bind
  (("C-x C-r" . consult-recent-file)))

(use-package bookmark
  :ensure nil
  :init
  (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)
        bookmark-save-flag 1)
  :config
  (progn
    (add-hook 'kill-emacs-hook #'bookmark-save)
    (bookmark-load bookmark-default-file t t)))

(use-package format-all
  :ensure t
  :commands format-all-mode
  :hook
  (prog-mode . format-all-mode)
  (emacs-lisp-mode . (lambda () (add-hook 'before-save-hook 'format-all-buffer nil 'local)))
  :config
  (setq-default format-all-formatters
                '(("Python" ruff "--format"))))

(use-package ispell
  :init
  (setq ispell-program-name
        (if (eq system-type 'windows-nt)
            "C:/msys64/usr/bin/aspell"
          "aspell")
        ispell-extra-args '("--lang=en_US" "--encoding=utf-8")))

(use-package which-key
  :ensure t
  :init
  (setq which-key-idle-delay 0.1
        which-key-min-display-lines 9) 
  :config
  (which-key-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :ensure t
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-responsive 'top
        highlight-indent-guides-auto-character-face-perc '30))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1)
  (setq vertico-count 15
        vertico-scroll-margin 0
        vertico-resize nil
        vertico-cycle t))

(use-package consult
  :ensure t
  :after vertico
  :bind
  ("C-c b" . consult-buffer)
  ("C-c s" . consult-isearch-forward)
  ("C-c r" . consult-ripgrep)
  ("C-c l" . consult-line)
  ("C-c f" . consult-find)
  ("C-c F" . consult-flyspell)
  :config
  (setq consult-preview-buffer-height 15))

(use-package marginalia
  :ensure t
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  :config
  (setq
   corfu-auto t
   corfu-min-width 20
   corfu-quit-no-match 'separator
   corfu-auto-delay 0
   corfu-auto-prefix 1)
  (add-to-list
   'corfu-margin-formatters #'nerd-icons-corfu-formatter))
 
(use-package nerd-icons-corfu
  :ensure t)

(use-package eglot
  :ensure t
  :hook
  (python-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  :config
  (setq eglot-connect-timeout nil)
  (setq eglot-autoshutdown t)
  (setq eglot-sync-connect nil)
  (setq eglot-ignored-server-capabilities 
    '(:hoverProvider :documentLinkProvider :inlayHintProvider :documentOnTypeFormattingProvider))
  (add-to-list 'eglot-server-programs '(rust-mode .
    ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  (let ((pyright-path (cond
    ((eq system-type 'windows-nt)
      (concat (getenv "USERPROFILE") "\\.local\\bin\\pyright-langserver"))
    ((eq system-type 'darwin)
      (expand-file-name "~/.local/bin/pyright-langserver"))
                      (t "pyright-langserver"))))
    (add-to-list 'eglot-server-programs
      `(python-mode . (,pyright-path "--stdio")))))

;;(use-package eglot-booster-mode
;;  :after eglot
;;  :config (eglot-booster-mode))

(use-package tree-sitter
  :ensure t
  :hook
  ((prog-mode powershell-mode yaml-mode) .
   (lambda () (tree-sitter-mode) (tree-sitter-hl-mode))))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package flyspell
  :ensure t
  :bind
  :hook
  ((text-mode . flyspell-mode) 
   (prog-mode . flyspell-prog-mode)) 
  :config
  (dolist (face '(font-lock-string-face
                  font-lock-comment-face
                  font-lock-doc-face
                  tree-sitter-hl-face:comment
                  tree-sitter-hl-face:string
                  tree-sitter-hl-face:doc))
    (add-to-list 'flyspell-prog-text-faces face)))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-," . flyspell-correct-wrapper))
  :config
  (setq flyspell-correct-interface #'flyspell-correct-completing-read))

(use-package yasnippet
  :ensure t
  :hook
  ((prog-mode . yas-minor-mode)
   (org-mode . yas-minor-mode))
  :config
  (yas-reload-all)
  :bind
  (:map yas-minor-mode-map
        ("TAB" . yas-expand) 
        ("C-c y" . yas-insert-snippet) 
        ("C-c n" . yas-new-snippet)))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package python-mode
  :ensure t
  :bind
  (:map python-mode-map
        ("C-c C-d" . eldoc-documentation))

  :hook
  (python-mode . (lambda ()
                   (flycheck-select-checker 'python-ruff)
                   ;;(setq-local eldoc-documentation-function #'ignore)
                   (setq-local tab-width 4)))
  (python-mode . (lambda ()
                   (add-function :before-until (local 'tree-sitter-hl-face-mapping-function)
                                 (lambda (capture-name)
                                   (pcase capture-name
                                     ("doc" 'font-lock-comment-face))))))
  :config
  (setq python-shell-interpreter "ipython.exe"))

(use-package python-isort
  :ensure t
  :after python-mode
  :hook (python-mode . python-isort-on-save-mode))

(use-package ruff-format
  :ensure t
  :after python-mode
  :config
  (setq ruff-format-command "ruff format")
  :hook
  (python-mode . ruff-format-on-save-mode))

(use-package powershell
  :ensure t
  :mode
  ("\\.ps1\\'" . powershell-mode)
  :config
  (setq powershell-indent 2))

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/.minconda"))
  :hook
  (python-mode .
               (lambda ()
                 (setq conda-env-default-env "py312_dev")
                 (conda-env-activate conda-env-default-env)))
  :config
  (setq conda-env-initialize-interactive-shells t
        conda-env-initialize-eshell t))

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "pandoc"))

(use-package rst
  :ensure t
  :mode
  ("\\.rst\\'\\|\\.rest\\'". rst-mode))

(use-package yaml-mode
  :ensure t
  :mode
  ("\\.yml\\'\\|\\.yaml\\'\\|/\\.condarc\\'" . yaml-mode))

(use-package org
  :ensure t
  ;;  :hook
  ;;  (org-mode . yas-minor-mode)
  :config
  (setq org-image-actual-width 400)
  (setq org-highlight-latex-and-related '(latex))
  (setq org-latex-default-packages-alist nil)
  (setq org-cite-export-processors
        '((latex biblatex)
          (html  citeproc)
          (t     basic)))
  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (jupyter . t))))


(use-package citar
  :ensure t
  :custom
  (citar-bibliography
   (append
    (list
     (cond
      ((eq system-type 'windows-nt)
       (concat (getenv "OneDrive") "/notes/papers/bibliography.bib"))
      ((eq system-type 'darwin)
       (expand-file-name "~/Library/CloudStorage/OneDrive-Personal/notes/papers/bibliography.bib"))))
    (directory-files default-directory t "\\.bib$")))
 :hook
  ((org-mode . (lambda ()
                 (evil-define-key 'normal org-mode-map
                   (kbd "<leader> t c") 'citar-insert-citation
                   (kbd "<leader> t b") 'citar-insert-bibtex
                   (kbd "<leader> t r") 'citar-refresh
                   (kbd "<leader> t s") 'citar-open)))
   (TeX-mode . (lambda ()
                 (evil-define-key 'normal TeX-mode-map
                   (kbd "<leader> t c") 'citar-insert-citation
                   (kbd "<leader> t b") 'citar-insert-bibtex
                   (kbd "<leader> t r") 'citar-refresh
                   (kbd "<leader> t s") 'citar-open)))
   (bibtex-mode . (lambda ()
                    (evil-define-key 'normal bibtex-mode-map
                      (kbd "<leader> t c") 'citar-insert-citation
                      (kbd "<leader> t b") 'citar-insert-bibtex
                      (kbd "<leader> t r") 'citar-refresh
                      (kbd "<leader> t s") 'citar-open)))))

(use-package org-download
  :ensure t
  :after org
  :config
  (setq org-download-image-dir "./images")
  :bind
  ("C-M-y" . org-download-clipboard))

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode))

(use-package latex
  :after tex-site
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex))

(use-package cdlatex
  :ensure t
  :hook
  (org-mode . turn-on-org-cdlatex)
  (LaTeX-mode . turn-on-cdlatex))


;;(use-package cape
;; :ensure t
;; :bind
;; (("M-p p" . completion-at-point)
;;  ("M-p d" . cape-dabbrev)
;;  ("M-p k" . cape-keyword)
;;  ("M-p \\". cape-tex)
;;  ("M-p l" . cape-line)
;;  ("M-p f" . cape-file)
;;  ("M-p e" . cape-elisp-block)
;;  ("M-p h" . cape-history)
;;  ("M-p w" . cape-dict)
;; :init
;; (dolist (func '(cape-dabbrev cape-file cape-elisp-block
;;                              cape-history cape-keyword cape-dict))
;;   (add-to-list 'completion-at-point-functions func))))

(add-to-list 'load-path "~/.emacs.d/copilot.el")
(use-package copilot
  :ensure nil
  :bind
  (:map copilot-mode-map
        ("S-<return>" . copilot-accept-completion))
  :hook
  ((prog-mode markdown-mode) . copilot-mode)
  (prog-mode . copilot-turn-on-unless-buffer-read-only)
  :config
  (setq copilot-indent-offset-warning-disable t)
  (setq copilot-idle-delay 0.12)
  (setq copilot-max-char -1)
  (setq copilot-log-max 10000))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  (evil-set-leader 'motion (kbd "SPC"))

  (evil-define-key 'normal 'global
    (kbd "<leader>fe") #'eval-buffer
    (kbd "<leader>fs") #'save-buffer
    (kbd "<leader>ff") #'find-file
    (kbd "<leader>fl") #'load-file
    (kbd "<leader>fi") #'_find-file-emacs-init
    (kbd "<leader>fp") #'package-install
    (kbd "<leader>fP") #'list-packages
    (kbd "<leader>fr") #'revert-buffer)

  (evil-define-key 'normal 'global
    (kbd "<leader>s")  #'isearch-forward
    (kbd "<leader>S")  #'isearch-backward
    (kbd "<leader>l") #'consult-line
    (kbd "<leader>cb") #'consult-buffer
    (kbd "<leader>cr") #'consult-ripgrep
    (kbd "<f13> l") #'consult-line)

  (evil-define-key 'normal 'global
    (kbd "TAB") #'indent-for-tab-command
    (kbd "C-a") #'beginning-of-line
    (kbd "C-e") #'end-of-line)

  (evil-define-key 'insert 'evil-insert-state-map
    (kbd "C-c") #'evil-normal-state
    (kbd "C-a") #'beginning-of-line
    (kbd "C-e") #'end-of-line))


(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  (with-eval-after-load 'package
    (evil-define-key 'normal package-menu-mode-map
      (kbd "f n") 'package-menu-filter-by-name
      (kbd "f c") 'package-menu-clear-filter
      (kbd "f m") 'package-menu-filter-marked
      (kbd "f u") 'package-menu-filter-upgradable
      (kbd "f i") 'package-menu-filter-description
      (kbd "f s") 'package-menu-filter-status)))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (push '(?< . ("<" . ">")) evil-surround-pairs-alist))

(use-package all-the-icons
  :if (display-graphic-p))


(use-package dired
  :config
  (use-package treemacs-icons-dired
    :ensure t
    :if (display-graphic-p)
    :config (treemacs-icons-dired-mode)))

;;(use-package all-the-icons-dired
;;  :ensure t
;;  :hook (dired-mode . all-the-icons-dired-mode)
;;  :config (setq all-the-icons-dired-monochrome nil))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
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
  (setq doom-gruvbox-dark-variant "hard"))

(use-package emacs
  :config
  (global-display-line-numbers-mode t)
  (column-number-mode 1)
  (global-auto-revert-mode 1)
  (electric-pair-mode 1)
  (global-set-key [remap dabbrev-expand] 'hippie-expand)
  (setq
   inhibit-splash-screen t
   backup-inhibited t
   auto-save-default nil
   custom-file "~/.emacs.d/custom.el"
   ring-bell-function 'ignore
   explicit-shell-file-name (if (eq system-type 'windows-nt) "pwsh.exe" nil)
   tab-always-indent t
   kill-emacs-query-functions nil
   mac-command-modifier (if (eq system-type 'darwin) 'meta 'none)
   mac-option-modifier (if (eq system-type 'darwin) 'super 'none)
   mac-control-modifier 'control
   mac-function-modifier (if (eq system-type 'darwin) 'hyper 'none)))

  (setq-default
   indent-tabs-mode nil
   display-line-numbers-width 3)
  (setq inhibit-warning-function
        (lambda (type message)
          (or (and (eq type 'deprecation)
                   (string-match-p "events-buffer-scrollback-size" message))
              (and (eq type 'deprecation)
                   (string-match-p "events-buffer-config" message)))))

(defun initialize-frame (frame)
  (if (display-graphic-p frame)
      (progn
        (tool-bar-mode 0)
        (scroll-bar-mode -1)
        (menu-bar-mode -1)
        (set-frame-font "CaskaydiaCove Nerd Font Mono:size=12" nil t)
        ;;(set-frame-font "CaskaydiaCove NFM-10" nil t)
        (set-frame-width frame 138)
        (set-frame-height frame 75)
        (let ((screen-width (display-pixel-width))
              (screen-height (display-pixel-height))
              (frame-width (* (frame-char-width) 138))
              (frame-height (* (frame-char-height) 75)))
          ;; Center the frame
          (set-frame-position frame
                              (/ (- screen-width frame-width) 2)
                              (/ (- screen-height frame-height) 2)))
        (set-frame-parameter frame 'internal-border-width 2))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'initialize-frame)
  (initialize-frame (selected-frame)))
(put 'downcase-region 'disabled nil)
