;; NOTE:
;; run the following to update packages list in case a package cant be found
;; M-x list-packages

;; import customize config from a seperate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; generic emacs config
;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; minimalism
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
;;(setq visible-bell t)

;; fonts
(set-face-attribute 'default nil :font "Input" :height 110)

;; line numbers (col + row)
(column-number-mode)
(global-display-line-numbers-mode t)

;; disable line numbers in certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; external package setup
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; package setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; enable use-package
(require 'use-package)
(setq use-package-always-ensure t)


;; custom packages
;;;;;;;;;;;;;;;;;;

;; counsel + ivy for generic code completion
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; get rid of annoying ^ prompt
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; undo redo goodness
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; evil mode
(defun rxpii/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  erc-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))
		  
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
;; NOTE: not using this for now as it seems to break evil
;  :hook (evil-mode . rxpii/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  :custom ((evil-undo-system 'undo-tree)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; NOTE: run the following for after loading the configuration for the
;; first time
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; delimiters for parens
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which key, to help with memory loss
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))


;; keymappings
;;;;;;;;;;;;;;

;; spacemacs
(use-package general
  :config
  (general-create-definer rxpii/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

;; define keymap modes for repetitive commands
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "quit" :exit t))

(defhydra hydra-resize-win (:timeout 4)
  "resize window"
  ("h" evil-window-decrease-width)
  ("j" evil-window-decrease-height)
  ("k" evil-window-increase-height)
  ("l" evil-window-increase-width)
  ("q" nil "quit" :exit t))

(defhydra hydra-switch-win (:timeout 4)
  "switch window"
  ("h" evil-window-left)
  ("j" evil-window-down)
  ("k" evil-window-up)
  ("l" evil-window-right)
  ("q" nil "quit" :exit t))

(rxpii/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")
   "ts" '(hydra-text-scale/body :which-key "scale text")
   "tr" '(hydra-resize-win/body :which-key "resize window")
   "tw" '(hydra-switch-win/body :which-key "switch window")
   ":" '(counsel-M-x :which-key "bring up M-x"))

;; make esc quit prompts
;; TODO get rid of these
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-b") 'counsel-switch-buffer)


;; prettifiers
;;;;;;;;;;;;;;

(use-package doom-themes
  :config (load-theme 'doom-laserwave t))
