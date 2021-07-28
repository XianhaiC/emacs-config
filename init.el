;; NOTE:
;; run the following to update packages list in case a package cant be found
;; M-x list-packages


;; initializations
;;;;;;;
;;;;;;;

;; You will most likely need to adjust this font size for your system!
(defvar rxpii/default-font-size 110)
(defvar rxpii/default-variable-font-size 110)

;; import customize config from a seperate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; generic emacs config
;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; use the left super key instead of left alt
(setq x-super-keysym 'meta)

;; minimalism
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
;;(setq visible-bell t)

;; fonts
;(set-face-attribute 'default nil :font "Input" :height 110)
(set-face-attribute 'default nil :font "Fira Code Retina" :height rxpii/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height rxpii/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height rxpii/default-variable-font-size :weight 'regular)

;; line numbers (col + row)
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

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
  :after (evil magit)
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
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

;; ergonomics
(use-package ace-jump-mode)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; org mode!
(defun rxpii/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun rxpii/org-font-setup ()
  ;; make bullets hyphens
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; use different font sizes for different header levels
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell"  :weight 'bold :height (cdr face)))

  ;; ensure that anything that should be fixed-pitch in org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . rxpii/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)
  (rxpii/org-font-setup))

(use-package org-superstar
  :after org
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :custom (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun rxpii/org-mode-visual-fill ()
  (setq visual-fill-column-width 80
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . rxpii/org-mode-visual-fill))

(use-package oj
  :ensure t
  :custom ((oj-default-online-judge 'codeforces)
	   (oj-home-dir "~/projects/cc/")))

(use-package perspective
  :config
  (persp-mode))

(use-package elfeed
  :config
  (setq elfeed-feeds
	'("https://morepablo.com/feeds/all.rss.xml"
	  "http://planet.lisp.org/rss20.xml"
	  "https://stevelosh.com/rss.xml"
	  "https://pvk.ca/atom.xml"
	  "https://geohot.github.io/blog/feed.xml"))
  (setq shr-inhibit-images t))

;; keymappings
;;;;;;;;;;;;;;

;; globals
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

   "p" '(:ignore t :which-key "projects")
   "pp" '(projectile-switch-project :which-key "switch projects")

   ; speed movement
   "j" '(:ignore t :which-key "ergonomics")
   "jj" '(evil-ace-jump-line-mode :which-key "jump to line")
   "jk" '(evil-ace-jump-char-mode :which-key "jump to char")
   "jl" '(evil-ace-jump-word-mode :which-key "jump to word")

   ; windows
   "w" '(:ignore t :which-key "windows")
   "ws" '(evil-window-split :which-key "window split")
   "wv" '(evil-window-vsplit :which-key "window vsplit")

   ; generic mappings
   ":" '(counsel-M-x :which-key "bring up M-x")
   "." '(find-file :which-key "find file")
   "SPC" '(projectile-find-file :which-key "find project file")
   "TAB" '(persp-switch-by-number :which-key "switch to workspace number"))

;; prettifiers
;;;;;;;;;;;;;;

(use-package doom-themes
  :config (load-theme 'doom-horizon t))


;; language support
;;;;;;;;;;;;;;;;;;;

;; lsp - the ide of the gods
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (add-to-list 'exec-path "~/.emacs.d/packages/elixir-ls")
  :hook ((python-mode . lsp)
	 (elixir-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; company completion framework
(use-package company
  :hook ((after-init . global-company-mode)
	 (after-init . company-tng-mode))) ; tab completes rather than cycles

;; ocaml
(use-package tuareg)

(use-package merlin
  :after tuareg
  :hook (tuareg-mode . merlin-mode))

;; haskell
(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :init
  ;(add-hook 'haskell-mode-hook 'flycheck-mode)
  ;; OR for flymake support:
  ;(add-hook 'haskell-mode-hook 'flymake-mode)
  ;(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

  (add-hook 'haskell-mode-hook 'dante-mode)
  )

(use-package elixir-mode
  :init
  (add-hook 'elixir-mode-hook
            (lambda ()
              (push '(">=" . ?\u2265) prettify-symbols-alist)
              (push '("<=" . ?\u2264) prettify-symbols-alist)
              (push '("!=" . ?\u2260) prettify-symbols-alist)
              (push '("==" . ?\u2A75) prettify-symbols-alist)
              (push '("=~" . ?\u2245) prettify-symbols-alist)
              (push '("<-" . ?\u2190) prettify-symbols-alist)
              (push '("->" . ?\u2192) prettify-symbols-alist)
              (push '("<-" . ?\u2190) prettify-symbols-alist)
              (push '("|>" . ?\u25B7) prettify-symbols-alist))))

(use-package inf-elixir
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . 'inf-elixir-send-line)
         ("C-c i r" . 'inf-elixir-send-region)
         ("C-c i b" . 'inf-elixir-send-buffer)))

(use-package reformatter
  :ensure t
  :config
  ; Adds a reformatter configuration called "+elixir-format"
  ; This uses "mix format -"
  (reformatter-define +elixir-format
    :program "mix"
    :args '("format" "-"))
  ; defines a function that looks for the .formatter.exs file used by mix format
  (defun +set-default-directory-to-mix-project-root (original-fun &rest args)
    (if-let* ((mix-project-root (and buffer-file-name
                                     (locate-dominating-file buffer-file-name
                                                             ".formatter.exs"))))
        (let ((default-directory mix-project-root))
          (apply original-fun args))
      (apply original-fun args)))
  ; adds an advice to the generated function +elxir-format-region that sets the proper root dir
  ; mix format needs to be run from the root directory otherwise it wont use the formatter configuration
  (advice-add '+elixir-format-region :around #'+set-default-directory-to-mix-project-root)
  ; Adds a hook to the major-mode that will add the generated function +elixir-format-on-save-mode
  ; So, every time we save an elixir file it will try to find a .formatter.exs and then run mix format from
  ; that file's directory
  (add-hook 'elixir-mode-hook #'+elixir-format-on-save-mode))


;; custom commands
;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;

;(defun open-daily-tasks () (interactive) (
(put 'erase-buffer 'disabled nil)
