;; Package management and configuring.


;; Required package
(setq package-list '(use-package))
(setq package-check-signature nil)


;; Installing packages repos. 
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; Fetching and installing use-package if it's not
(dolist (package package-list)
(unless (package-installed-p package)
	(package-refresh-contents)
	(package-install package)))
(package-initialize)

;; Enabling use package
(require 'use-package)

;; Enabling auto-install for all packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;; GPG keys updater	
(use-package gnu-elpa-keyring-update)
(setq package-check-signature 'allow-unsigned)


;; Auto package update
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  ;; (auto-package-update-maybe) ;; Commented for now cuz annoying. Call 'auto-package-update-now' from M-x menu to manual update
  )



;; Advanced bracers matching
(use-package mic-paren
	:config
	(paren-activate))
	


;; Nerd icons and fonts, for sassy apearence
(use-package nerd-icons
	:config
    (Advice-add 'nerd-icons-install-fonts :around #'my/disable-yornp)
	(unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
		(nerd-icons-install-fonts))
	(unless (file-exists-p "~/.local/share/fonts/FiraCodeNerdFontMono-Regular.ttf")
		(copy-file "~/.emacs.d/fonts/FiraCodeNerdFontMono-Regular.ttf"
				   "~/.local/share/fonts/FiraCodeNerdFontMono-Regular.ttf")
		(restart-emacs)))



;; Doom modeline
(use-package doom-modeline
	:after nerd-icons
	:init (doom-modeline-mode 1)
	:custom
	(doom-modeline-hud t))



;; Magit, git client
(use-package magit
    :config
    (setq magit-blame-styles
          '((margin
             (margin-width . 20)
             (margin-format . (" %c%f"))
             (margin-face . ((t (:foreground "#b4befe" :background "#45475a" :box "#6c7086"))))
             (margin-body-face . ((t (:foreground "#b4befe" :background "#45475a"))))
             (show-message . t)))))

(use-package ghub
	:after magit)

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package hl-todo
  :after magit
  :config
  (setq
   hl-todo-highlight-punctuation ":"
   hl-todo-keyword-faces
   '(("TODO"   . "#fe640b")
     ("FIXME"  . "#179299")
     ("NOTE"   . "#7287fd")))
  (with-eval-after-load 'magit
  (add-hook 'magit-log-wash-summary-hook
            #'hl-todo-search-and-highlight t)
  (add-hook 'magit-revision-wash-message-hook
            #'hl-todo-search-and-highlight t))
  (global-hl-todo-mode)
  )



;; Company, code autocompletion
(use-package company
	:hook
	(prog-mode . company-mode)
    (scala-mode . company-mode)
    (c++-mode . company-mode)
	(c-mode . company-mode)
    (rust-mode-hook . company-mode)
	(after-init-hook . global-company-mode))


;; Rustic installation
(use-package rustic
  :ensure t
  :after
  (rust-mode)
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-cargo-use-last-stored-arguments t))

;; Rust language support
(use-package rust-mode
    :init
    (setq rust-format-on-save t)
    :config
    (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
    (add-hook 'rust-mode-hook (lambda () (prettify-symbols-mode))))

;; Cargo rust mode
(use-package cargo-mode
  :hook
  (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-scroll-output t))

;; Meson mode
(use-package meson-mode
    :after
    (company)
    :hook
    (meson-mode-hook . company-mode)
    )


;; Scala mode
;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package lsp-metals
  :after lsp-mode
  :custom
  ;; You might set metals server options via -J arguments. This might not always work, for instance when
  ;; metals is installed using nix. In this case you can use JAVA_TOOL_OPTIONS environment variable.
  (lsp-metals-server-args '(;; Metals claims to support range formatting by default but it supports range
                            ;; formatting of multiline strings only. You might want to disable it so that
                            ;; emacs can use indentation provided by scala-mode.
                            "-J-Dmetals.allow-multiline-string-formatting=off"
                            ;; Enable unicode icons. But be warned that emacs might not render unicode
                            ;; correctly in all cases.
                            "-J-Dmetals.icons=unicode"))
  ;; In case you want semantic highlighting. This also has to be enabled in lsp-mode using
  ;; `lsp-semantic-tokens-enable' variable. Also you might want to disable highlighting of modifiers
  ;; setting `lsp-semantic-tokens-apply-modifiers' to `nil' because metals sends `abstract' modifier
  ;; which is mapped to `keyword' face.
  (lsp-metals-enable-semantic-highlighting t)
  :hook (scala-mode . lsp))


;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

;; LSP, language server
(use-package lsp-mode
	:init (setq lsp-keymap-prefix "M-s l")
	:hook ((c++-mode . lsp)
		   (c-mode . lsp)
           (rust-mode-hook . lsp)
           (scala-mode . lsp)
           (lsp-mode . lsp-lens-mode)
           ;; if you want which-key integration
           (lsp-mode . lsp-enable-which-key-integration))
	:commands (lsp lsp-diferred))

;; LSP-UI, interface lsp helpers
(use-package lsp-ui
	:after lsp-mode
	:custom
	(lsp-ui-sideline-enable t)
	(lsp-ui-sideline-show-diagnostics t)
	(lsp-ui-sideline-update-mode 1)
	(lsp-ui-sideline-delay 0.5)
	(lsp-ui-peek-enable t)
	(lsp-ui-peek-show-directory 1)
	(lsp-ui-doc-position 'top)
	(lsp-ui-doc-side 'right)
	(lsp-ui-doc-delay 1.5)
	(lsp-ui-doc-show-with-cursor t))

(use-package lsp-treemacs
	:ensure lsp-ui
	:after lsp-ui)



;; DAP - Debugger Adapter Protocol
(use-package dap-mode
	:after lsp-mode
	:config
	(dap-mode 1)
	(dap-ui-mode 1)
	(dap-tooltip-mode 1)
	(tooltip-mode 1)
	(dap-ui-controls-mode 1)
    (setq dap-auto-configure-features '(tooltip))
    (add-hook 'dap-terminated-hook (lambda (arg) (call-interactively #'dap-ui-controls-mode))))

(use-package dap-gdb-lldb
	:ensure dap-mode
	:after dap-mode
	:config
	(dap-gdb-lldb-setup))


;; Treemacs, Popup window with project tree
(use-package treemacs)

;; Treemacs and Projectile copability
(use-package treemacs-projectile
	:after (projectile treemacs))



;; Vertico, search helper
(use-package vertico
	:init
    (vertico-mode)
	:custom
    (vertico-cycle t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
	:init
    (savehist-mode))

;; A few more useful configurations...
(use-package emacs
	:hook (minibuffer-setup-hook . cursor-intangible-mode)
	:init
	(defun crm-indicator (args)
		(cons (format "[CRM%s] %s"
					  (replace-regexp-in-string
					   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
					   crm-separator)
					  (car args))
			  (cdr args)))
	(advice-add #'completing-read-multiple :filter-args #'crm-indicator)
	(setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
	(setq enable-recursive-minibuffers t)
	(setq read-extended-command-predicate #'command-completion-default-include-p))

;; `orderless' completion style, for fancy complition in vertico buffers.
(use-package orderless
	:after vertico
	:init
	(setq completion-styles '(substring orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion)))))

;; Configure directory extension.
(use-package vertico-directory
	:after vertico
	:ensure vertico
	:bind (:map vertico-map
				("<backspace>" . vertico-directory-up)
				("RET" . vertico-directory-enter)
				("<backspace>" . vertico-directory-delete-char)
				("C-<backspace>" . vertico-directory-delete-word))
	:hook
    (rfn-eshadow-update-overlay . vertico-directory-tidy))



;; Dashboard, welcome menu
(use-package dashboard
	:after (centaur-tabs nerd-icons)
	:custom
	(dashboard-center-content t)		                ; Content is not centered by default. To center, set
	(dashboard-vertically-center-content t)             ; vertically center content
	(dashboard-items '((recents   . 5)
					   (bookmarks . 5)
					   (projects  . 5)
					   (agenda    . 5)))
	(dashboard-heading-shorcut-format " [%s]")
	(dashboard-item-shortcuts '((recents   . "r")
								(bookmarks . "m")
								(projects  . "p")
								(agenda    . "a")))
	(dashboard-navigation-cycle t)
	(dashboard-display-icons-p t)                                       ; display icons on both GUI and terminal
	(dashboard-icon-type 'nerd-icons)								    ; Use `nerd-icons' package
	:config
	(add-hook 'dashboard-after-initialize-hook #'centaur-tabs-local-mode)                    ; Hides tabs at dashboard
	(add-hook 'dashboard-after-initialize-hook (lambda () (setq-local mode-line-format nil))); Hides modeline at dashboard
	(add-hook 'dashboard-after-initialize-hook #'dashboard-jump-to-recents)	                 ; Jumps to the begining of the dashboard, at emacs initialize
    (dashboard-setup-startup-hook)
	(hl-line-mode -1))



;; Projectile, project manager
(use-package projectile)



;; Centaur-tabs, buffers in tabs
(use-package centaur-tabs
	:custom
	(centaur-tabs-height 38)
	(centaur-tabs-set-icons t)
	(centaur-tabs-plain-icons t)
	(centaur-tabs-gray-out-icons 'buffer)
	(centaur-tabs-set-bar 'over)
	(centaur-tabs-set-close-button nil)
	(centaur-tabs-set-modified-marker t)
	(centaur-tabs-icon-scale-factor 2)
	(centaur-tabs-modified-marker "󰳼")
	(centaur-tabs-style "bar")
	:config
	(add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
	(centaur-tabs-change-fonts "FiraCode Nerd Font Mono" 165)
	(centaur-tabs-mode))



;; Shell-pop, popup quater-sized shell
(use-package shell-pop
	:after centaur-tabs
	:config
	(defun pop-shell ()
		(let ((current-prefix-arg -1))
			(centaur-tabs-local-mode))
		(end-of-buffer)
		(scroll-up))
	(setq shell-pop-in-after-hook 'pop-shell))



;; Catppuccin color theme
(use-package catppuccin-theme
	:init
	(load-theme 'catppuccin :no-confirm)
	:custom
	(catppuccin-flavor 'mocha)
	:config
	(set-frame-font "FiraCode Nerd Font Mono" nil t)
	(set-face-attribute 'default nil :height 140))



;;Which-key, key suggestion
(use-package which-key
	:custom
	(setq which-key-popup-type 'minibuffer)
	(setq which-key-idle-delay 0.5)
	(setq which-key-add-column-padding 3)
	(setq which-key-prefix-prefix "󰍜 " )
	(setq which-key-separator " 󰧂 " )
	:config
	(which-key-mode)
	(which-key-setup-side-window-bottom))



;; Diff-hl
(use-package diff-hl
	:after magit
	:init
	(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
	(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
	:config
	(global-diff-hl-mode)
	(diff-hl-margin-mode))



;; Blamer
(use-package blamer
	:defer 20
	:custom
    (blamer-idle-time 0.3)
	(blamer-min-offset 70)
	(blamer-author-formatter "  ✎ %s ")
	(blamer-datetime-formatter "[%s]")
	(blamer-commit-formatter " ● %s")
	(blamer-prettify-time-p t)
	(blamer-show-avatar-p t)
	(blamer-type 'visual)
	(blamer-max-commit-message-length 1000)
	:custom-face
    (blamer-face ((t :foreground "#7a88cf"
                     :background nil
                     :height 140
                     :italic t))))
(global-blamer-mode 0)                  ;; Отключаем Blamer потому что не всегда удобно


;; General, Fancy keybindings
(use-package general
	:after (magit blamer treemacs lsp centaur-tabs))
