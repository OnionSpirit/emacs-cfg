;; My name and e-mail adress
(setq user-full-name   "Ivan Moskalev")
(setq user-mail-adress "wano.mos@gmail.com")


;; ;; Display the name of the current buffer in the title bar
;; (setq frame-title-format "GNU Emacs: %b")
;; ;; Disable name of the current buffer in the title bar
;; (setq frame-title-format "")

;; Disable system titlebar
(setq-default default-frame-alist
      (append
       '((undecorated . t)
         (drag-internal-border . t)
		 (drag-with-mode-line . t)
         (internal-border-width . 4))
       default-frame-alist))

;; Dired
(require 'dired)
(setq dired-recursive-deletes 'top)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)

;; Show-paren-mode settings. Selection between (), {}, [].
;; (show-paren-mode t)
;; (setq show-paren-style 'expression)

;; Electric-modes settings. Auto complete for bracers
(electric-pair-mode    1)
(electric-indent-mode -1)

;; Delete selection. Can paste above selected text
(delete-selection-mode t)

;;Truncate Lines
(setq-default truncate-lines t)

;; Lines indication settings.
(global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(global-hl-line-mode 1)

;; Fringe settings
(fringe-mode '(8 . 0)) ;; органичиталь текста только слева
;; Denable system-dependent key stuff
(set-keyboard-coding-system nil)

;; Map Alt key to Meta
(setq x-alt-keysym 'meta)

;; Display file size/time in mode-line
(setq display-time-24hr-format t) ;; 24-часовой временной формат в mode-line
(setq column-number-mode t)		  ;; Column number and line are shown
(display-time-mode             t) ;; показывать часы в mode-line
(size-indication-mode          t) ;; размер файла в %-ах
(show-paren-mode t)				  ;; Показывает строку со второй строкой

;; Syntax highlighting
(require 'font-lock)
(global-font-lock-mode             t) ;; включено с версии Emacs-22. На всякий...
(setq font-lock-maximum-decoration t)
(global-whitespace-mode 1)					;; Подветка символов отступа

;; Indent setting
(setq-default indent-tabs-mode nil) ;; отключить возможность ставить отступы TAB'ом
(setq-default tab-width          4) ;; ширина табуляции - 4 пробельных символа
(setq-default c-basic-offset     4)
(setq-default standart-indent    4) ;; стандартная ширина отступа - 4 пробельных символа
(setq-default lisp-body-indent   4) ;; сдвигать Lisp-выражения на 4 пробельных символа
(global-set-key (kbd "RET") 'newline-and-indent) ;; при нажатии Enter перевести каретку и сделать отступ
(setq lisp-indent-function  'common-lisp-indent-function)

;; Scrolling settings
(setq scroll-step               1) ;; вверх-вниз по 1 строке
(setq scroll-margin            10) ;; сдвигать буфер верх/вниз когда курсор в 10 шагах от верхней/нижней границы
(setq scroll-conservatively 10000)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; Highlight search resaults
(setq search-highlight        t)
(setq query-replace-highlight t)

;; Short messages for yes|no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1) ;; отключаем графическое меню
(tool-bar-mode     -1) ;; отключаем tool-bar
(scroll-bar-mode   -1) ;; отключаем полосу прокрутки
;; (blink-cursor-mode -1) ;; курсор не мигает
(setq use-dialog-box     nil) ;; никаких графических диалогов и окон - все через минибуфер
(setq redisplay-dont-pause t)  ;; лучшая отрисовка буфера
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал


;; Including specified files
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "functions.el")
(load-user-file "packages.el")
(load-user-file "keybindings.el")


;; Doom modeline autogen
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-matching-paren nil)
 '(doom-modeline-always-show-macro-register t)
 '(doom-modeline-bar-width 0)
 '(doom-modeline-buffer-file-true-name t)
 '(doom-modeline-buffer-modification-icon t)
 '(doom-modeline-display-default-persp-name t)
 '(doom-modeline-position-column-line-format '("%l:%c"))
 '(doom-modeline-position-line-format '(""))
 '(package-selected-packages
   '(@ centaur-tabs consult dashboard lsp-metals lsp-treemacs meson-mode
     mic-paren projectile shell-pop treemacs treemacs-projectile
     vertico vterm which-key))
 '(paren-display-message 'always)
 '(paren-dont-load-timer nil)
 '(paren-message-linefeed-display "\"^J\"")
 '(paren-message-no-match nil)
 '(paren-message-show-linenumber 'absolute)
 '(paren-sexp-mode nil)
 '(warning-suppress-log-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "#737994" :foreground "#e5c890"))))
 '(lsp-face-highlight-read ((t (:inherit highlight))))
 '(mode-line ((t (:foreground "#b4befe" :background "#45475a" :box "#cba6f7"))))
 '(mode-line-inactive ((t (:foreground "#a6adc8" :background "#313244" :box "#6c7086"))))
 '(paren-face-match ((t (:background "dark slate blue" :foreground "sky blue"))))
 '(paren-face-mismatch ((t (:background "light coral" :foreground "dark red"))))
 '(paren-face-no-match ((t (:foreground "goldenrod"))))
 '(trailing-whitespace ((t (:background nil :foreground "#fab387"))))
 '(whitespace-line ((t (:underline nil)))))
          ;Настройки для слишком длинных линий

;; make whitespace-mode use just basic coloring
;;(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
  '(
    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (newline-mark 10 [#x21B2 10]) ; 10 LINE FEED
    (tab-mark 9 [#x27F6 9] [#x27F6 9]) ; 9 TAB
    ))

;; Removing inital advices
(advice-remove 'nerd-icons-install-fonts #'my/disable-yornp)
(advice-remove 'restart-emacs #'my/disable-yornp)


;; Start window size and centring frame at the screen
(when (window-system)
    (set-frame-width (selected-frame) 150)
    (frame-center))
