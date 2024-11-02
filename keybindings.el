;;Common


;; Ctrl - Z to undo, Meta - Z to redo
(general-unbind
	"C-z"
	"M-z"
	"C-?"
	"C-/"
    "M-}"
    "M-{"
	)
(general-define-key
    :keymaps 'undo-fu-mode-map
    "C-/" 'nil
    "C-z" 'undo-fu-only-undo
    )


;; Global hotkeys
(general-def
	"C-c c c" 'compile					  ; Start compilation
	"M-z" 'undo-redo					  ; Rollback undoed changes
	"C-z" 'undo						  ; Undo changes
	"C-c r" 'restart-emacs				  ; Restart emacs
	"C-`" 'shell-pop					  ; Popup terminal
    "M-p" 'backward-paragraph             ; Moves coursor to the next paragraph
    "M-n" 'forward-paragraph              ; Moves coursor to the previous paragraph
	"M-<up>" 'move-text-up				  ; Moves selected region up
	"M-<down>" 'move-text-down			  ; Moves selected region down
    "C-c f w" 'delete-trailing-whitespace ; Deletes witespaces at the end of the line
    )


;; C/C++ modes hotkeys
(general-define-key
    :keymaps '(c++-mode-map c-mode-map)
    "<backtab>" 'un-indent-by-removing-4-spaces   ; Backtab selected block
    "C-> ." 'xah-forward-dot-comma				  ; Move coursor to ',' or '.' forward
    "C-< ," 'xah-backward-dot-comma			      ; Move coursor to ',' or '.' backward
    "C-> =" 'xah-forward-equal-sign			      ; Move coursor to '=' forward
    "C-< -" 'xah-backward-equal-sign			  ; Move coursor to '=' backward
    "M-]" 'arg-forward							  ; Move coursor forward to next arg in args list
    "M-[" 'arg-backward						      ; Move coursor to the top of args list
    )


;; Dired
;; Subdir perfomance control
(general-define-key
    :keymaps 'dired-mode-map
    "<tab>" 'dired-insert-subdir        ; Open extra subdir
    "<backtab>" 'dired-kill-subdir      ; Kill extra subdir
    )

;; Creating new object in dired mode
(general-define-key
    :keymaps 'dired-mode-map
    :prefix "C-c d"
    "f" 'make-empty-file                ; New empty file
    "d" 'make-directory                 ; New directory
    )



;; Macro ring actions
;; Disabling basic macro bindings
(general-unbind
    :prefix "C-x C-k"
    "C-n"
    "C-p"
    "C-a"
    "C-d"
    )

(general-def
    "<f6>" 'kmacro-cycle-ring-next      ; Next in ring
    "<f5>" 'kmacro-cycle-ring-previous  ; Previous in ring
    "<f8>" 'kmacro-set-counter          ; Set macro counter value
    "<f9>" 'kmacro-delete-ring-head     ; Delete macro from macro ring
    "<f7>" 'kmacro-add-counter          ; Add to macro cnt
    )



;; M-s - Start utility prefix
(defconst start-utility-prefix "M-s ")


;; LSP
(defconst lsp-prefix (concat start-utility-prefix "l")) ; LSP utility prefix - l
(general-create-definer lsp-keymap)

(lsp-keymap
    :prefix lsp-prefix
	""  '(nil :wk "LSP language server")
    "s" '(lsp-ui-imenu :wk "imenu with code entities")                  ; imenu with Code entities
    "l" '(lsp :wk "Start Language server")                              ; Start language server
	"d" '(dap-debug :wk "Start Debugging")                              ; Start Debugging
    )

(lsp-keymap
    :keymaps 'lsp-ui-mode-map
    [remap xref-find-definitions] 'lsp-ui-peek-find-definitions ; Remap for definitions search
    [remap xref-find-references] 'lsp-ui-peek-find-references   ; Remap for references search
)

;; DAP (Debugger)
(defconst dap-prefix (concat start-utility-prefix "d")) ; DAP utility prefix - l
(general-create-definer dap-keymap)

(dap-keymap
    :prefix dap-prefix
	""  '(nil :wk "DAP - Debugger")
    "l" '(dap-ui-locals :wk "Show window with locals")                       ; Open locals window
    "w" '(dap-ui-expressions :wk "Show window with watching expressions")    ; Open expressions window
    "s" '(dap-ui-sessions :wk "Show window with active debug sessions")      ; Open debug sessions window
    "c" '(dap-ui-controls-mode :wk "Toggle control buttons")                 ; Toggle debug control buttons bar
    "e" '(dap-eval :wk "Evaluate expression")                                ; Evaluate expression for GDB
    "d" '(dap-ui-breakpoints :wk "Show window with breakpoints")             ; Open breakpoints window
    "h" '(dap-hydra :wk "Minibuffer with debugging hotkeys")                 ; Open panel with hotkeys (Nice to use instead control buttons)
	"d" '(dap-debug :wk "Start Debugging")                                   ; Start Debugging session
    )


;; Treemacs
(defconst treemacs-prefix (concat start-utility-prefix "t")) ;Treemacs utility prefix - t
(general-create-definer treemacs-keymap)

(treemacs-keymap "C-<tab>" 'treemacs-select-window)		; Toggle focus with treemacs

(treemacs-keymap
    :keymaps 'treemacs-mode-map
    "c" 'treemacs-remove-project-from-workspace ; Close workspace
    "a" 'treemacs-add-project                   ; Add extra workspace
	"C-H" 'nil
	"C-<left>" 'treemacs-root-up		        ; Move treemacs root one directory up
	"C-<right>" 'treemacs-root-down				; Move treemacs root to selected directory
	"C-<up>" 'treemacs-move-project-up		    ; Move project up in treemacs
	"C-<down>" 'treemacs-move-project-down		; Move project down in treemacs
    )


;; Centaur-tabs
(defconst centaur-prefix (concat start-utility-prefix "c")) ; Centaur-tabs utility prefix - c
(general-create-definer centaur-keymap)

(centaur-keymap
    :prefix centaur-prefix
	""  '(nil :wk "Centaur-tabs")
    "c" '(centaur-tabs-mode :wk "Toggling tab mode")              ; Toggling tab mode
    )

(centaur-keymap
    "C-." 'centaur-tabs-forward         ; Switch tab forward
    "C-," 'centaur-tabs-backward        ; Switch tab backward
    )

;; Magit + blamer
(defconst magit-prefix (concat start-utility-prefix "g")) ; Centaur-tabs utility prefix - c
(general-create-definer magit-keymap)

(magit-keymap
    :keymaps 'magit-mode-map
	"M-a" 'magit-smerge-keep-all
	"M-b" 'magit-smerge-keep-base
	"M-c" 'magit-smerge-keep-current
	"M-l" 'magit-smerge-keep-lower
	"M-u" 'magit-smerge-keep-upper
	)

(magit-keymap
    :prefix magit-prefix
	""  '(nil :wk "Magit")
	"g" '(magit :wk "Opens magit dashboard") ; Opens magit dashboard
    "d" '(magit-dispatch :wk "Opens short magit dashboard")	; Opens short magit dashboard
	"i" '(blamer-show-posframe-commit-info :wk "Show pop-up commit info") ; Show pop-up commit info
  	"t" '(global-blamer-mode :wk "Toggle message printing") ; Toggle message printing
	"b" '(magit-blame :wk "Opens magit blame settings")	; Opens magit blame settings
    )
