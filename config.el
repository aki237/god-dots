;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Initial Essentials;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/essentials/cl-lib.el")                 ;; load the essentials first
(load-file "~/.emacs.d/mylisp/init.el")                       ;; load the function that I wrote.
(load-file "~/.emacs.d/mylisp/mypkg.el")                      ;; load the package manager that I wrote.
(mypkg/package-directory "~/.emacs.d/mypkg/")                 ;; call to load all the git based packages in that directory
(mypkg/load-el-from "~/.emacs.d/external")                    ;; load all the external single file packages.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;All requires;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'multiple-cursors)                                   ;; #include <multiple-cursors.h>
(require 'magit)
(require 'neotree)
(require 'autopair)                                           ;; #include <autopair>
(require 'expand-region)                                      ;; #include <expand-region>
(require 'yasnippet)                                          ;; #include <yasnippet>
(require 'go-mode-autoloads)                                  ;; #include <go-mode-autoloads>
(require 'auto-complete)                                      ;; #include <auto-complete>
(require 'auto-complete-config)                               ;; #include <auto-complete-config>
(require 'go-autocomplete)                                    ;; #include <go-autocomplete>
(require 'ido-better-flex)                                    ;; #include <ido-better-flex>
(require 'fuzzy-file-finder)                                  ;; #include <fuzzy-file-finder>
(require 'powerline)
(require 'dockerfile-mode)
(require 'mybkmrk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;All settings;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(display-battery-mode)
(setq battery-update-interval 1)
(setq battery-mode-line-format "🔌%B \% %p")
(global-whitespace-mode)
(server-start)
(setq inhibit-startup-screen t)                               ;; Inhibit the startup screen and show the scratch buffer
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.gl\\'" . lisp-mode))
(defun my-go-mode-hook ()			              ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  (local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(add-hook 'go-mode-hook 'my-go-mode-hook)
(autoload 'markdown-mode "markdown-mode" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(ido-better-flex/enable)                                      ;; fuzzy find powers for ido-mode.
(powerline-center-evil-theme)
(powerline-default-theme)
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(menu-bar-mode -1)                                            ;; Disable menu-bars
(tool-bar-mode -1)                                            ;; Disable tool bars
(setq make-backup-files nil)                                  ;; Disable the temporary backup files created by emacs (files with ~ suffix)
(set-default 'truncate-lines t)                               ;; Disable text wrapping
(global-linum-mode 1)                                         ;; Show Line numbers
(autopair-global-mode)                                        ;; Auto pairing brackets, quotes, and stuff.....
(yas-global-mode 1)                                           ;; Enabling Snippets plugin globally
(setq linum-format "%4d ")                                    ;; 4 digit linum numbers
(ido-mode 1)                                                  ;; enable ido-mode
(scroll-bar-mode 0)                                           ;; Disable scroll bar mode
(delete-selection-mode 1)                                     ;; Replace the selected text with the text being typed.
(global-hl-line-mode t)                                       ;; Highlight the current line.
(show-paren-mode 1)                                           ;; Highlight matching parenthesis.
(ac-config-default)                                           ;; auto-complete default config.
(setq show-paren-delay 0)                                     ;; Set the paranthesis show time
(setq-default left-fringe-width 0)                            ;; Set the width of the left fringe - disabled :P
(setq-default right-fringe-width 0)                           ;; Set the width of the right fringe - disabled :P
(setq url-proxy-services '(("http" . "127.0.0.1:9999")))      ;; Set http proxy for all the url services.
(setq url-proxy-services '(("https" . "127.0.0.1:9999")))     ;; Set https proxy for all the url services.
(setq url-proxy-services '(("socks" . "127.0.0.1:9050")))     ;; Set socks proxy for all the url services.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")    ;; Add a directory to search for themes.
(load-theme 'granger t)                                      ;; Load my favourite theme.
(setq-default c-basic-offset 4)
(mybkmrk/init)                                                ;; yacp : yet another crappy package
(setq-default line-spacing 5)                                 ;; Set default line sppacing
(setq neo-smart-open t)
(defun doom*neo-insert-fold-symbol (name)
  "Custom hybrid unicode theme with leading whitespace."
  (or (and (eq name 'open)  (neo-buffer--insert-with-face " - 📂 " 'neo-expand-btn-face))
      (and (eq name 'close) (neo-buffer--insert-with-face " + 📁 " 'neo-expand-btn-face))
      (and (eq name 'leaf)  (neo-buffer--insert-with-face "   " 'neo-expand-btn-face))))
(advice-add 'neo-buffer--insert-fold-symbol :override 'doom*neo-insert-fold-symbol)
(set-fontset-font t 'unicode "Symbola" nil 'prepend)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;Function Hooks;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook #'hs-minor-mode)                    ;; Hide or show blocks of code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;All key chords;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-n") 'next-buffer)                     ;; Go to the next buffer
(global-set-key (kbd "M-p") 'previous-buffer)                 ;; Go to previous buffer
(global-set-key (kbd "M-\\") (kbd "C-x o"))                   ;; Switch splits in multiple splits.
(global-set-key (kbd "C-}") (kbd "C-x }"))                    ;; Enlarge split area
(global-set-key (kbd "C-{") (kbd "C-x {"))                    ;; Shrink split area
(global-set-key (kbd "C-4") 'aki237/duplicate-line)           ;; Key chord for "duplicate-line" function : mylisp/init.el
(global-set-key (kbd "C-<return>")(kbd "C-x k"))              ;; Kill the current buffer.... I know It is dangerous....
(global-set-key (kbd "C-|") 'compile)                         ;; Open compile command mode.
(global-set-key (kbd "M-1") (kbd "C-x 1"))                    ;; Go to single pane mode if there is any splits
(global-set-key (kbd "M-2") (kbd "C-x 2"))                    ;; Open a hsplit at the bottom
(global-set-key (kbd "M-3") (kbd "C-x 3"))                    ;; Open a vsplit at right
(global-set-key (kbd "M-0") (kbd "C-x 0"))                    ;; Close the active split..(Not killed remember)
(global-set-key (kbd "C-?") 'comment-or-uncomment-region)     ;; Well.. Comment or uncomment region
(global-set-key (kbd "C-S-<up>") 'aki237/move-line-up)        ;; Move a line up - in init.el
(global-set-key (kbd "C-S-<down>") 'aki237/move-line-down)    ;; Move a line down - in init.el
(global-set-key (kbd "C-M-f") (kbd "C-c @ C-c"))              ;; CODE FOLDING !!!
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)           ;; Mark a few lines,trigger this and see the magic.
(global-set-key (kbd "C->") 'mc/mark-next-like-this)          ;; Mark next like this....Easy
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)      ;; Similar to above.
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)       ;; Mark all like this.....
(global-set-key (kbd "C-<tab>") 'er/expand-region)            ;; Expand selected text letter -> word -> quotes or brackets and so on
(global-set-key (kbd "M-f") 'fff:fuzzy-find)                  ;; My crappy package
(global-set-key (kbd "C-s-f") 'find-file-at-point)            ;; Open a file specified at the insertion point
(global-set-key (kbd "C-\\" ) 'neotree-toggle)                ;; Open directory structure split.
(global-set-key (kbd "C-x g") 'magit-status)                  ;; Open magit control panel (git status)
(global-set-key (kbd "C-M-p") 'mybkmrk/goto-project)        ;; Goto Project
(global-set-key (kbd "C-x C-a") 'mybkmrk/add-new-project)     ;; Add a new project
(setq right-fringe-width 0)
(setq left-margin-width 0)
(setq left-fringe-width 0)
(setq linum-format "%6d ")
