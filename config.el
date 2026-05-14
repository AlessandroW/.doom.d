;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(load! "local.el") ;; Machine-specific settings.

(setq user-full-name "Alessandro Wollek"
      user-mail-address "contact@wollek.ai"
      org-directory my/org-directory)

(load! "config/org.el")
(load! "config/org-roam.el")

;; UI and Window Management
;(desktop-save-mode 1)

(setq! doom-theme 'doom-one
       doom-font (font-spec :family "FiraCode Nerd Font" :size 14)
       doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font" :size 14)
       ;; Big font for recording
       doom-big-font (font-spec :family "FiraCode Nerd Font" :size 24)
       display-line-numbers-type t
       evil-split-window-below t
       evil-vsplit-window-right t
       ;; no buffer<2> anymore
       uniquify-buffer-name-style 'forward)

(after! idle-highlight-mode ;; From https://github.com/TheJJ/conffiles/blob/6f15b4881ef3422980b9d146f708269de8fd8fa9/.doom.d/visual.el#L3
 (setq idle-highlight-idle-time 0.2
       idle-highlight-visible-buffers t))

(after! jit-lock-mode ;; From https://github.com/TheJJ/conffiles/blob/6f15b4881ef3422980b9d146f708269de8fd8fa9/.doom.d/visual.el#L10
 (setq jit-lock-stealth-time 0.3   ;; fontify unfontified areas when idle for this time
       jit-lock-stealth-nice 0.3   ;; time between fontifying chunks
       jit-lock-chunk-size 4096))  ;; number of characters to fontify at once

(defun my/highlight-occurrences ()
 "Highlight occurrences of word under cursor."
 (idle-highlight-mode t))

(defun my/whitespace-highlight ()
 "Originally from github.com/thejj/conffiles and doom/lisp.doom-ui.el"
 (unless (or (eq major-mode 'fundamental-mode)
             (eq major-mode 'org-mode)
             (null buffer-file-name))
   (require 'whitespace)
   (setq-local whitespace-style
               '(face indentation tabs tab-mark spaces space-mark newline newline-mark trailing)
               whitespace-display-mappings
               '((tab-mark ?\t [?› ?\t])
                 (newline-mark ?\n [?¬ ?\n])
                 (space-mark ?\  [?·] [?.])))
   (whitespace-mode +1)))

(defun my/remove-doom-whitespace-highlight ()
 "Remove dooms default highlight "
 (remove-hook 'after-change-major-mode-hook #'doom-highlight-non-default-indentation-h))

(defun my/use-apple-emoji-font ()
 "Remove dooms default highlight "
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/53
 (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)
 (remove-hook 'window-setup-hook #'use-apple-emoji-font))

(setq! ;; Use snipe for horizontal and vertical movement.
      evil-snipe-scope 'whole-visible)

(after! persp-mode
 ;; HACK from https://github.com/doomemacs/doomemacs/issues/4179
 (defun +workspace-switch (name &optional auto-create-p)
 "Switch to another workspace named NAME (a string).

If AUTO-CREATE-P is non-nil, create the workspace if it doesn't exist, otherwise
throws an error."
 (unless (+workspace-exists-p name)
   (if auto-create-p
       (+workspace-new name)
     (error "%s is not an available workspace" name)))
 (let ((old-name (+workspace-current-name)))
   (unless (equal old-name name)
     (setq +workspace--last
           (or (and (not (string= old-name persp-nil-name))
                    old-name)
               +workspaces-main))
     (unless (+workspace-exists-p "main")
       (+workspace-new "main"))
     (persp-switch "main")
     (persp-frame-switch name))
   (equal (+workspace-current-name) name))))

;; HOOKS
(add-hook 'prog-mode-hook #'my/highlight-occurrences)
;; Run after doom-init-ui-h that sets the doom-highlight-non-default-indentation-h
(add-hook 'window-setup-hook #'my/remove-doom-whitespace-highlight -99)
(add-hook 'window-setup-hook #'my/use-apple-emoji-font -99)
(add-hook 'after-change-major-mode-hook #'my/whitespace-highlight)


;; Custom functions
(defun my/insert-todays-date ()
  "Inserts today's date in the ISO 8601 format YEAR-MONTH-DAY."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; Keybindings
(map! "C-x C-b" #'consult-buffer  ;; don't open the buffer menu when pressing ctrl for too long.
      "C-;" #'other-window
      ;; use the mouse to go to the previous and next buffer.
      "<mouse-4>" #'previous-buffer
      "<mouse-5>" #'next-buffer)
(map! :leader
      :desc "Insert today's date as Y-m-d"
      "i d" #'my/insert-todays-date)

(defun my/switch-meta ()
  "Switch meta option key.
BUG: External keyboard meta is right by default."
  (interactive)
  (if (equal mac-option-modifier 'none)
      (setq! mac-option-modifier 'meta
            mac-right-option-modifier 'none)
    (setq! mac-option-modifier 'none
          mac-right-option-modifier 'meta)))

(after! elaiza
  (elaiza-backends--add-integration (make-elaiza-ollama
                                     :name "QWQ" :model "qwq"))
  (elaiza-backends--add-integration (make-elaiza-ollama
                                     :name "Qwen 2.5 coder" :model "qwen2.5-coder:32b"))
  (setq! elaiza-default-model (make-elaiza-claude-sonnet-3-7)
         elaiza-debug t))

;; YOUTUBE
(define-minor-mode youtube-mode
  "A minor mode for configuring YouTube related settings."
  :global t
  :init-value nil
  :lighter " YouTube"
  :group 'convenience
  :keymap (let ((map (make-sparse-keymap)))
            ;; You can define key bindings here if needed
            map)
  (if youtube-mode
      (progn
        (setq! org-directory my/public-org-directory
               org-roam-directory org-directory
               my/previous-frame-width (frame-width)
               my/previous-frame-height (frame-height))
        (set-frame-size (selected-frame) 1953 1080 t) ;; +33 px for the macOS bar
        (keycast-header-line-mode 1))
    (progn
      (setq! org-directory my/org-directory
             org-roam-directory my/org-roam-directory)
      (set-frame-size (selected-frame)
                      my/previous-frame-width
                      my/previous-frame-height)
      (keycast-header-line-mode -1))))

;; Flutter
(after! flutter
  ;; Open Flutter as popup.
  (set-popup-rule! "^\\*Flutter\\*"))

;; What are the Info keybindings again?
(use-package! casual-info :defer t)

(use-package! languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command "~/.languagetool/languagetool-commandline.jar"
        languagetool-server-command "~/.languagetool/languagetool-server.jar"))


(use-package! eglot-booster
  :after eglot
  :defer t
  :config (eglot-booster-mode))

;; TRAMP
(after! tramp
  (setq tramp-verbose 2))

(after! doom-modeline
  (defadvice! my/doom-modeline-skip-remote-a (&rest _)
    :before-until #'doom-modeline-update-buffer-file-name
    (and buffer-file-name (file-remote-p buffer-file-name))))

(defadvice! my/no-lsp-on-remote-a (&rest _)
  :before-until #'lsp!
  (and buffer-file-name (file-remote-p buffer-file-name)))

(after! magit
  (add-hook! 'magit-mode-hook
    (defun my/magit-remote-perf-h ()
      (when (file-remote-p default-directory)
        (setq-local magit-refresh-status-buffer nil
                    magit-commit-show-diff nil
                    magit-branch-direct-configure nil)))))

(after! files-x
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process))

;; Projectile + TRAMP
(defvar my/tramp--projectile-root-cache nil)
(defadvice! my/tramp--memoized-projectile-root-a (fn &optional dir)
  :around #'projectile-project-root
  (+tramp--memoize (or dir default-directory)
                   'my/tramp--projectile-root-cache fn dir))

(defadvice! my/projectile-alien-on-remote-a (fn directory)
  :around #'projectile-dir-files
  (let ((projectile-indexing-method
         (if (file-remote-p directory) 'alien projectile-indexing-method)))
    (funcall fn directory)))

(add-hook! 'tramp-cleanup-connection-hook
  (defun my/tramp-clear-projectile-cache-h ()
    (setq my/tramp--projectile-root-cache nil)))
