;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(load! "local.el") ;; Machine-specific settings.

(setq user-full-name "Alessandro Wollek"
      user-mail-address "contact@wollek.ai"
      org-directory my/org-directory)

(load! "config/org.el")
(load! "config/org-roam.el")

;; UI and Window Management
(desktop-save-mode 1)

(setq! doom-theme 'doom-one
       doom-font (font-spec :family "Fira Code" :size 14)
       doom-variable-pitch-font (font-spec :family "Fira Code" :size 14)
       ;; Big font for recording
       doom-big-font (font-spec :family "Fira Code" :size 24)
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
  (message "removed hook!")
  (remove-hook 'after-change-major-mode-hook #'doom-highlight-non-default-indentation-h))

(setq! ;; Use snipe for horizontal and vertical movement.
       evil-snipe-scope 'whole-visible)

;; HOOCKS
(add-hook 'prog-mode-hook #'my/highlight-occurrences)
;; Run after doom-init-ui-h that sets the doom-highlight-non-default-indentation-h
(add-hook 'window-setup-hook #'my/remove-doom-whitespace-highlight -99)
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

(use-package! elaiza
  :config
  (setq! elaiza-default-model (make-elaiza-gpt-4o-mini)
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
