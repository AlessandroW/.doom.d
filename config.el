;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Alessandro Wollek"
      user-mail-address "contact@wollek.ai")

(load! "local.el") ;; Machine-specific settings.
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
       evil-vsplit-window-right t)

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

(setq! ;; Use snipe for horizontal and vertical movement.
       evil-snipe-scope 'whole-visible)

;; HOOCKS
(add-hook 'prog-mode-hook 'my/highlight-occurrences)


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
