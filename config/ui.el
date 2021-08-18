;;; ui.el -*- lexical-binding: t; -*-

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Use snipe for horizontal and vertical movement.
(setq evil-snipe-scope 'whole-visible)

(when (> (display-pixel-height) 1200)
  (setq doom-font (font-spec :family "Fira Code" :size 21)
        doom-variable-pitch-font (font-spec :family "DejaVu Sans" :style "Regular" :size 30 :weight 'regular)
        doom-big-font (font-spec :family "Fira Code" :size 26)))

(when (< (display-pixel-height) 1200)
  (setq doom-font (font-spec :family "Fira Code" :size 14)
        doom-variable-pitch-font (font-spec :family "DejaVu Sans" :style "Regular" :size 16 :weight 'regular)
        doom-big-font (font-spec :family "Fira Code" :size 20)))

(use-package! company-posframe
  :hook (company-mode . company-posframe-mode))
