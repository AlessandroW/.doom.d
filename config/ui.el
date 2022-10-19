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

(if (equal machine "workstation")
    (setq doom-font (font-spec :family "Fira Code" :size 18)
          doom-variable-pitch-font (font-spec :family "DejaVu Sans" :style "Regular" :size 18 :weight 'regular)
          doom-big-font (font-spec :family "Fira Code" :size 22))
  (when (> (display-pixel-height) 1200)
    (setq doom-font (font-spec :family "Fira Code" :size 22)
          doom-variable-pitch-font (font-spec :family "DejaVu Sans" :style "Regular" :size 22 :weight 'regular)
          doom-big-font (font-spec :family "Fira Code" :size 26)))

  (when (< (display-pixel-height) 1200)
    (setq doom-font (font-spec :family "Fira Code" :size 14)
          doom-variable-pitch-font (font-spec :family "DejaVu Sans" :style "Regular" :size 16 :weight 'regular)
          doom-big-font (font-spec :family "Fira Code" :size 20))))

(when IS-MAC
  (setq doom-font (font-spec :family "Fira Code" :size 14)
        doom-variable-pitch-font (font-spec :family "Fira Code" :size 14)
        doom-big-font (font-spec :family "Fira Code" :size 20)))
(when (equal machine "desktop")
    (setq doom-font (font-spec :family "Fira Code Nerd Font" :size 22 :style "Regular")
          doom-variable-pitch-font (font-spec :family "Overpass Nerd Font" :size 25 :weight 'regular)
          doom-big-font (font-spec :family "Fira Code Nerd Font" :size 30)))
(defvar required-fonts '("Fira Code Nerd Font" "Alegreya" "Overpass Nerd Font"))

(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)

(after! mixed-pitch
  (defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)
  (setq mixed-pitch-set-height t)
  (setq variable-pitch-serif-font (font-spec :family "Alegreya" :size 27))
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))

(defvar available-fonts
  (delete-dups (or (font-family-list)
                   (split-string (shell-command-to-string "fc-list : family")
                                 "[,\n]"))))

(defvar missing-fonts
  (delq nil (mapcar
             (lambda (font)
               (unless (delq nil (mapcar (lambda (f)
                                           (string-match-p (format "^%s$" font) f))
                                         available-fonts))
                 font))
             required-fonts)))

(if missing-fonts
    (pp-to-string
     `(unless noninteractive
        (add-hook! 'doom-init-ui-hook
          (run-at-time nil nil
                       (lambda ()
                         (message "%s missing the following fonts: %s"
                                  (propertize "Warning!" 'face '(bold warning))
                                  (mapconcat (lambda (font)
                                               (propertize font 'face 'font-lock-variable-name-face))
                                             ',missing-fonts
                                             ", "))
                         (sleep-for 0.5))))))
  ";; No missing fonts detected")
