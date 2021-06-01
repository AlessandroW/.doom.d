;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alessandro Wollek"
      user-mail-address "alessandro.wollek@tum.de")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))



;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


(load! "config/org.el")
(load! "config/org-roam.el")
(load! "config/ui.el")
(load! "config/checkers.el")
(load! "config/tools.el")
(load! "config/completion.el")

(map! "C-x C-b" 'ivy-switch-buffer ) ;; Don't open the buffer menu when pressing Ctrl for too long.
(map! "C-ö" #'other-window
      "C-;" #'other-window)

(map! :n "Ü" #'evil-backward-paragraph)
(map! :n "*" #'evil-forward-paragraph)

(map! :mode 'org-mode :i "C-c TAB" #'org-table-toggle-column-width)

(defun copy-rectangle-to-system-clipboard (start end)
  "Like `copy-rectangle-as-kill', but also copy to system clipboard."
  (interactive "r")
  (call-interactively #'copy-rectangle-as-kill)
  (with-temp-buffer
    (yank-rectangle)
    (delete-trailing-whitespace)
    (funcall interprogram-cut-function (buffer-string))))
(map! "C-x r M-c" #'copy-rectangle-to-system-clipboard)


(after! 'python-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "/home/wollek/.pyenv/versions/3.8.8/bin/pyls")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote)))

;; Projetile
(setq projectile-indexing-method 'alien)  ;; No projectile post-processing, better for remote work
(setq projectile-enable-caching nil)
