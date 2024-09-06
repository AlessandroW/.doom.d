;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Highlight word under point
(package! idle-highlight-mode)
;; Zotero integration in org-mode
(package! org-ref)
;; Personal AI assistant, https://github.com/SFTtech/emacs-elaiza
(package! elaiza :recipe (:local-repo "/Users/alessandro/src/work/elaiza" :build (:not compile)))
;; Hide emphasis markers in org
(package! org-appear :recipe (:host github :repo "awth13/org-appear"))
;; Prettier org margins
(package! olivetti)
;; Show others what I type
(package! keycast)
;; What are the Info keybindings again?
(use-package! casual-info :defer t)
