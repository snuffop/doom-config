;;; $doomdir/packages.el --- Packages for Doom -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2022, marty buchaus, all rights reserved.
;; created:  1 November 2021
;;
;;;; Notes
;;
;; 2021 12 03 mRemoved Github version of org-roam-ui and org-roam-timestamps to melpa version
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package! evil-snipe :disable t)
(package! evil-escape :disable t)
(package! solaire-mode :disable t)

(package! aggressive-indent)
(package! all-the-icons-dired)
(package! beacon)
(package! boxy)
(package! boxy-headings)
(package! company-prescient)
(package! company-org-block)
(package! consult-dir)
(package! doct)
(package! emacsql-sqlite3)
(package! fzf)
(package! hyperbole)
(package! i3wm)
(package! i3wm-config-mode)
(package! info-colors)
(package! jenkinsfile-mode)
(package! khardel)
(package! major-mode-hydra)
(package! mixed-pitch)
(package! mu4e-column-faces)
(package! nginx-mode)
(package! org-appear)
(package! org-edna)
(package! org-notifications)
(package! org-pdftools)
(package! org-projectile)
(package! org-real)
(package! org-ref)
(package! org-reverse-datetree)
(package! org-roam-timestamps)
(package! org-roam-ui)
(package! org-super-agenda)
(package! org-wild-notifier)
(package! ox-gfm)
(package! prescient)
(package! ssh-config-mode)
(package! systemd)
(package! tree-sitter)
(package! tree-sitter-langs)
(package! vimrc-mode)
(package! vlf)
(package! wakatime-mode)
(package! weblorg)
(package! zeal-at-point)

(package! company-nginx      :recipe (:host github :repo "emacsmirror/company-nginx"))
(package! consult-org-roam   :recipe (:host github :repo "jgru/consult-org-roam"))
(package! consult-tramp      :recipe (:host github :repo "ladicle/consult-tramp"))
(package! emacs-with-nyxt    :recipe (:host github :repo "ag91/emacs-with-nyxt"))
(package! khalel             :recipe (:host github :repo "emacsmirror/khalel"))
(package! mu4e-marker-icons  :recipe (:host github :repo "emacsmirror/mu4e-marker-icons"))
(package! org-view-mode      :recipe (:host github :repo "amno1/org-view-mode"))
(package! outshine           :recipe (:host github :repo "alphapapa/outshine"))
(package! ox-hugo            :recipe (:host github :repo "kaushalmodi/ox-hugo"))
