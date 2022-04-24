;;; $doomdir/packages.el --- Packages for Doom -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2022, marty buchaus, all rights reserved.
;; created:  1 November 2021
;;
;;;; Notes
;;
;; 2021 12 03 Removed Github version of org-roam-ui and org-roam-timestamps to melpa version
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package! evil-snipe :disable t)
(package! solaire-mode :disable t)

(unpin! org-roam)

(package! aggressive-indent)
(package! all-the-icons-dired)
(package! boxy)
(package! boxy-headings)
(package! chezmoi)
(package! company-org-block)
(package! company-prescient)
(package! dashboard)
(package! doct)
(package! emacsql-sqlite3)
(package! fzf)
(package! i3wm)
(package! i3wm-config-mode)
(package! info-colors)
(package! khardel)
(package! major-mode-hydra)
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
(package! templatel)
(package! systemd)
(package! vimrc-mode)
(package! vlf)
(package! wakatime-mode)
(package! weblorg)

(package! company-nginx      :recipe (:host github :repo "emacsmirror/company-nginx"))
(package! consult-tramp      :recipe (:host github :repo "ladicle/consult-tramp"))
(package! consult-org-roam   :recipe (:host github :repo "jgru/consult-org-roam"))
(package! khalel             :recipe (:host github :repo "emacsmirror/khalel"))
(package! outshine           :recipe (:host github :repo "alphapapa/outshine"))
(package! ox-hugo            :recipe (:host github :repo "kaushalmodi/ox-hugo"))
