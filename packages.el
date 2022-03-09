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
(package! company-prescient)
(package! counsel-tramp)
(package! doct)
(package! ebuku)
(package! emacsql-sqlite3)
(package! fzf)
(package! hyperbole)
(package! i3wm)
(package! i3wm-config-mode)
(package! jenkinsfile-mode)
(package! khardel)
(package! major-mode-hydra)
(package! mixed-pitch)
(package! mu4e-column-faces)
(package! mutt-mode)
(package! nginx-mode)
(package! org-appear)
(package! org-edna)
(package! org-notifications)
(package! org-pdftools)
(package! org-projectile)
(package! org-ref)
(package! org-reverse-datetree)
(package! org-roam-timestamps)
(package! org-roam-ui)
(package! org-super-agenda)
(package! org-wild-notifier)
(package! ox-gfm)
(package! prescient)
(package! salt-mode)
(package! ssh-config-mode)
(package! systemd)
(package! vlf)
(package! wakatime-mode)
(package! weblorg)

(package! chezmoi            :recipe (:host github :repo "tuh8888/chezmoi.el"))
(package! company-nginx      :recipe (:host github :repo "emacsmirror/company-nginx"))
(package! khalel             :recipe (:host github :repo "emacsmirror/khalel"))
(package! mu4e-marker-icons  :recipe (:host github :repo "emacsmirror/mu4e-marker-icons"))
(package! org-view-mode      :recipe (:host github :repo "amno1/org-view-mode"))
(package! outshine           :recipe (:host github :repo "alphapapa/outshine"))
(package! ox-hugo            :recipe (:host github :repo "kaushalmodi/ox-hugo"))

;; TESTING
(package! info-colors)
(package! theme-magic :pin "844c4311bd26ebafd4b6a1d72ddcc65d87f074e3")
(package! modus-themes :pin "18c08990c6b0ec7f144f9155da46988aa8c95e3a")

