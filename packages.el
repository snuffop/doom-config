;;; $doomdir/packages.el --- Packages for Doom -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2022, marty buchaus, all rights reserved.
;; created:  1 November 2021
;;
;;;; Notes
;;
;; 2022 04 21 removed weblorg and tempmlatel  Switched back to ox_hugo
;; 2021 12 03 Removed Github version of org-roam-ui and org-roam-timestamps to melpa version
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package! evil-snipe :disable t)
(package! solaire-mode :disable t)

;; (package! atomic-chrome)
(package! aggressive-indent)
(package! all-the-icons-dired)
(package! boxy)
(package! boxy-headings)
(package! chezmoi)
(package! company-org-block)
(package! company-prescient)
(package! dashboard)
(package! doct)
(package! ebuku)
(package! emacsql-sqlite3)
(package! evil-matchit)
(package! fzf)
(package! i3wm)
(package! i3wm-config-mode)
(package! info-colors)
(package! keychain-environment)
(package! khardel)
(package! khalel)
(package! mu4e-column-faces)
(package! nginx-mode)
(package! org-appear)
(package! org-edna)
(package! org-notifications)
(package! org-pdftools)
(package! org-pomodoro)
(package! org-projectile)
(package! org-real)
(package! org-ref)
(package! org-reverse-datetree)
(package! org-roam-ui)
(package! org-super-agenda)
(package! org-wild-notifier)
(package! ox-gfm)
(package! prescient)
(package! ssh-config-mode)
(package! systemd)
(package! vimrc-mode)
(package! vlf)
(package! wakatime-mode)
(package! zeal-at-point)
(package! zoxide)

(package! company-nginx       :recipe (:host github :repo "emacsmirror/company-nginx"))
(package! consult-tramp       :recipe (:host github :repo "ladicle/consult-tramp"))
(package! khalel              :recipe (:host github :repo "emacsmirror/khalel"))
(package! outshine            :recipe (:host github :repo "alphapapa/outshine"))
(package! ox-hugo             :recipe (:host github :repo "kaushalmodi/ox-hugo"))
(package! org-ql              :recipe (:host github :repo "alphapapa/org-ql"))

