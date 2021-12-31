;;; $doomdir/packages.el --- Packages for Doom -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2021, marty buchaus, all rights reserved.
;; created:  1 November 2021
;;
;;;; Notes
;;
;; 2021 12 03 mRemoved Github version of org-roam-ui and org-roam-timestamps to melpa version
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package! evil-snipe :disable t)
(package! solaire-mode :disable t)

(package! aggressive-indent)
(package! all-the-icons-dired)
(package! company-nginx)
(package! company-prescient)
(package! counsel-tramp)
(package! doct)
(package! ebuku)
(package! fzf)
(package! i3wm)
(package! i3wm-config-mode)
(package! jenkinsfile-mode)
(package! khalel)
(package! khardel)
(package! major-mode-hydra)
(package! mu4e-column-faces)
(package! mu4e-marker-icons)
(package! mutt-mode)
(package! nginx-mode)
(package! org-appear)
(package! org-edna)
(package! org-notifications)
(package! org-pdftools)
(package! org-projectile)
(package! org-ref)
(package! org-roam-timestamps)
(package! org-super-agenda)
(package! org-wild-notifier)
(package! ox-gfm)
(package! prescient)
(package! salt-mode)
(package! ssh-config-mode)
(package! systemd)
(package! vlf)
(package! wakatime-mode)

(package! outshine                 :recipe (:host github :repo "alphapapa/outshine"))
(package! org-transclusion         :recipe (:host github :repo "nobiot/org-transclusion"))
