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
(package! all-the-icons-dired)
(package! company-nginx)
(package! company-prescient)
(package! consult-recoll)
(package! counsel-tramp)
(package! doct)
(package! ebuku)
(package! elfeed-protocol)
(package! fzf)
(package! jenkinsfile-mode)
(package! i3wm)
(package! i3wm-config-mode)
(package! khalel)
(package! khardel)
(package! magithub)
(package! major-mode-hydra)
(package! mu4e-column-faces)
(package! mu4e-marker-icons)
(package! mutt-mode)
(package! nginx-mode)
(package! org-appear)
(package! org-edna)
(package! org-noter)
(package! org-noter-pdftools)
(package! org-notifications)
(package! org-pdftools)
(package! org-projectile)
(package! org-ref)
(package! org-roam-bibtex)
(package! org-roam-timestamps)
(package! org-roam-ui)
(package! org-super-agenda)
(package! org-wild-notifier)
(package! ox-gfm)
(package! paperless)
(package! prescient)
(package! salt-mode)
(package! ssh-config-mode)
(package! systemd)
(package! wakatime-mode)
(package! vlf)


(package! activity-watch-mode      :recipe (:host github :repo "pauldub/activity-watch-mode"))
(package! org-ol-tree              :recipe (:host github :repo "Townk/org-ol-tree"))
(package! org-pandoc-import        :recipe (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors")))
(package! org-pandoc-import        :recipe (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors")))
(package! org-pretty-table         :recipe (:host github :repo "Fuco1/org-pretty-table"))
(package! org-recoll               :recipe (:host github :repo "alraban/org-recoll"))
(package! org-transclusion         :recipe (:host github :repo "nobiot/org-transclusion" :branch "main" :files ("*.el")))
(package! outshine                 :recipe (:host github :repo "alphapapa/outshine"))
