;;; packages.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2021, Marty Buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Packages

(package! evil-snipe :disable t)

(package! aggressive-indent)
(package! alert)
(package! all-the-icons-completion)
(package! autoinsert)
(package! browse-kill-ring)
(package! counsel-tramp)
(package! dashboard)
(package! ebuku)
(package! info-colors)
(package! khardel)
(package! khalel)
(package! mixed-pitch)
(package! mu4e-column-faces)
(package! mu4e-marker-icons)
(package! mutt-mode)
(package! org-edna)
(package! org-projectile)
(package! org-super-agenda)
(package! paperless)
(package! salt-mode)
(package! systemd)
(package! telega)
(package! wakatime-mode)

;;;;  IF
;;(when (string= (system-name) "archovo.home.snuffy.org"))

(package! activity-watch-mode           :recipe (:host github
                                                 :repo "pauldub/activity-watch-mode"))
(package! i3wm-config-mode              :recipe (:host github
                                                 :repo "Alexander-Miller/i3wm-Config-Mode"))
(package! org-pandoc-import             :recipe (:host github
                                                 :repo "tecosaur/org-pandoc-import"
                                                 :files ("*.el" "filters" "preprocessors")))
(package! org-pretty-table              :recipe (:host github
                                                 :repo "Fuco1/org-pretty-table"))
(package! org-roam-ui                   :recipe (:host github
                                                 :repo "org-roam/org-roam-ui"
                                                 :files ("*.el" "out")))
(package! org-transclusion              :recipe (:host github
                                                 :repo "nobiot/org-transclusion"
                                                 :branch "main"
                                                 :files ("*.el")))
(package! outshine                      :recipe (:host github
                                                 :repo "alphapapa/outshine"))
(package! vlf                           :recipe (:host github
                                                 :repo "m00natic/vlfi"
                                                 :files ("*.el") ))
(package! notdeft                       :recipe (:host github
                                                 :repo "hasu/notdeft"
                                                 :files ("*.el" "xapian")))
