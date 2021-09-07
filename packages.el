;;; packages.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2021, Marty Buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Packages

(package! mixed-pitch)

(package! org-projectile)

(package! org-super-agenda)

(package! org-pandoc-import             :recipe (:host github
                                                 :repo "tecosaur/org-pandoc-import"
                                                 :files ("*.el" "filters" "preprocessors")))

(package! org-roam-ui                   :recipe (:host github
                                                 :repo "org-roam/org-roam-ui"
                                                 :files ("*.el" "out")))

(package! org-transclusion              :recipe (:host github
                                                 :repo "nobiot/org-transclusion"
                                                 :branch "main"
                                                 :files ("*.el")))

(package! org-edna)

(when (string= (system-name) "archovo.home.snuffy.org")
  (package! org-caldav))

(package! mu4e-column-faces)

(package! activity-watch-mode             :recipe (:host github
                                                   :repo "pauldub/activity-watch-mode"))

(package! aggressive-indent)

(package! alert)

(package! all-the-icons-completion)

(package! autoinsert)

(package! browse-kill-ring)

(package! eva                           :recipe (:host github :repo "meedstrom/eva"
                                                 :files (:defaults "assets"  "renv" "*.R" "*.gnuplot")))

(package! i3wm-config-mode              :recipe (:host github
                                                 :repo "Alexander-Miller/i3wm-Config-Mode"))

(package! khardel)

(package! outshine                      :recipe (:host github
                                                 :repo "alphapapa/outshine"))

(package! org-onenote                   :recipe (:host github
                                                 :repo "ifree/org-onenote"))

(package! paperless)

(package! salt-mode)

(package! systemd)

(package! counsel-tramp)

(package! vlf                           :recipe (:host github
                                                 :repo "m00natic/vlfi"
                                                 :files ("*.el") ))

(package! wakatime-mode)
