;;; packages.el --- Summary -*- lexical-binding: t; no-byte-compile: t; -*-
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

(package! org-caldav)

(package! org-roam-ui                   :recipe (:host github
                                                 :repo "org-roam/org-roam-ui"
                                                 :files ("*.el" "out")))

(package! alert)

(package! aggressive-indent)

(package! autoinsert)

(package! i3wm-config-mode              :recipe (:host github
                                                 :repo "Alexander-Miller/i3wm-Config-Mode"))

(package! khardel)

(package! outshine                      :recipe (:host github
                                                 :repo "alphapapa/outshine"))

(package! paperless)

(package! salt-mode)

(package! systemd)

(package! counsel-tramp)

(package! vlf                           :recipe (:host github
                                                 :repo "m00natic/vlfi"
                                                 :files ("*.el") ))

(package! wakatime-mode)
