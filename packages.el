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

(package! mu4e-marker-icons)

(package! activity-watch-mode             :recipe (:host github
                                                   :repo "pauldub/activity-watch-mode"))

(package! aggressive-indent)

(package! alert)

(package! all-the-icons-completion)

(package! autoinsert)

(package! browse-kill-ring)

(package! dashboard)

(package! elpher)

(package! i3wm-config-mode              :recipe (:host github
                                                 :repo "Alexander-Miller/i3wm-Config-Mode"))

(package! info-colors)

(package! khardel)

(package! outshine                      :recipe (:host github
                                                 :repo "alphapapa/outshine"))

(package! org-onenote                   :recipe (:host github
                                                 :repo "ifree/org-onenote"))

(package! org-pretty-table              :recipe (:host github
                                                 :repo "Fuco1/org-pretty-table"))

(package! paperless)

(package! salt-mode)

(package! systemd)

(package! counsel-tramp)

(package! vlf                           :recipe (:host github
                                                 :repo "m00natic/vlfi"
                                                 :files ("*.el") ))

(package! wakatime-mode)

(package! tree-sitter
  :pin "4d9871d23999fe5f8de821e23c9ec576df2b2738")

(package! tree-sitter-langs
  :pin "fa47b55f7bd11bd2b17ab48deb03ed23000bb974")

(when (featurep! :editor evil +everywhere)
  (package! evil-textobj-tree-sitter
    :pin "eedc1f54611e4403ea228b33056388a8539a2b3e"))
