;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! aggressive-indent)
(package! async)
(package! autoinsert)
(package! counsel-tramp)
(package! ebuku)
(package! khardel)
(package! org-projectile)
(package! org-caldav)
(package! org-web-tools)
(package! paperless)
(package! salt-mode)
(package! systemd)
(package! vdirel)
(package! tldr)
(package! wakatime-mode)

(package! i3wm-config-mode  :recipe (:host github
                                     :repo "Alexander-Miller/i3wm-Config-Mode"))
(package! muttrc-mode       :recipe (:host github
                                     :repo "neomutt/muttrc-mode-el"))
(package! outshine          :recipe (:host github
                                     :repo "alphapapa/outshine"))
(package! vlf               :recipe (:host github
                                     :repo "m00natic/vlfi"
                                     :files ("*.el") ))
(package! org-pandoc-import :recipe (:host github
                                     :repo "tecosaur/org-pandoc-import"
                                     :files ("*.el" "filters" "preprocessors")))
