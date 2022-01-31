;;; hydra.el -*- lexical-binding: t; -*-
;;
;; author: marty buchaus <marty@dabuke.com>
;; copyright © 2022, marty buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust face)
    "Display an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-faicon (icon str &optional height v-adjust face)
    "Display an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon ':v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-fileicon (icon str &optional height v-adjust face)
    "Display an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-octicon (icon str &optional height v-adjust face)
    "Display an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str)))

;;;; HYDRA MAGIT

(pretty-hydra-define hydra-magit
  (:hint nil :color teal :quit-key "q" :title (with-octicon "mark-github" "Magit" 1 -0.05))
  ("Action"
   (("b" magit-blame "blame")
    ("c" magit-clone "clone")
    ("i" magit-init "init")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("s" magit-status "status"))
   "Other"
   (("t" git-timemachine "TimeMachine"))
   ))

;;;; HYDRA BTOGGLE

(pretty-hydra-define hydra-btoggle
  (:hint nil :color amaranth :quit-key "q" :title (with-faicon "toggle-on" "Toggle" 1 -0.05))
  ("Basic"
   (("a" abbrev-mode "abbrev" :toggle t)
    ("h" global-hungry-delete-mode "hungry delete" :toggle t)
    ("t" tab-bar-mode "Tab Bar" :toggle t )
    ("n" display-line-numbers-mode "Line Numbers" :toggle t))
   "Coding"
   (("F" flyspell-mode "flyspell" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t)
    ("l" lsp-mode "lsp" :toggle t)
    ("s" smartparens-mode "smartparens" :toggle t)
    ("d" toggle-debug-on-error "Debug on Error" :toggle t))
   "Org"
   (("A" org-appear-mode "Org Appear" :toggle t))))

;;;; HYDRA FLYCHECK

(pretty-hydra-define hydra-flycheck
  (:hint nil :color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
  ("Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "mode/enable")
    ("s" flycheck-select-checker "select"))
   "Errors"
   (("<" flycheck-previous-error "previous" :color pink)
    (">" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))

;;;; HYDRA OPEN PROJECT

(pretty-hydra-define hydra-open-project
  (:hint nil :color teal :quit-key "q" :title (with-octicon "database" "Project to Open" 1 -0.05))
  ("Projects"
   (("n"  #'marty/new-tab-bar-Notes "Notes")
    ("c"  #'marty/new-tab-bar-chezmoi "Chezmoi")
    ("e"  #'marty/new-tab-bar-MyEmacs "MyEmacs")
    ("d"  #'marty/new-tab-bar-doom "doom-config")
    ("s"  #'marty/new-tab-bar-spacemacs "spacemacs-config")
    ("D"  #'marty/new-tab-bar-Doom "Doom")
    ("S"  #'marty/new-tab-bar-Spacemacs "Spacemacs")
    )
   "Applications"
   (("m"  #'marty/new-tab-bar-MU4E "MU4E")
    ("b"  #'marty/new-tab-bar-ebuku "BUKU"))))

;;;; HYDRA ROAM JUMP

(pretty-hydra-define hydra-roam-jump
  (:hint nil :color teal :quit-key "q" :title (with-octicon "steps" "Roam Jump" 1 -0.05))
  ("Roam"
   (
    ("B"   #'org-roam-demote-entire-buffer "Demote Buffer")
    ("I"   #'org-roam-node-insert "insert (orig)")
    ("R"   #'marty/org-roam-rg-search "ripgrep search")
    ("a"   #'marty/org-roam-move-todo-to-today "archive to daily")
    ("b"   #'org-roam-buffer "Show Buffer")
    ("f"   #'org-roam-ref-find "Find Reference")
    ("g"   #'org-roam-graph "roam-graph")
    ("i"   #'org-roam-node-insert-immediate "insert immediate")
    ("j"   #'org-roam-dailies-capture-today  "capture today")
    ("m"   #'org-roam-buffer-toggle "buffer toggle")
    ("M"   #'org-roam-buffer-display-dedicated "buffer dedicated")
    ("n"   #'org-roam-node-find "Find Node")
    ("r"   #'org-roam-refile "roam refile")
    ("R"   #'org-roam-link-replace-all "link replace all")
    ("s"   #'org-roam-db-sync "sync DB")
    )
   "Dailies"
   (
    ("d-"  #'org-roam-dailies-find-directory "Find Directory")
    ("dT"  #'org-roam-dailies-goto-tomorrow "tomorrow")
    ("dd"  #'org-roam-dailies-goto-date "date")
    ("dn"  #'org-roam-dailies-goto-next-note "next-note")
    ("dp"  #'org-roam-dailies-goto-previous-note "previous-note")
    ("dt"  #'org-roam-dailies-goto-today "today")
    ("dy"  #'org-roam-dailies-goto-yesterday "yesterday")
    )
   "Capture"
   (
    ("cT"  #'org-roam-dailies-capture-tomorrow "capture tomorrow")
    ("cb"  #'marty/org-roam-capture-inbox "capture to inbox")
    ("cc"  #'org-roam-capture "capture")
    ("cd"  #'org-roam-dailies-capture-date "capture by date")
    ("ct"  #'org-roam-dailies-capture-today "capture today")
    ("cy"  #'org-roam-dailies-capture-yesterday "capture yesterday")
    )
   "Database"
   (
    ("DD"  #'org-roam-db-diagnose-node "Diagnose")
    ("Dc"  #'org-roam-db-clear-all "Clear All")
    ("Dt"  #'org-roam-db-autosync-toggle "Auto Sync Toggle")
    )
   "Object"
   (
    ("oA"  #'org-roam-alias-remove "remove alias")
    ("oR"  #'org-roam-ref-remove "remove reference")
    ("oT"  #'org-roam-tag-remove "remove tag")
    ("oa"  #'org-roam-alias-add "add alias")
    ("or"  #'org-roam-ref-add "add reference")
    ("ot"  #'org-roam-tag-add "add tag")
    )
   "Jump"
   (("Jm" #'dw/org-roam-goto-month "Month")
    ("Je" #'dw/org-roam-goto-year "Year"))))

;;;; HYDRA TAB BAR

(pretty-hydra-define hydra-layout
  (:hint nil :color teal :quit-key "q" :title (with-octicon "credit-card" "Layout" 1 -0.05))
  ("Switch Tabs"
   (("TAB" #'tab-bar-switch-to-recent-tab "Recent")
    ("n"   #'tab-bar-switch-to-next-tab   "Next Tab")
    ("p"   #'tab-bar-switch-to-prev-tab   "Prev Tab")
    ("l"   #'tab-bar-select-tab-by-name   "Select by Name"))
   "Select by Number"
   (("1" #'my-tab-bar-select-tab-1 "Switch to Tab 1")
    ("2" #'my-tab-bar-select-tab-2 "Switch to Tab 2")
    ("3" #'my-tab-bar-select-tab-3 "Switch to Tab 3")
    ("4" #'my-tab-bar-select-tab-4 "Switch to Tab 4")
    ("5" #'my-tab-bar-select-tab-5 "Switch to Tab 5")
    ("6" #'my-tab-bar-select-tab-6 "Switch to Tab 6")
    ("7" #'my-tab-bar-select-tab-7 "Switch to Tab 7")
    ("8" #'my-tab-bar-select-tab-8 "Switch to Tab 8")
    ("9" #'my-tab-bar-select-tab-9 "Switch to Tab 9")
    ("0" #'my-tab-bar-select-tab-9 "Switch to Tab 10"))
   "Open"
   (("c" #'my-tab-bar-create       "Create New Tab")
    ("C" #'my-tab-bar-clone        "Clone Tab")
    ("o" #'hydra-open-project/body "Project/Application"))
   "Action"
   (("d"   #'tab-bar-close-tab        "Delete Tab")
    ("D"   #'tab-bar-close-other-tabs "Delete Other")
    ("r"   #'tab-bar-rename-tab       "Rename")
    ("u"   #'tab-bar-undo-close-tab   "undo close Tab"))))


;;;; HYDRA GO TO FILE

(pretty-hydra-define hydra-go-to-file
  (:hint nil :color teal :quit-key "q" :title (with-octicon "file-symlink-file" "Go To" 1 -0.05))
  ("Agenda"
   (("ac" (find-file "~/Nextcloud/Notes/org/contacts.org") "contacts")
    ("at" (find-file "~/Nextcloud/Notes/org/Tasks.org") "Tasks")
    ("a0" (find-file "~/Nextcloud/Notes/org/0mobile.org") "0mobile")
    ("ap" (find-file "~/Nextcloud/Notes/org/people/index.org") "people")
    ("ah" (find-file "~/Nextcloud/Notes/org/Habits.org") "Habits"))
   "Notes"
   (("na" (find-file (format "~/.personal/notes/affirmations.pdf" xdg-config)) "Affirmations"))
   "Other"
   (("ob" (find-file "~/.personal/other/books.org") "book")
    ("ol" (find-file "~/.personal/other/long-goals.org") "long-terms goals")
    ("om" (find-file "~/.personal/other/movies.org"))
    ("op" (find-file "~/.personal/other/purchases.org") "purchase")
    ("os" (find-file "~/.personal/other/short-goals.org") "short-terms goals")
    ("ou" (find-file "~/.personal/other/usb.org") "usb")
    ("oL" (find-file "~/.personal/other/learning.org") "learning"))))

;;;; HYDRA ORG CLOCK

(pretty-hydra-define hydra-clock
  (:hint nil :color teal :quit-key "q" :title (with-faicon "clock-o" "Org Clock" 1 -0.05))
  ("Action"
   (("c" org-clock-cancel "cancel")
    ("d" org-clock-display "display")
    ("e" org-clock-modify-effort-estimate "effort")
    ("i" org-clock-in "in")
    ("j" org-clock-goto "jump")
    ("o" org-clock-out "out")
    ("p" org-pomodoro "pomodoro")
    ("r" org-clock-report "report"))))

;;;; HYDRA SPELLING

(pretty-hydra-define hydra-spelling
  (:hint nil :color teal :quit-key "q" :title (with-faicon "magic" "Spelling" 1 -0.05))
  ("Checker"
   (
    ("c" langtool-correct-buffer "correction")
    ("C" langtool-check-done "clear")
    ("d" ispell-change-dictionary "dictionary")
    ("l" (message "Current language: %s (%s)" langtool-default-language ispell-current-dictionary) "language")
    ("s" my/switch-language "switch")
    ("w" wiki-summary "wiki"))
   "Errors"
   (("<" flyspell-correct-previous "previous" :color pink)
    (">" flyspell-correct-next "next" :color pink)
    ("f" langtool-check "find"))))


;;;; HYDRA WINDOW

(pretty-hydra-define hydra-windows
  (:hint nil :forein-keys warn :quit-key "q" :title (with-faicon "windows" "Windows" 1 -0.05))
  ("Window"
   (("b" balance-windows "balance")
    ("i" enlarge-window "heighten")
    ("j" shrink-window-horizontally "narrow")
    ("k" shrink-window "lower")
    ("u" winner-undo "undo")
    ("r" winner-redo "redo")
    ("l" enlarge-window-horizontally "widen")
    ("L" #'spacemacs/window-layout-toggle "layout")
    ("s" switch-window-then-swap-buffer "swap" :color teal))
   "Zoom"
   (("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("=" (text-scale-increase 0) "reset"))))
;;;; HYDRA TRAMP To HOST

(pretty-hydra-define hydra-tramp-hosts
  (:hint nil :forein-keys warn :quit-key "q" :title (with-octicon "link-external" "External Hosts" 1 -0.05))
  ("Session"
   (("m" :placeholder  ))))

;;;; HYDRA STRAIGHT
(pretty-hydra-define hydra-straight
  (:hint nil :forein-keys warn :quit-key "q" :title (with-octicon "package" "Straight" 1 -0.05))
  ("Check"
   (("c" straight-check-all)
    ("C" straight-check-package))
   "Rebuild"
   (("r" straight-rebuild-all)
    ("R" straight-rebuild-package))
   "Fetch"
   (("f" straight-fetch-all)
    ("F" straight-fetch-package))
   "Pull"
   (("p" straight-pull-all)
    ("P" straight-pull-package))
   "Merge"
   (("m" straight-merge-all)
    ("M" straight-merge-package))
   "Normalize"
   (("n" straight-normalize-all)
    ("N" straight-normalize-package))
   "Push"
   (("u" straight-push-all)
    ("U" straight-push-package))
   "Misc"
   (("v" straight-freeze-versions)
    ("V" straight-thaw-versions)
    ("w" straight-watcher-start)
    ("W" straight-watcher-quit)
    ("g" straight-get-recipe)
    ("e" straight-prune-build))))

;;; END
