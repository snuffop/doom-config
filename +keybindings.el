;; $DOOMDIR/keybindings.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2022, Marty Buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; CODE:

(setq doom-localleader-key ",")

;;;; GLOBAL KEYBINDINGS

(define-key! help-map
  "h"    #'helpful-at-point)

(after! evil
  (map! :m  "-"  #'dired-jump
        :en "C-h"   #'evil-window-left
        :en "C-j"   #'evil-window-down
        :en "C-k"   #'evil-window-up
        :en "C-l"   #'evil-window-right))

(map!
 ;; :n "C-:"    #'+spell/correct
 :n "C-:"    #'flyspell-correct-wrapper
 :n "C-."    #'embark-act
 :n "C-,"    #'+spell/next-error)

;;;; LEADER KEYBINDINGS

(map! :leader
      "TAB"  #'evil-switch-to-windows-last-buffer
      "SPC"  #'execute-extended-command

      ;;; <leader> a --- Application
      (:prefix-map ("a" . "Application")
       "m"  #'=mu4e
       "b"  #'ebuku

       ;; ORG
       (:prefix-map ("o" . "org")
        "/" #'org-occur-in-agenda-files
        "a" #'org-agenda-list
        "t" #'org-todo-list
        "l" #'org-store-link
        "m" #'org-tags-view
        "o" #'org-agenda
        "s" #'org-search-view
        "t" #'org-todo-list
        (:prefix-map ("C" . "clock/contacts")
         "c"  #'org-clock-cancel
         "g"  #'org-clock-goto
         "i"  #'org-clock-in
         "j"  #'org-clock-jump-to-current-clock
         "o"  #'org-clock-out
         "r"  #'org-resolve-clocks
         "I"  #'org-clock-in-last
         (:prefix-map ("t" . "org-timer")
          "t"  #'org-timer-set-timer
          "p"  #'org-timer-pause-or-continue
          "q"  #'org-timer-stop)))

       ;; Tools
       (:when (featurep! :tools pass)
        (:prefix-map ("p" . "pass")
         "/"  #'+pass/consult
         "c"  #'password-store-edit
         "d"  #'password-store-remove
         "g"  #'password-store-generate
         "i"  #'password-store-insert
         "r"  #'password-store-rename
         "w"  #'password-store-url
         "y"  #'password-store-copy
         "D"  #'password-store-clear
         "I"  #'password-store-init
         (:prefix-map ("o" . "otp")
          "a" #'password-store-otp-append
          "i" #'password-store-otp-insert
          "y" #'password-store-otp-token-copy
          "A" #'password-store-otp-append-from-image
          "Y" #'password-store-otp-uri-copy)))
       (:prefix-map ("t" . "Tramp")
        "C"  #'tramp-cleanup-all-connections
        "B"  #'tramp-cleanup-all-buffers
        "c"  #'tramp-cleanup-this-connection
        "t"  #'counsel-tramp
        "q"  #'counsel-tramp-quit))

;;;;; <leader> l --- workspace / Layout
      (:when (featurep! :ui workspaces)
       (:prefix-map ("l" . "workspace")
        :desc "Display tab bar"           "SPC" #'+workspace/display
        :desc "Cycle tab bar"             "TAB" #'+workspace/cycle
        :desc "Switch workspace"          "."   #'+workspace/switch-to
        :desc "Switch to last workspace"  "`"   #'+workspace/other
        :desc "Move workspace left"       "<"   #'+workspace/swap-left
        :desc "Move workspace right"      ">"   #'+workspace/swap-right
        :desc "Burly open Bookmark "      "b"   #'burly-open-bookmark
        :desc "New workspace"             "n"   #'+workspace/new
        :desc "Create named workspace"    "N"   #'+workspace/new-named
        :desc "open workspace from file"  "o"   #'+workspace/load
        :desc "Save workspace to file"    "s"   #'+workspace/save
        :desc "Delete session"            "x"   #'+workspace/kill-session
        :desc "Delete this workspace"     "d"   #'+workspace/delete
        :desc "Rename workspace"          "r"   #'+workspace/rename
        :desc "Switch workspace"          "l"   #'+workspace/switch-to
        :desc "Restore last session"      "R"   #'+workspace/restore-last-session
        :desc "Next workspace"            "]"   #'+workspace/switch-right
        :desc "Previous workspace"        "["   #'+workspace/switch-left
        :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
        :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
        :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
        :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
        :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
        :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
        :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
        :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
        :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8
        :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final)))

;;;;; <leader n --- notes

(map! :leader
      :prefix "n"
      "b" #'marty/org-roam-capture-inbox
      "r" #'hydra-roam-jump/body)

;;;;; <leader> o --- open

(map! :leader
      :prefix "o"
      (:prefix-map ("m" . "MY")
       :desc "0mobile"       "0" #'(lambda () (interactive) (find-file (concat org-directory "0mobile.org")))
       :desc "Bullets"       "b" #'(lambda () (interactive) (find-file (concat org-directory "Joyent/Bullets.org")))
       :desc "Desktop"       "d" #'(lambda () (interactive) (find-file (concat org-directory "desktop.org")))
       :desc "contacts"      "o" #'(lambda () (interactive) (find-file (concat org-directory "contacts.org")))
       :desc "read-later"    "r" #'(lambda () (interactive) (find-file (concat org-directory "read-later.org")))
       :desc "Tasks"         "g" #'(lambda () (interactive) (find-file (concat org-directory "Tasks.org")))
       :desc "Habits"        "h" #'(lambda () (interactive) (find-file (concat org-directory "Habits.org")))
       :desc "read later"    "l" #'(lambda () (interactive) (find-file (concat org-directory "read-later.org")))
       :desc "Someday"       "s" #'(lambda () (interactive) (find-file (concat org-directory "Someday.org")))
       :desc "Tip Jar"       "t" #'(lambda () (interactive) (find-file (concat org-directory "TipJar.org")))
       (:prefix-map ("c" . "+config")
        :desc "keybindings"  "k"  #'(lambda () (interactive) (find-file (concat doom-private-dir "keybindings.el")))
        :desc "config"       "c"  #'(lambda () (interactive) (find-file (concat doom-private-dir "config.el")))
        :desc "org"          "o"  #'(lambda () (interactive) (find-file (concat doom-private-dir "org-mode.el")))
        :desc "init"         "i"  #'(lambda () (interactive) (find-file (concat doom-private-dir "init.el")))
        :desc "packages"     "p"  #'(lambda () (interactive) (find-file (concat doom-private-dir "packages.el")))
        :desc "mu4e"         "m"  #'(lambda () (interactive) (find-file (concat doom-private-dir "mu4e.el"))))
       (:prefix-map ("C" . "calendar")
        :desc "Calendar"      "C"  #'(lambda () (interactive) (find-file (concat org-directory "Calendar.org")))
        :desc "VdirSync"      "s"  #'khalel-run-vdirsyncer
        :desc "import"        "i"  #'khalel-import-upcoming-events)))

;;;;; <leader> R --- ROAM
(map! :leader
      :prefix "r"
      "r" #'hydra-roam-jump/body)


;;;; MODE MAPS
;;;;; OVERRIDE ORG MODE MAP

(map! :after org
      :map org-mode-map
      ;; Recently, a [tab] keybind in `outline-mode-cycle-map' has begun
      ;; overriding org's [tab] keybind in GUI Emacs. This is needed to undo
      ;; that, and should probably be PRed to org.
      [tab]        #'org-cycle

      "C-c C-S-l"  #'+org/remove-link
      "C-c C-i"    #'org-toggle-inline-images
      ;; textmate-esque newline insertion
      "S-RET"      #'+org/shift-return
      "C-RET"      #'+org/insert-item-below
      "C-S-RET"    #'+org/insert-item-above
      "C-M-RET"    #'org-insert-subheading
      [C-return]   #'+org/insert-item-below
      [C-S-return] #'+org/insert-item-above
      [C-M-return] #'org-insert-subheading
      (:when IS-MAC
       [s-return]   #'+org/insert-item-below
       [s-S-return] #'+org/insert-item-above
       [s-M-return] #'org-insert-subheading)
      ;; Org-aware C-a/C-e
      [remap doom/backward-to-bol-or-indent]          #'org-beginning-of-line
      [remap doom/forward-to-last-non-comment-or-eol] #'org-end-of-line

      :localleader
      "#" #'org-update-statistics-cookies
      "'" #'org-edit-special
      "*" #'org-ctrl-c-star
      "+" #'org-ctrl-c-minus
      "," #'org-switchb
      "." #'org-goto
      "@" #'org-cite-insert
      (:when (featurep! :completion ivy)
       "." #'counsel-org-goto
       "/" #'counsel-org-goto-all)
      (:when (featurep! :completion helm)
       "." #'helm-org-in-buffer-headings
       "/" #'helm-org-agenda-files-headings)
      (:when (featurep! :completion vertico)
       "." #'consult-org-heading
       "/" #'consult-org-agenda)
      "A" #'org-archive-subtree
      "e" #'org-export-dispatch
      "f" #'org-footnote-action
      "h" #'org-toggle-heading
      "i" #'org-toggle-item
      "I" #'org-id-get-create
      "m" #'hydra-roam-jump/body
      "n" #'org-store-link
      "o" #'org-set-property
      "q" #'org-set-tags-command
      "t" #'org-todo
      "T" #'org-todo-list
      "x" #'org-toggle-checkbox
      (:prefix ("a" . "attachments")
       "a" #'org-attach
       "d" #'org-attach-delete-one
       "D" #'org-attach-delete-all
       "f" #'+org/find-file-in-attachments
       "l" #'+org/attach-file-and-insert-link
       "n" #'org-attach-new
       "o" #'org-attach-open
       "O" #'org-attach-open-in-emacs
       "r" #'org-attach-reveal
       "R" #'org-attach-reveal-in-emacs
       "u" #'org-attach-url
       "s" #'org-attach-set-directory
       "S" #'org-attach-sync
       (:when (featurep! +dragndrop)
        "c" #'org-download-screenshot
        "p" #'org-download-clipboard
        "P" #'org-download-yank))
      (:prefix ("b" . "tables")
       "-" #'org-table-insert-hline
       "a" #'org-table-align
       "b" #'org-table-blank-field
       "c" #'org-table-create-or-convert-from-region
       "e" #'org-table-edit-field
       "f" #'org-table-edit-formulas
       "h" #'org-table-field-info
       "s" #'org-table-sort-lines
       "r" #'org-table-recalculate
       "R" #'org-table-recalculate-buffer-tables
       (:prefix ("d" . "delete")
        "c" #'org-table-delete-column
        "r" #'org-table-kill-row)
       (:prefix ("i" . "insert")
        "c" #'org-table-insert-column
        "h" #'org-table-insert-hline
        "r" #'org-table-insert-row
        "H" #'org-table-hline-and-move)
       (:prefix ("t" . "toggle")
        "f" #'org-table-toggle-formula-debugger
        "o" #'org-table-toggle-coordinate-overlays)
       (:when (featurep! +gnuplot)
        "p" #'org-plot/gnuplot))
      (:prefix ("c" . "clock")
       "c" #'org-clock-cancel
       "d" #'org-clock-mark-default-task
       "e" #'org-clock-modify-effort-estimate
       "E" #'org-set-effort
       "g" #'org-clock-goto
       "G" (cmd! (org-clock-goto 'select))
       "l" #'+org/toggle-last-clock
       "i" #'org-clock-in
       "I" #'org-clock-in-last
       "o" #'org-clock-out
       "r" #'org-resolve-clocks
       "R" #'org-clock-report
       "t" #'org-evaluate-time-range
       "=" #'org-clock-timestamps-up
       "-" #'org-clock-timestamps-down)
      (:prefix ("d" . "date/deadline")
       "d" #'org-deadline
       "s" #'org-schedule
       "t" #'org-time-stamp
       "T" #'org-time-stamp-inactive)
      (:prefix ("g" . "goto")
       "g" #'org-goto
       (:when (featurep! :completion ivy)
        "g" #'counsel-org-goto
        "G" #'counsel-org-goto-all)
       (:when (featurep! :completion helm)
        "g" #'helm-org-in-buffer-headings
        "G" #'helm-org-agenda-files-headings)
       (:when (featurep! :completion vertico)
        "g" #'consult-org-heading
        "G" #'consult-org-agenda)
       "c" #'org-clock-goto
       "C" (cmd! (org-clock-goto 'select))
       "i" #'org-id-goto
       "r" #'org-refile-goto-last-stored
       "v" #'+org/goto-visible
       "x" #'org-capture-goto-last-stored)
      (:prefix ("l" . "links")
       "c" #'org-cliplink
       "d" #'+org/remove-link
       "i" #'org-id-store-link
       "l" #'org-insert-link
       "L" #'org-insert-all-links
       "s" #'org-store-link
       "S" #'org-insert-last-stored-link
       "t" #'org-toggle-link-display)
      (:prefix ("P" . "publish")
       "a" #'org-publish-all
       "f" #'org-publish-current-file
       "p" #'org-publish
       "P" #'org-publish-current-project
       "s" #'org-publish-sitemap)
      (:prefix ("r" . "refile")
       "." #'+org/refile-to-current-file
       "c" #'+org/refile-to-running-clock
       "d" #'org-refile-to-datetree
       "l" #'+org/refile-to-last-location
       "f" #'+org/refile-to-file
       "o" #'+org/refile-to-other-window
       "O" #'+org/refile-to-other-buffer
       "v" #'+org/refile-to-visible
       "r" #'org-refile) ; to all `org-refile-targets'
      (:prefix ("s" . "tree/subtree")
       "a" #'org-toggle-archive-tag
       "b" #'org-tree-to-indirect-buffer
       "c" #'org-clone-subtree-with-time-shift
       "d" #'org-cut-subtree
       "h" #'org-promote-subtree
       "j" #'org-move-subtree-down
       "k" #'org-move-subtree-up
       "l" #'org-demote-subtree
       "n" #'org-narrow-to-subtree
       "r" #'org-refile
       "s" #'org-sparse-tree
       "A" #'org-archive-subtree
       "N" #'widen
       "S" #'org-sort)
      (:prefix ("p" . "priority")
       "d" #'org-priority-down
       "p" #'org-priority
       "u" #'org-priority-up))

(map! :after org-agenda
      :map org-agenda-mode-map
      :m "C-SPC" #'org-agenda-show-and-scroll-up
      :localleader
      (:prefix ("d" . "date/deadline")
       "d" #'org-agenda-deadline
       "s" #'org-agenda-schedule)
      (:prefix ("c" . "clock")
       "c" #'org-agenda-clock-cancel
       "g" #'org-agenda-clock-goto
       "i" #'org-agenda-clock-in
       "o" #'org-agenda-clock-out
       "r" #'org-agenda-clockreport-mode
       "s" #'org-agenda-show-clocking-issues)
      (:prefix ("p" . "priority")
       "d" #'org-agenda-priority-down
       "p" #'org-agenda-priority
       "u" #'org-agenda-priority-up)
      "q" #'org-agenda-set-tags
      "r" #'org-agenda-refile
      "t" #'org-agenda-todo)

;;;; DIRED KEYBINDINGS
;;;;;  TS FILE TRIGGER KEYBINDING
(define-key! dired-mode-map
  (kbd "C-t") #'marty/dired-copy-filename-as-tsfile-link)

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d"                   #'dired
       :desc "Dired jump to current" "j"        #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Create TSfile link" "d t"        #'marty/dired-copy-filename-as-tsfile-link
        :desc "Dired view file" "d v"           #'dired-view-file)))
;; Make 'h' and 'l' go back and forward in dired. Much faster to navigate the directory structure!

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-view-file
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-chmod
  (kbd "O") 'dired-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; If peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)


;;; END
:
