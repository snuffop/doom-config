;;; $DOOMDIR/keybindings.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2021, Marty Buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(setq doom-localleader-key ",")


;;;; Global keybindings

(define-key! help-map
  "h"    #'helpful-at-point)

(map!
 ;;:n "C-:"    #'+spell/correct
 :n "C-,"    #'+spell/next-error)

;;;;; Leader Keybindings

(map! :leader
      "TAB"  #'evil-switch-to-windows-last-buffer
      "SPC"  #'execute-extended-command
      ;;; <leader> a --- Application
      (:prefix-map ("a" . "Application")
       "m"  #'=mu4e
       "r"  #'ranger
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
       (:prefix-map ("t" . "tools")
        (:when (featurep! :tools pass)
         (:prefix-map ("p" . "pass")
          "/"  #'ivy-pass
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
         "q"  #'counsel-tramp-quit)))

      ;;; <leader> l --- workspace / Layout
      (:when (featurep! :ui workspaces)
       (:prefix-map ("l" . "workspace")
        :desc "Display tab bar"           "SPC" #'+workspace/display
        :desc "Cycle tab bar"             "TAB" #'+workspace/cycle
        :desc "Switch workspace"          "."   #'+workspace/switch-to
        :desc "Switch to last workspace"  "`"   #'+workspace/other
        :desc "Move workspace left"       "<"   #'+workspace/swap-left
        :desc "Move workspace right"      ">"   #'+workspace/swap-right
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
        :desc "Switch to final workspace" "0"   #'+workspace/switch-to-final))
      )

;;;;;; <leader n --- notes

(map! :leader
      :prefix "n"
      "b" #'marty/org-roam-capture-inbox)

;;;;;; <leader> o --- open

(map! :leader
      :prefix "o"
      (:prefix-map ("m" . "MY")
       :desc "0mobile"       "0" #'mb/0mobile
       :desc "Desktop"       "d" #'mb/desktop
       :desc "contacts"      "o" #'mb/contacts
       :desc "Tasks"         "g" #'mb/Tasks
       :desc "Habits"        "h" #'mb/Habits
       :desc "read later"    "l" #'mb/read-later
       :desc "Someday"       "s" #'mb/Someday
       :desc "Tip Jar"       "t" #'mb/TipJar
       (:prefix-map ("c" . "+config")
        :desc "keybindings"  "k"  #'mb/base-keybinding
        :desc "config"       "c"  #'mb/base-config
        :desc "org"          "o"  #'mb/org-config)
       (:prefix-map ("C" . "calendar")
        "c"  #' mb/open-calendar
        "C"  #' mb/calendar
        "s"  #' org-caldav-sync)))



;;;; Mode Maps
;;;;; Override org mode map

(map! :after org
      :map org-mode-map
      :localleader
      :prefix "m"
      "a"  #'marty/org-roam-move-todo-to-today
      "b"  #'marty/org-roam-capture-inbox
      "i"  #'org-roam-node-insert-immediate
      "j"  #'org-roam-dailies-capture-today
      "p"  #'marty/org-roam-find-project
      "s"  #'org-roam-db-sync
      :prefix "md"
      "p"  #'org-roam-dailies-goto-previous-note
      "n"  #'org-roam-dailies-goto-next-note
      )

 ;;;; Dired keybindings
 ;;;;;  TS file trigger keybinding
;; (define-key! dired-mode-map
;;   (kbd "C-t") #'marty/dired-copy-filename-as-tsfile-link)

 ;;;;; Map Leader D

;; (map! :leader
;;       (:prefix ("d" . "dired")
;;        :desc "Open dired" "d"                   #'dired
;;        :desc "Dired jump to current" "j"        #'dired-jump)
;;       (:after dired
;;        (:map dired-mode-map
;;         :desc "Peep-dired image previews" "d p" #'peep-dired
;;         :desc "Create TSfile link" "d t"        #'marty/dired-copy-filename-as-tsfile-link
;; :desc "Dired view file" "d v"           #'dired-view-file)))
;; Make 'h' and 'l' go back and forward in dired. Much faster to navigate the directory structure!
;;
;; (evil-define-key 'normal dired-mode-map
;;   (kbd "M-RET") 'dired-display-file
;;   (kbd "h") 'dired-up-directory
;;   (kbd "l") 'dired-view-file
;;   (kbd "m") 'dired-mark
;;   (kbd "t") 'dired-toggle-marks
;;   (kbd "u") 'dired-unmark
;;   (kbd "C") 'dired-do-copy
;;   (kbd "D") 'dired-do-delete
;;   (kbd "J") 'dired-goto-file
;;   (kbd "M") 'dired-chmod
;;   (kbd "O") 'dired-chown
;;   (kbd "P") 'dired-do-print
;;   (kbd "R") 'dired-rename
;;   (kbd "T") 'dired-do-touch
;;   (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
;;   (kbd "+") 'dired-create-directory
;;   (kbd "-") 'dired-up-directory
;;   (kbd "% l") 'dired-downcase
;;   (kbd "% u") 'dired-upcase
;;   (kbd "; d") 'epa-dired-do-decrypt
;;   (kbd "; e") 'epa-dired-do-encrypt)
;; ;; If peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
;; (evil-define-key 'normal peep-dired-mode-map
;;   (kbd "j") 'peep-dired-next-file
;;  (kbd "k") 'peep-dired-prev-file)
