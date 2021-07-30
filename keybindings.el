;;; $DOOMDIR/keybindings.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2021, Marty Buchaus, all rights reserved.
;; Created:  7 July 2021
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;; Code:

(define-key! dired-mode-map
  (kbd "C-t") #'marty/dired-copy-filename-as-tsfile-link)

(define-key! help-map
  "h"    #'helpful-at-point)

;;; Global keybindings





;;
;;; <leader>

(map! :leader

      ;;; <leader> a --- Application
      (:prefix-map ("a" . "Application")
       (:prefix-map ("t" . "Tramp")
        :desc "Cleanup All Connections"    "C"  #'tramp-cleanup-all-connections
        :desc "Cleanup All Buffers"        "B"  #'tramp-cleanup-all-buffers
        :desc "Cleanup This Connection"    "c"  #'tramp-cleanup-this-connection
        :desc "Counsel Tramp"              "t"  #'counsel-tramp
        :desc "Counsel Tramp Quit"         "q"  #'counsel-tramp-quit))


      ;;; <leader> l --- workspace / Layout
      (:when (featurep! :ui workspaces)
       (:prefix-map ("l" . "workspace")
        :desc "Display tab bar"           "SPC" #'+workspace/display
        :desc "Cycle tab bar"             "TAB" #'+workspace/cycle
        :desc "Switch workspace"          "."   #'+workspace/switch-to
        :desc "Switch to last workspace"  "`"   #'+workspace/other
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

;;       ;;; <leader> n --- notes
;;       (:prefix-map ("n" . "notes")
;;        :desc "Search notes for symbol"      "*" #'+default/search-notes-for-symbol-at-point
;;        :desc "Org agenda"                   "a" #'org-agenda
;;        (:when (featurep! :tools biblio)
;;         :desc "Bibliographic entries"        "b"
;;         (cond ((featurep! :completion ivy)   #'ivy-bibtex)
;;               ((featurep! :completion helm)  #'helm-bibtex)))

;;        :desc "Toggle last org-clock"        "c" #'+org/toggle-last-clock
;;        :desc "Cancel current org-clock"     "C" #'org-clock-cancel
;;        :desc "Open deft"                    "d" #'deft
;;        (:when (featurep! :lang org +noter)
;;         :desc "Org noter"                  "e" #'org-noter)

;;        :desc "Find file in notes"           "f" #'+default/find-in-notes
;;        :desc "Browse notes"                 "F" #'+default/browse-notes
;;        :desc "Org store link"               "l" #'org-store-link
;;        :desc "Tags search"                  "m" #'org-tags-view
;;        :desc "Org capture"                  "n" #'org-capture
;;        :desc "Goto capture"                 "N" #'org-capture-goto-target
;;        :desc "Active org-clock"             "o" #'org-clock-goto
;;        :desc "Todo list"                    "t" #'org-todo-list
;;        :desc "Search notes"                 "s" #'+default/org-notes-search
;;        :desc "Search org agenda headlines"  "S" #'+default/org-notes-headlines
;;        :desc "View search"                  "v" #'org-search-view
;;        :desc "Org export to clipboard"        "y" #'+org/export-to-clipboard
;;        :desc "Org export to clipboard as RTF" "Y" #'+org/export-to-clipboard-as-rich-text

;;        (:when (featurep! :lang org +roam)
;;         (:prefix ("r" . "roam")
;;          :desc "Switch to buffer"              "b" #'org-roam-switch-to-buffer
;;          :desc "Org Roam Capture"              "j" #'org-roam-capture
;;          :desc "Find file"                     "f" #'org-roam-find-file
;;          :desc "Show graph"                    "g" #'org-roam-graph
;;          :desc "Insert"                        "i" #'org-roam-insert
;;          :desc "Insert (skipping org-capture)" "I" #'org-roam-insert-immediate
;;          :desc "Org Roam"                      "r" #'org-roam
;;          :desc "dailies next"                  "n" #'org-roam-dailies-find-next-note
;;          :desc "dailies prev"                  "p" #'org-roam-dailies-find-previous-note
;;          (:prefix ("d" . "by date")
;;           :desc "Arbitrary date" "d" #'org-roam-dailies-find-date
;;           :desc "Today"          "t" #'org-roam-dailies-find-today
;;           :desc "Tomorrow"       "m" #'org-roam-dailies-find-tomorrow
;;           :desc "Yesterday"      "y" #'org-roam-dailies-find-yesterday)))

;;        (:when (featurep! :lang org +journal)
;;         (:prefix ("j" . "journal")
;;          :desc "New Entry"           "j" #'org-journal-new-entry
;;          :desc "New Scheduled Entry" "J" #'org-journal-new-scheduled-entry
;;          :desc "Search Forever"      "s" #'org-journal-search-forever)))

;;       ;;; <leader> o --- open
;;       (:prefix-map ("o" . "open")
;;        :desc "Org agenda"       "A"  #'org-agenda
;;        (:prefix ("a" . "org agenda")
;;         :desc "Agenda"         "a"  #'org-agenda
;;         :desc "Todo list"      "t"  #'org-todo-list
;;         :desc "Tags search"    "m"  #'org-tags-view
;;         :desc "View search"    "v"  #'org-search-view)
;;        :desc "Default browser"    "b"  #'browse-url-of-file
;;        :desc "Start debugger"     "d"  #'+debugger/start
;;        :desc "New frame"          "f"  #'make-frame
;;        :desc "Select frame"       "F"  #'select-frame-by-name
;;        :desc "REPL"               "r"  #'+eval/open-repl-other-window
;;        :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
;;        :desc "Dired"              "-"  #'dired-jump
;;        (:when (featurep! :ui neotree)
;;         :desc "Project sidebar"              "p" #'+neotree/open
;;         :desc "Find file in project sidebar" "P" #'+neotree/find-this-file)
;;        (:when (featurep! :ui treemacs)
;;         :desc "Project sidebar" "p" #'+treemacs/toggle
;;         :desc "Find file in project sidebar" "P" #'treemacs-find-file)
;;        (:when (featurep! :term shell)
;;         :desc "Toggle shell popup"    "t" #'+shell/toggle
;;         :desc "Open shell here"       "T" #'+shell/here)
;;        (:when (featurep! :term term)
;;         :desc "Toggle terminal popup" "t" #'+term/toggle
;;         :desc "Open terminal here"    "T" #'+term/here)
;;        (:when (featurep! :term vterm)
;;         :desc "Toggle vterm popup"    "t" #'+vterm/toggle
;;         :desc "Open vterm here"       "T" #'+vterm/here)
;;        (:when (featurep! :term eshell)
;;         :desc "Toggle eshell popup"   "e" #'+eshell/toggle
;;         :desc "Open eshell here"      "E" #'+eshell/here)
;;        (:when (featurep! :os macos)
;;         :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
;;         :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder
;;         :desc "Send to Transmit"           "u" #'+macos/send-to-transmit
;;         :desc "Send project to Transmit"   "U" #'+macos/send-project-to-transmit
;;         :desc "Send to Launchbar"          "l" #'+macos/send-to-launchbar
;;         :desc "Send project to Launchbar"  "L" #'+macos/send-project-to-launchbar
;;         :desc "Open in iTerm"              "i" #'+macos/open-in-iterm)
;;        (:when (featurep! :tools docker)
;;         :desc "Docker" "D" #'docker)
;;        (:when (featurep! :email mu4e)
;;         :desc "mu4e" "e" #'=mu4e)
;;        (:prefix-map ("m" . "MY")
;;         :desc "0mobile"       "0" #'mb/0mobile
;;         :desc "Desktop"       "d" #'mb/desktop
;;         :desc "contacts"      "c" #'mb/contacts
;;         :desc "Tasks"         "g" #'mb/Tasks
;;         :desc "Habits"        "h" #'mb/Habits
;;         :desc "read later"    "l" #'mb/read-later
;;         :desc "Someday"       "s" #'mb/Someday
;;         :desc "Tip Jar"       "t" #'mb/TipJar
;;         (:prefix-map ("c" . "+config")
;;          :desc "keybindings"  "k"  #'mb/base-keybinding
;;          :desc "config"       "c"  #'mb/base-config
;;          :desc "org"          "o"  #'mb/org-config
;;          :desc "functions"    "f"  #'mb/functions
;;          :desc "packages"     "p"  #'mb/packages
;;          )))



;;;;;
;;;;; Override org mode map
;;;;;

;; (map! :after org
;;       :map org-mode-map
;;       :localleader
;;       (:prefix-map ("r" . "org-roam")
;;        "b" #'org-roam-switch-to-buffer
;;        "f" #'org-roam-find-file
;;        "g" #'org-roam-graph
;;        "i" #'org-roam-insert
;;        "I" #'org-roam-insert-immediate
;;        "j" #'org-roam-dailies-capture-today
;;        "r" #'org-roam
;;        "n" #'org-roam-dailies-find-next-note
;;        "p" #'org-roam-dailies-find-previous-note
;;        "t" #'org-roam-tag-add
;;        "T" #'org-roam-tag-delete
;;        (:prefix ("d" . "by date")
;;         :desc "Find previous note" "b" #'org-roam-dailies-find-previous-note
;;         :desc "Find date"          "d" #'org-roam-dailies-find-date
;;         :desc "Find next note"     "f" #'org-roam-dailies-find-next-note
;;         :desc "Find tomorrow"      "m" #'org-roam-dailies-find-tomorrow
;;         :desc "Capture today"      "n" #'org-roam-dailies-capture-today
;;         :desc "Find today"         "t" #'org-roam-dailies-find-today
;;         :desc "Capture Date"       "v" #'org-roam-dailies-capture-date
;;         :desc "Find yesterday"     "y" #'org-roam-dailies-find-yesterday
;;         :desc "Find directory"     "." #'org-roam-dailies-find-directory))
;;       (:prefix-map ("s" . "tree/subtree")
;;        "a" #'org-toggle-archive-tag
;;        "b" #'org-tree-to-indirect-buffer
;;        "d" #'org-cut-subtree
;;        "h" #'org-promote-subtree
;;        "j" #'org-move-subtree-down
;;        "k" #'org-move-subtree-up
;;        "l" #'org-demote-subtree
;;        "n" #'org-narrow-to-subtree
;;        (:prefix ("r" . "refile")
;;         "." #'+org/refile-to-current-file
;;         "c" #'+org/refile-to-running-clock
;;         "l" #'+org/refile-to-last-location
;;         "f" #'+org/refile-to-file
;;         "o" #'+org/refile-to-other-window
;;         "O" #'+org/refile-to-other-buffer
;;         "v" #'+org/refile-to-visible
;;         "r" #'org-refile) ; to all `org-refile-targets'
;;        "s" #'org-sparse-tree
;;        "A" #'org-archive-subtree
;;        "N" #'widen
;;        "S" #'org-sort))
