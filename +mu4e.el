;;; mu4e.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Marty Buchaus <marty@dabuke.com>
;; Copyright © 2022, Marty Buchaus, all rights reserved.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;;;; Config
(after! mu4e
;;;;; Header Actions

  (add-to-list 'mu4e-headers-actions
               '("follow up" . marty/capture-mail-follow-up) t)
  (add-to-list 'mu4e-headers-actions
               '("read later" . marty/capture-mail-read-later) t)
  (add-to-list 'mu4e-view-actions '("ytag message" . mu4e-action-retag-message) t)

;;;;; Header

  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

  (setq mu4e-use-fancy-chars t)

;;;;; Info

  (add-to-list 'mu4e-header-info-custom
               '(:full-mailing-list .
                 ( :name "Mailing-list"                     ;; long name, as seen in the message-view
                   :shortname "Mail List"                    ;; short name, as seen in the headers view
                   :help "Full name for mailing list" ;; tooltip
                   :function (lambda (msg)
                               (or (mu4e-message-field msg :mailing-list) "")))))

  (add-to-list 'mu4e-header-info-custom
               '(:xlabel .
                 ( :name "X-Label or Tag"                 ;; long name, as seen in the message-view
                   :shortname "X-Label"        ;; short name, as seen in the headers view
                   :help "Maildir X-Label"   ;; tooltip
                   :function (lambda (msg)
                               (or (mu4e-message-field msg :X-Label) "")))))

  (defvar +mu4e-header--folder-colors nil)

  (appendq! mu4e-header-info-custom
            '((:folder .
               (:name "Folder" :shortname "Folder" :help "Lowest level folder" :function
                (lambda (msg)
                  (+mu4e-colorize-str
                   (replace-regexp-in-string "\\`.*/" "" (mu4e-message-field msg :maildir))
                   '+mu4e-header--folder-colors))))))

;;;;; fields

  (setq mu4e-headers-fields '((:account-stripe    . 1)
                              (:human-date        . 10)
                              (:flags             . 6)
                              (:size              . 10)
                              (:from-or-to        . 40)
                              (:full-mailing-list . 40)
                              (:tags              . 15)     ;;  X-label
                              (:folder            . 20)
                              (:subject           . nil)))  ;;  :subject or thread-subject

  (setq mu4e-view-fields '(:date
                           :from
                           :to
                           :cc
                           :bcc
                           :subject
                           :flags
                           :maildir
                           :full-mailing-list
                           :size
                           :signature
                           :xlabel
                           :tags
                           :decryption
                           :attachments))

;;;;; (config)

  (setq mu4e-action-tags-header "X-Label")
  (setq mu4e-attachment-dir "/home/marty/Downloads/Mail")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-get-mail-command "mbsync -c ~/.mbsyncrc -a")
  (setq mu4e-headers-date-format "%m/%d/%y")
  (setq mu4e-headers-time-format "⧖ %H:%M")
  (setq mu4e-headers-results-limit 1000)
  (setq mu4e-index-cleanup t)
  (setq mu4e-index-lazy-check t)
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-alert-icon "/usr/share/icons/Papirus/64x64/apps/evolution.svg")

;;;;;; Set from Context  these are default

  (setq mu4e-drafts-folder nil)                      ;; set from context
  (setq mu4e-get-mail-command nil)                   ;; set from context
  (setq mu4e-sent-folder nil)                        ;; set from context
  (setq mu4e-trash-folder nil)                       ;; set from context

;;;;;; PGP

  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)

;;;;;; Send Mail

  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-sendmail-extra-arguments '("--read-envelope-from"));, "--read-recipients")
  (setq message-sendmail-f-is-evil t)
  (setq send-mail-function #'smtpmail-send-it)
  (setq sendmail-program "/usr/bin/msmtp")

;;;;;; VIEW Email

  (setq mu4e-headers-include-related t)
  (setq mu4e-headers-precise-alignment t)

;;;;;; Compose

  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-signature nil)        ;; Pulled from Contexts so Null as default
  (setq mm-sign-option 'guided)

;;;;;; compose mode hook

  (add-hook 'mu4e-compose-mode-hook
            #'(lambda ()
                "My Setting for Composing Messages"
                (save-excursion (message-add-header "X-Mailer: mu4e/Linux"))
                (save-excursion (message-add-header "X-PGP-KEY-Fingerprint: CBE2 3C6D 1E97 5793 2C4F 91D8 3B77 E2C6 70F4 ACD2"))
                (save-excursion (message-add-header "X-PGP-Key-ID: 0x3B77E2C670F4ACD2"))
                (save-excursion (message-add-header "X-PGP-Key: https://keybase.io/mbuchaus/key.asc "))
                (marty-mu4e/mu4e-compose-maybe-signed-and-crypted)
                (set-fill-column 72)
                (turn-on-auto-fill)))


  (setq mu4e-compose-hidden-headers '("^Face:"
                                      "^X-Face:"
                                      "^Openpgp:"
                                      "^X-Draft-From:"
                                      "^X-Mailer:"
                                      "^User-agent:"))

;;;;;; Encryption

  (setq epg-user-id "0x3B77E2C670F4ACD2")
  (setq mu4e-decryption-policy t)
  (setq mu4e-compose-crypto-reply-plain-policy 'sign)
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender  t)

;;;;; bookmarks

  (setq mu4e-bookmarks
        '(
          (:name "All Inboxes"
           :query "maildir:/Dabuke/INBOX OR maildir:/Google/INBOX OR maildir:/RHH/INBOX OR maildir:/RedEarth/INBOX OR maildir:/OFMasons/INBOX OR maildir:/letEarnMoney/INBOX"
           :key ?i)
          (:name "Unread messages"
           :query "flag:unread AND NOT flag:trashed AND NOT maildir:/Google/Spam AND NOT maildir:/Dabuke/Spam"
           :key ?u)
          (:name "Unread Dabuke"
           :query "flag:unread AND NOT flag:trashed AND maildir:/Dabuke/"
           :key ?d)
          (:name "Today's messages"
           :query "date:today..now AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam"
           :key ?t)
          (:name "Yesterday and today messages"
           :query "date:1d..now AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam"
           :key ?y)
          (:name "Last 7 days"
           :query "date:7d..now AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam"
           :key ?w)
          (:name "Messages with images last 30 days"
           :query "date:30d..now mime:image/*"
           :key ?p)
          (:name "Messages with images All"
           :query "mime:image/*"
           :key ?P)
          (:name "Messages with attachments last 30 days"
           :query "date:30d..now flag:attach"
           :key ?a)
          (:name "Messages with attachments All"
           :query "flag:attach"
           :key ?A)
          ))
;;;;; Contexts

  (setq mu4e-context-policy 'always-ask)
  (setq mu4e-contexts

        `(
;;;;;; Dabuke
          ,(make-mu4e-context
            :name "Dabuke"
            :enter-func (lambda () (mu4e-message "Switch to the Dabuke context"))
            :leave-func (lambda () (mu4e-message "Leaving Dabuke context"))
            :vars '((user-mail-address      . "marty@dabuke.com")
                    (mu4e-get-mail-command  . "mbsync Dabuke")
                    (mu4e-refile-folder     . "/Dabuke/Archive")
                    (mu4e-trash-folder      . "/Dabuke/Trash")
                    (mu4e-drafts-folder     . "/Dabuke/Drafts")
                    (mu4e-sent-folder       . "/Dabuke/Sent")
                    (user-full-name         . "Marty Buchaus")
                    (mu4e-maildir-shortcuts . ((:maildir "/Dabuke/Archive"              :key ?a)
                                               (:maildir "/Dabuke/Drafts"               :key ?d)
                                               (:maildir "/Dabuke/INBOX"                :key ?i)
                                               (:maildir "/Dabuke/Spam"                 :key ?S)
                                               (:maildir "/Dabuke/Lists.CraigsList"     :key ?c)
                                               (:maildir "/Dabuke/Lists.Doom"           :key ?D)
                                               (:maildir "/Dabuke/Lists.Emacs"          :key ?e)
                                               (:maildir "/Dabuke/Lists.Github"         :key ?g)
                                               (:maildir "/Dabuke/Lists.Joyent"         :key ?J)
                                               (:maildir "/Dabuke/Lists.Linode"         :key ?l)
                                               (:maildir "/Dabuke/Lists.Mutt"           :key ?M)
                                               (:maildir "/Dabuke/Lists.PHP"            :key ?P)
                                               (:maildir "/Dabuke/Lists.Root"           :key ?R)
                                               (:maildir "/Dabuke/Lists.nextcloud"      :key ?N)
                                               (:maildir "/Dabuke/Lists.ofmasons"       :key ?O)
                                               (:maildir "/Dabuke/Lists.passwordstore"  :key ?W)
                                               (:maildir "/Dabuke/Lists.qutebrowser"    :key ?q)
                                               (:maildir "/Dabuke/Lists.ranger"         :key ?r)
                                               (:maildir "/Dabuke/Sent"                 :key ?s)
                                               (:maildir "/Dabuke/Trash"                :key ?T)))
                    (message-sendmail-extra-arguments . ("--account=Dabuke"))
                    (mu4e-compose-signature .
                                            (concat
                                             "William Marty Buchaus Jr\n"
                                             "A person is smart. People are dumb, panicky, dangerous animals and you know it. -k MIB\n"
                                             "Meet on the level Act by the Plumb and Part upon the Square  AF&AM 832\n"
                                             "https://snuffy.org\n"))))

;;;;;; Joyent
          ,(make-mu4e-context
            :name "Joyent"
            :enter-func (lambda () (mu4e-message "Switch to the Joyent context"))
            :leave-func (lambda () (mu4e-message "Leaving Joyent context"))
            :vars '((user-mail-address       . "marty.buchaus@joyent.com")
                    (mu4e-get-mail-command   . "mbsync Joyent")
                    (mu4e-trash-folder       . "/Joyent/Trash")
                    (mu4e-drafts-folder      . "/Joyent/Drafts")
                    (mu4e-sent-folder        . "/Joyent/Sent")
                    (mu4e-refile-folder      . "/Joyent/Archive")
                    (user-full-name          . "Marty Buchaus")
                    (mu4e-maildir-shortcuts  . ((:maildir "/Joyent/Archive"           :key ?a)
                                                (:maildir "/Joyent/Drafts"            :key ?d)
                                                (:maildir "/Joyent/INBOX"             :key ?i)
                                                (:maildir "/Joyent/sysops"            :key ?y)
                                                (:maildir "/Joyent/Nagios"            :key ?n)
                                                (:maildir "/Joyent/FollowUp"          :key ?f)
                                                (:maildir "/Joyent/Sent"              :key ?s)
                                                (:maildir "/Joyent/Spam"              :key ?S)
                                                (:maildir "/Joyent/Trash"             :key ?T)))
                    (message-sendmail-extra-arguments . ("--account=Joyent"))
                    (mu4e-compose-signature .
                                            (concat
                                             "Marty Buchaus\n"
                                             "Meet on the Level Act by the Plumb and Part upon the Square\n"))))

;;;;;; Lets Earn Money
          ,(make-mu4e-context
            :name "letsEarnMoney"
            :enter-func (lambda () (mu4e-message "Switch to the letsEarnMoney context"))
            :leave-func (lambda () (mu4e-message "Leaving letsEarnMoney context"))
            :vars '((user-mail-address      . "marty@letsearnmoney.com")
                    (mu4e-get-mail-command  . "mbsync letsEarnMoney")
                    (mu4e-refile-folder     . "/letsEarnMoney/Archive")
                    (mu4e-trash-folder      . "/letsEarnMoney/Trash")
                    (mu4e-drafts-folder     . "/letsEarnMoney/Drafts")
                    (mu4e-sent-folder       . "/letsEarnMoney/Sent")
                    (user-full-name         . "Marty Buchaus")
                    (mu4e-maildir-shortcuts . ((:maildir "/letsEarnMoney/Archive"              :key ?a)
                                               (:maildir "/letsEarnMoney/Drafts"               :key ?d)
                                               (:maildir "/letsEarnMoney/INBOX"                :key ?i)
                                               (:maildir "/letsEarnMoney/Junk"                 :key ?j)
                                               (:maildir "/letsEarnMoney/Queue"                :key ?Q)
                                               (:maildir "/letsEarnMoney/SBL"                  :key ?b)
                                               (:maildir "/letsEarnMoney/Sent"                 :key ?s)
                                               (:maildir "/letsEarnMoney/Spam"                 :key ?S)
                                               (:maildir "/letsEarnMoney/Trash"                :key ?T)))
                    (message-sendmail-extra-arguments . ("--account=letsEarnMoney"))
                    (mu4e-compose-signature .
                                            (concat
                                             "William Marty Buchaus Jr\n"
                                             "https://www.letsearnmoney.com\n"))))

;;;;;; OFMasons
          ,(make-mu4e-context
            :name "OFMasons"
            :enter-func (lambda () (mu4e-message "Switch to the OFMasons context"))
            :leave-func (lambda () (mu4e-message "Leaving OFMasons context"))
            :vars '((user-mail-address      . "marty@ofmasons.com")
                    (mu4e-get-mail-command  . "mbsync OFMasons")
                    (mu4e-refile-folder     . "/OFMasons/Archive")
                    (mu4e-trash-folder      . "/OFMasons/Trash")
                    (mu4e-drafts-folder     . "/OFMasons/Drafts")
                    (mu4e-sent-folder       . "/OFMasons/Sent")
                    (user-full-name         . "Marty Buchaus")
                    (mu4e-maildir-shortcuts . ((:maildir "/OFMasons/Archive"              :key ?a)
                                               (:maildir "/OFMasons/Drafts"               :key ?d)
                                               (:maildir "/OFMasons/INBOX"                :key ?i)
                                               (:maildir "/OFMasons/Junk"                 :key ?j)
                                               (:maildir "/OFMasons/Queue"                :key ?Q)
                                               (:maildir "/OFMasons/SBL"                  :key ?b)
                                               (:maildir "/OFMasons/Sent"                 :key ?s)
                                               (:maildir "/OFMasons/Spam"                 :key ?S)
                                               (:maildir "/OFMasons/Trash"                :key ?T)))
                    (message-sendmail-extra-arguments . ("--account=OFMasons"))
                    (mu4e-compose-signature .
                                            (concat
                                             "William Marty Buchaus Jr\n"
                                             "Meet on the level Act by the Plumb and Part upon the Square  AF&AM 832\n"
                                             "https://www.ofmasons.com\n"))))

;;;;;; Radhits
          ,(make-mu4e-context
            :name "TRadhits"
            :enter-func (lambda () (mu4e-message "Switch to the Rad Hits context"))
            :leave-func (lambda () (mu4e-message "Leaving Rad Hits context"))
            :vars '((user-mail-address       . "marty@radhits.net")
                    (mu4e-get-mail-command   . "mbsync Radhits")
                    (mu4e-trash-folder       . "/Radhits/Trash")
                    (mu4e-refile-folder      . "/Radhits/Archive")
                    (mu4e-drafts-folder      . "/Radhits/Drafts")
                    (mu4e-sent-folder        . "/Radhits/Sent")
                    (user-full-name          . "Marty Buchaus")
                    (mu4e-maildir-shortcuts  . ((:maildir "/Radhits/INBOX"    :key ?i)
                                                (:maildir "/Radhits/Archive"  :key ?a)
                                                (:maildir "/Radhits/Trash"    :key ?T)
                                                (:maildir "/Radhits/Sent"     :key ?s)))
                    (message-sendmail-extra-arguments . ("--account=Radhits"))
                    (mu4e-compose-signature .
                                            (concat
                                             "Marty Buchaus\n"
                                             "Meet on the Level Act by the Plumb and Part upon the Square\n"
                                             "mobile: 210-763-4052\n"))))

;;;;;; RedEarth Group Inc
          ,(make-mu4e-context
            :name "ERedEarthgroupinc"
            :enter-func (lambda () (mu4e-message "Switch to the Red Earth Group context"))
            :leave-func (lambda () (mu4e-message "Leaving Red Earth Group context"))
            :vars '((user-mail-address       . "marty@redearthgroupinc.com")
                    (mu4e-get-mail-command   . "mbsync RedEarth")
                    (mu4e-trash-folder       . "/RedEarth/Trash")
                    (mu4e-refile-folder      . "/RedEarth/Archive")
                    (mu4e-drafts-folder      . "/RedEarth/Drafts")
                    (mu4e-sent-folder        . "/RedEarth/Sent")
                    (user-full-name          . "Marty Buchaus")
                    (mu4e-maildir-shortcuts  . ((:maildir "/RedEarth/INBOX"      :key ?i)
                                                (:maildir "/RedEarth/Archive"    :key ?a)
                                                (:maildir "/RedEarth/Drafts"     :key ?d)
                                                (:maildir "/RedEarth/Trash"      :key ?T)
                                                (:maildir "/REdEarth/JUnk"       :key ?S)
                                                (:maildir "/RedEarth/Sent"       :key ?s)))
                    (message-sendmail-extra-arguments . ("--account=RedEarth"))
                    (mu4e-compose-signature .
                                            (concat
                                             "Marty Buchaus\n"
                                             "Meet on the Level Act by the Plumb and Part upon the Square\n"
                                             "mobile: 210-763-4052\n"))))

;;;;;; RE Construction FL
          ,(make-mu4e-context
            :name "FREconstructionfl"
            :enter-func (lambda () (mu4e-message "Switch to the Red Earth Construction FL context"))
            :leave-func (lambda () (mu4e-message "Leaving Red Earth Construction FL context"))
            :vars '((user-mail-address       . "marty@reconstructionfl.com")
                    (mu4e-get-mail-command   . "mbsync reconstructionfl")
                    (mu4e-trash-folder       . "/reconstructionfl/Trash")
                    (mu4e-refile-folder      . "/reconstructionfl/Archive")
                    (mu4e-drafts-folder      . "/reconstructionfl/Drafts")
                    (mu4e-sent-folder        . "/reconstructionfl/Sent")
                    (user-full-name          . "Marty Buchaus")
                    (mu4e-maildir-shortcuts  . ((:maildir "/reconstructionfl/INBOX"      :key ?i)
                                                (:maildir "/reconstructionfl/Archive"    :key ?a)
                                                (:maildir "/reconstructionfl/Drafts"     :key ?d)
                                                (:maildir "/reconstructionfl/JUnk"       :key ?S)
                                                (:maildir "/reconstructionfl/Trash"      :key ?T)
                                                (:maildir "/reconstructionfl/Sent"       :key ?s)))
                    (message-sendmail-extra-arguments . ("--account=ReConstructionFL"))
                    (mu4e-compose-signature .
                                            (concat
                                             "Marty Buchaus\n"
                                             "Meet on the Level Act by the Plumb and Part upon the Square\n"
                                             "mobile: 210-763-4052\n"))))

;;;;;; Google
          ,(make-mu4e-context
            :name "Gmail"
            :enter-func (lambda () (mu4e-message "Switch to the Gmail context"))
            :leave-func (lambda () (mu4e-message "Leaving Gmail context"))
            :vars '((user-mail-address       . "snuffop@gmail.com")
                    (mu4e-get-mail-command   . "mbsync Google")
                    (mu4e-trash-folder       . "/Google/Trash")
                    (mu4e-drafts-folder      . "/Google/Drafts")
                    (mu4e-sent-folder        . "/Google/Sent")
                    (mu4e-refile-folder      . "/Dabuke/Archive")
                    (user-full-name          . "Marty Buchaus")
                    (mu4e-maildir-shortcuts  . ((:maildir "/Google/Archive"           :key ?a)
                                                (:maildir "/Google/Drafts"            :key ?d)
                                                (:maildir "/Google/INBOX"             :key ?i)
                                                (:maildir "/Google/Sent"              :key ?s)
                                                (:maildir "/Google/Spam"              :key ?S)
                                                (:maildir "/Google/Trash"             :key ?T)))
                    (message-sendmail-extra-arguments . ("--account=Google"))
                    (mu4e-compose-signature .
                                            (concat
                                             "Marty Buchaus\n"
                                             "Meet on the Level Act by the Plumb and Part upon the Square\n"))))

;;;;;; Real House Hunters
          ,(make-mu4e-context
            :name "HH"
            :enter-func (lambda () (mu4e-message "Switch to the RHH context"))
            :leave-func (lambda () (mu4e-message "Leaving RHH context"))
            :vars '((user-mail-address      . "wbuchaus@realhousehunters.com")
                    (mu4e-get-mail-command  . "mbsync RHH")
                    (mu4e-refile-folder     . "/RHH/Archive")
                    (mu4e-trash-folder      . "/RHH/Trash")
                    (mu4e-drafts-folder     . "/RHH/Drafts")
                    (mu4e-sent-folder       . "/RHH/Sent")
                    (user-full-name         . "Marty Buchaus")
                    (mu4e-maildir-shortcuts . ((:maildir "/RHH/INBOX"   :key ?i)
                                               (:maildir "/RHH/Trash"   :key ?T)
                                               (:maildir "/RHH/Drafts"  :key ?d)
                                               (:maildir "/RHH/Archive" :key ?a)
                                               (:maildir "/RHH/Sent"    :key ?s)))
                    (message-sendmail-extra-arguments . ("--account=RHH"))
                    (mu4e-compose-signature .
                                            (concat
                                             "Marty Buchaus\n"
                                             "Real House Hunters / Jazney Inc\n"))))
          ) ;; End Lists
        ) ;; End Contexts
;;;;; END of after mu4e
  ) ;;end after mu4e
;;;; Modules
;;;;; mu4e-column-faces

(use-package! mu4e-column-faces
  :after mu4e
  :config (mu4e-column-faces-mode))

;;;; Functions

(defvar marty-mu4e/mu4e-compose-signed-p t)
(defvar marty-mu4e/mu4e-compose-signed-and-crypted-p nil)

(defun marty-mu4e/mu4e-compose-maybe-signed-and-crypted ()
  "Maybe sign or encrypt+sign message. Message is signed or encrypted+signed
when replying to a signed or encrypted message, respectively. Alternatively,
message is signed or encrypted+signed if `ambrevar/mu4e-compose-signed-p' or
`ambrevar/mu4e-compose-signed-and-crypted-p' is non-nil, respectively. This
function is suitable for `mu4e-compose-mode-hook'."
  (let ((msg mu4e-compose-parent-message))
    (cond
     ((or marty-mu4e/mu4e-compose-signed-and-crypted-p
          (and msg (member 'encrypted (mu4e-message-field msg :flags))))
      (mml-secure-message-sign-encrypt))
     ((or marty-mu4e/mu4e-compose-signed-p
          (and msg (member 'signed (mu4e-message-field msg :flags))))
      (mml-secure-message-sign-pgpmime)))))

;; Follow up quick key

(defun marty/capture-mail-follow-up (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture "ma"))

(defun marty/capture-mail-read-later (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture "mr"))
