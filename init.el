;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       (company           ; the ultimate code completion backend
        +childframe)
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;; (ivy               ; a search engine for love and life
       ;; +prescient
       ;; +fuzzy)
       (vertico
        +icons)

       :ui
       ;; deft
       doom              ; what makes doom look the way it does
       doom-dashboard    ; a nifty splash screen for emacs
       ;; doom-quit         ; doom quit-message prompts when you quit emacs
       (emoji +unicode)  ; 🙂
       ;;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight todo/fixme/note/deprecated/hack/review
       hydra
       ;;indent-guides     ; highlighted indent columns
       ligatures         ; ligatures and symbols to make your code pretty again
       ;;  +extra)
       ;;minimap           ; show a map of the code on the side
       modeline          ; snazzy, atom-inspired modeline, plus api
       nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like nerdtree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +defaults
        +all)
       ;;tabs              ; a tab bar for emacs
       treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond eob
       (window-select     ; visually switch windows
        +numbers)
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       ;;file-templates    ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       format              ; automated prettiness
       ;;god               ; run emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;; parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. they type so i don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired             ; making dired pretty [functional]
        +icons)
       electric          ; smarter, keyword-based electric-indent
       (ibuffer         ; interactive buffer management
        +icons)
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and emacs, sitting in a tree

       :term
       eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell repl for emacs
       ;;term              ; basic terminal emulator for emacs
       vterm             ; the best terminal emulation in emacs

       :checkers
       (syntax           ; tasing you for every semicolon you forget
        +childframe)
       (spell          ; tasing you for misspelling mispelling
        +flyspell)
       grammar           ; tasing grammar mistake every you make

       :tools
       ansible
       debugger          ; fixme stepping through code, to help you add bugs
       ;;direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame jupyter notebooks with emacs
       eval     ; run code, run (also, repls)
       ;; +overlay)
       gist              ; interacting with github gists
       (lookup              ; navigate your code and its documentation
        +dictionary)
       (lsp
        +peek)
       (magit)             ; a git porcelain for emacs

       make              ; run make tasks from emacs
       (pass              ; password manager for nerds
        +auth)
       pdf               ; pdf enhancements
       prodigy           ; fixme managing external services & code builders
       rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       terraform         ; infrastructure as code
       ;;tmux              ; an api for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp

       :os
       ;; (:if is-mac macos)  ; improve compatibility with macos
       (tty               ; improve the terminal emacs experience
        +osc)

       :lang
       ;;agda              ; types of types of types of types...
       (beancount         ; mind the gaap
        +lsp)
       (cc                ; c > c++ == 1
        +lsp)
       (clojure           ; java with a lisp
        +lsp)
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .net, and mono shenanigans
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of tea?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ml stands for microsoft's language
       ;;fstar             ; (dependent) types and (monadic) effects and z3
       ;;gdscript          ; the language you waited for
       (go                 ; the hipster dialect
        +lsp)
       (haskell            ; a language that's lazier than i am
        +dante)
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       (json              ; at least it ain't xml
        +lsp)
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript        ; all(hope(abandon(ye(who(enter(here))))))
        +lsp)
       ;;julia             ; a better, faster matlab
       ;;kotlin            ; a better, slicker java(script)
       (latex             ; writing papers in emacs has never been so fun
        +lsp)
       ;;lean              ; for folks with too much to prove
       ledger            ; be audit you can be
       (lua               ; one-based indices? one-based indices
        +lsp)
       (markdown          ; writing docs for people to ignore
        +grip)
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; i hereby declare "nix geht mehr!"
       ;; ocaml             ; an objective camel
       (org               ; organize your plain life in plain text
        +hugo
        +pandoc
        +pretty
        +pomodoro
        +noter
        +present
        +gnuplot
        +dragndrop
        +roam2)
       (php               ; perl's insecure younger brother
        +lsp)
       plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python            ; beautiful is better than ugly
        +lsp
        +pyenv
        +pyright)
       qt                ; the 'cutest' gui framework ever
       ;;racket            ; a dsl for dsls
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; emacs as a rest client
       ;;rst               ; rest in peace
       ;;(ruby +rails)     ; 1.step {|i| p "ruby is #{i.even? ? 'love' : 'life'}"}
       (rust              ; fe2o3.unwrap().unwrap().unwrap().unwrap()
        +lsp)
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       (sh                ; she sells {ba,z,fi}sh shells on the c xor
        +powershell
        +lsp)
       ;;sml
       ;;solidity          ; do you need a blockchain? no.
       ;;swift             ; who asked for emoji variables?
       terra             ; earth and moon in alignment for performance.
       web               ; the tubes
       (yaml              ; json, but readable
        +lsp)
       ;;zig               ; c, but simpler

       :email
       (mu4e
        +gmail)

       :app
       calendar
       emms
       everywhere        ; *leave* emacs!? you must be joking
       rss
       irc

       :config
       literate
       (default
         +bindings
         +smartparens))
