
;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
			 
			 (add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
			 
;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-completing-read+

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; project navigation
    projectile

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit))

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
;(if (eq system-type 'darwin)
 ;   (add-to-list 'my-packages 'exec-path-from-shell))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
;(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "C:/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
;(load "shell-integration.el")



;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
;(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   (quote
    (magit tagedit projectile smex ido-completing-read+ clojure-mode-extra-font-locking clojure-mode paredit ))))


 (custom-set-faces
 '(default ((t (:family "DejaVu Sans Mono" :foundry "outline" :slant normal :weight normal :height 143 :width normal))))
 '(show-paren-match ((t (:foreground "white" :background "snow4")))))
 
 

 
(defun my-particulars ()
  (setq split-list (split-string (emacs-version) "("))
  (setq version-emacs (nth 0 split-list))
  (princ "++++++++++++++++++++++++++++++++")
  (princ "\n")
  (princ "Emacs Directory -- ")
  (princ (expand-file-name user-emacs-directory))
  (princ "\n")
  (princ "Emacs Init file -- ")
  (princ user-init-file)
  (princ "\n")
  (princ " Emacs Location -- ")
  (princ (expand-file-name invocation-name invocation-directory))
  (princ "\n")
  (princ "  Emacs Version -- ")
  (princ version-emacs)
  (princ "\n")
  (princ " HOME Directory -- ")
  (princ (getenv "HOME"))
  (princ "\n")
  (princ "++++++++++++++++++++++++++++++++")) 

(require 'kmb)
(require 'ag)
(require 'highlight-symbol)
(require 'zoom-frm)
(require 'sesman)

(defun my-white-spaces ()
  (setq mode-require-final-newline nil)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun my-parens-colors ()              ; https://yoo2080.wordpress.com/2013/09/08/living-with-rainbow-delimiters-mode/
  (require 'rainbow-delimiters)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  
  (defface my-outermost-paren-face '((t (:weight bold))) "Face used for outermost parens.")
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil
                    :foreground 'unspecified
                    :inherit 'my-outermost-paren-face)
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil
                    :foreground 'unspecified
                    :inherit 'my-outermost-paren-face)
  
  (defvar my-paren-dual-colors '("hot pink" "dodger blue"))
  (setq rainbow-delimiters-outermost-only-face-count 0)
  (setq rainbow-delimiters-max-face-count 2)
  (set-face-foreground 'rainbow-delimiters-depth-1-face (elt my-paren-dual-colors 1))
  (set-face-foreground 'rainbow-delimiters-depth-2-face (elt my-paren-dual-colors 0))

  (require 'paren) ; show-paren-mismatch is defined in paren.el
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'show-paren-mismatch))
   
(defun my-focus (active-background)
 (require 'hiwin)
 (hiwin-activate)                           
 (set-face-background 'hiwin-face active-background)  
 (custom-set-faces '(show-paren-match ((t (:foreground "white" :background "snow4")))))
 (defface my-compile-face
   '((t (:foreground "green" :background "orange")))
   "Compile buffer highlighting of busy signal so can see when done"
   :group 'faces))

(defun my-boundry ()
  (modify-syntax-entry ?\/ "'" clojure-mode-syntax-table))

(defun my-modes ()
  (menu-bar-mode 1)
  (tool-bar-mode -1) 
  (scroll-bar-mode t)
  (setq column-number-mode t)
  (show-paren-mode t)
  (setq show-paren-style 'expression)
  (set-default 'truncate-lines t))

(defun my-tabs () 
  (require 'centaur-tabs)
  (centaur-tabs-mode t)                
  (setq centaur-tabs-modified-marker "O")                         
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'over)
  (setq centaur-tabs-set-modified-marker t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-icons t))   

(defun my-ibuffer ()
  (require 'ibuffer-sidebar)
  (require 'ibuf-ext)
 
 (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq-default ibuffer-default-sorting-mode 'mode-name)
  (setq ibuffer-saved-filter-groups
    (quote (("default"
            ("CIDER" (or (name . "^\\*cider-repl.*")
            (name . "^\\*cider-error\\*$")))
            (" CORE" (filename . "core.*\\.clj"))
            ("  Src" (filename . "^.*\\/src\\/.*\\.clj"))
            (" Test" (filename . "^.*\\/test\\/.*\\.*"))         
            (" PROC" (name . "^Procfile$") )
            (" Proj" (filename . "clj"))
            (" Eden" (filename . "edn"))
            (" Html" (filename . "html"))
            (" Css " (filename . "css"))
            (" Text" (filename . "txt"))
            (" Md  " (filename . "md"))
            ("dired" (mode . dired-mode))
            ("emacs" (or (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))
  (add-hook 'ibuffer-mode-hook
    '(lambda ()
      (ibuffer-auto-mode 1)
      (ibuffer-switch-to-saved-filter-groups "default")))
      (add-to-list 'ibuffer-never-show-predicates "^\\:")          ;; hide ":d:/..."
	  (add-to-list 'ibuffer-never-show-predicates "*dashboard*")   
	  (add-to-list 'ibuffer-never-show-predicates "*cider-ns-refresh-log*")    
	  (add-to-list 'ibuffer-never-show-predicates "*nrepl"))

(defun my-autosave (wait-seconds)
  (run-with-idle-timer wait-seconds t (lambda () (save-some-buffers t))))
 
(defun my-mousewheel ()
  (setq mouse-wheel-progressive-speed nil) 
  (setq mouse-wheel-follow-mouse 't)
  (setq scroll-step 1))
 
(defun my-cider ()
  (setq cider-repl-display-help-banner nil) 
  (setq cider-ns-save-files-on-refresh t)
  (setq cider-repl-require-ns-on-set t)
  (setq cider-auto-select-error-buffer nil))
 
(defun my-cider-funckeys ()
  (global-set-key [f1]  'my-cider-eval-sexp)         
  (global-set-key [f2]  'my-cider-load-buffer)       
  (global-set-key [f3]  'cider-repl-set-ns)        
  (global-set-key [f4]  'my-cider-reaload-all)
  (global-set-key [f5]  'cider-jack-in)
  (global-set-key [f11] 'cider-format-buffer)) 

(defun my-js-funckeys ()) 
 
(defun my-funckeys (lang-type) 
  (if (string-equal lang-type "clj")
    (my-cider-funckeys)
	(if (string-equal lang-type "js")
      (my-js-funckeys)))
  (global-set-key [f6]  'delete-window)
  (global-set-key [f7]  'split-window-below)
  (global-set-key [f8]  'split-window-right)
  (global-set-key [f9]  'my-full-screen)
  (global-set-key [f10] 'my-flip-comment-line-or-region)
  (global-set-key [f12] 'ag))
 
(defun my-cider-chords ()
  (key-chord-define-global "''"   'my-kill-non-core-cljs )   
  (key-chord-define-global "[["   'paredit-mode) 
  (key-chord-define-global "qq"   'cider-repl-clear-buffer)
  (key-chord-define-global "QQ"   'cider-repl-clear-buffer))
 
(defun my-chords (lang-type)
  (require 'key-chord)
  (key-chord-mode 1)
  (if (string-equal lang-type "clj")
	(my-cider-chords))
  (key-chord-define-global "KK"   'kill-whole-line)
  (key-chord-define-global "kk"   'kill-whole-line)
  (key-chord-define-global "--"   'zoom-frm-out)
  (key-chord-define-global "=="   'zoom-frm-in)
  (key-chord-define-global "^^"   'my-change-dired-sidebar)
  (key-chord-define-global "\\\\" 'my-kill-search-buffs )   
  (key-chord-define-global "``"   'highlight-symbol)
  (key-chord-define-global "]]"   'mark-whole-buffer)   
  (key-chord-define-global ",,"   'my-sidebar-toggle))
 
(defun my-modeline ()
  (setq-default mode-line-format
    (quote("- "
           (:eval (if (eq buffer-file-name nil) 
                    (propertize "[" 'face 'my-compile-face)))  
             mode-line-process
           (:eval (if (eq buffer-file-name nil) 
                    (propertize "]" 'face 'my-compile-face)))
           " "
             mode-line-mule-info
           " "
             mode-line-modified
		   " "
		     mode-line-position
           " "    
              buffer-file-name))))

(defun my-sidebar () 
  (require 'dired-sidebar) 
  
  (defun my-sidebar-toggle ()
    "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
    (interactive)
    (dired-sidebar-toggle-sidebar)
    (ibuffer-sidebar-toggle-sidebar))
   
  (defun my-change-dired-sidebar ()          
    "move dired-sidebar root to current buffer"
    (interactive)
    (dired-sidebar-switch-to-dir (file-name-directory default-directory))))  

(defun my-flip-comment-line-or-region ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))    
    
(defun my-kill-search-buffs ()
  (interactive)
   (dolist (buffer (buffer-list))
     (let ((name (buffer-name buffer)))
       (if  (string-prefix-p "*ag " name)
         (kill-buffer buffer)))))
       
(defun my-kill-non-core-cljs ()
  (interactive)
   (dolist (buffer (buffer-list))
     (let ((name (buffer-name buffer)))
       (if (string-suffix-p ".clj" name)
         (if (not (string-prefix-p "core" name) )
           (kill-buffer buffer))))))    

(defun my-cider-eval-sexp () ;F1 key          
  (interactive)
  (message "%s" "CIDER eval-sexp" buffer-file-name)
  (save-buffer)
  (kmb-kill-matching-buffers-no-ask "\*cider-error\*") 
  (cider-eval-defun-at-point))                                  
         
(defun my-cider-load-buffer () ;F2 key        
  (interactive)
  (message "%s" "CIDER load-buffer" buffer-file-name)
  (save-buffer)
  (kmb-kill-matching-buffers-no-ask "\*cider-error\*")   ;So can tell if there is NO CURRENT error by the absence of *cider-error* buffer"
  (cider-load-buffer))                                                          

(defun my-cider-reaload-all () ;F4 key       
  (interactive)
  (save-buffer)
  (cider-ns-reload-all)
  (cider-ns-refresh)
  (kmb-kill-matching-buffers-no-ask "\*cider-error\*")
  (kmb-kill-matching-buffers-no-ask "\*nrepl-server\*"))    
  
(defun my-full-screen()           
  (interactive)
  (toggle-frame-fullscreen)
    (if menu-bar-mode
     (menu-bar-mode 0)
      (menu-bar-mode 1)))  
 
(defun my-buffermenu ()	  
  (defun my-menu-read-modifed (a-buff)
   (let ((read-only-char "")
         (modified-char ""))
      (with-current-buffer a-buff
        (if buffer-read-only 
          (setq read-only-char "%")))
      (if (buffer-modified-p a-buff)
        (setq modified-char "*"))
      (concat read-only-char modified-char)))

  (defun my-menu-label-core-clj(a-buff) 
    "format clojure files, highlight if starts with 'core'"
    (format "%1$s%2$s%3$s%4$s" 
      (my-menu-read-modifed a-buff)
      (if (string-prefix-p "test" (buffer-name a-buff) ) 
        "      " 
        "")                                             ;   test_file.clj
      (buffer-name a-buff)
      (if (string-prefix-p "core" (buffer-name a-buff) )
        " ***" 
        "")))                                         ;core.clj ***

  (defun my-menu-no-case-sort (buff-1 buff-2)
    "ensure mouse-buffer-menu is sorted with no-case"
    (string> (downcase (buffer-name buff-1)) (downcase (buffer-name buff-2))))

  (defun my-menu-is-ag (name-of-buffer)
    "is the buffer an ag-search buffer?"
    (if (string-prefix-p "*ag " name-of-buffer)
       t
       nil))
     
  (defun my-menu-is-clj (name-of-buffer)
    "is the buffer a clojure buffer?"
    (if (string-suffix-p ".clj" name-of-buffer)
       t
       nil))     
     
  (defun my-menu-no-start-blank (name-of-buffer)
    "ignore buffers starting with white space"
    (if (/= (aref name-of-buffer 0) ?\s)
       t
       nil))
      
  (defun my-menu-label-default(a-buffer) 
    "format mouse-buffer entry as a default file"
    (format "  %1$s%2$s%3$s" 
      (my-menu-read-modifed a-buffer)
      (buffer-name a-buffer)    ; readme.txt ---
      " ---"))                 
     
  (defun my-menu-label-search(name-of-buffer) 
    "format mouse-buffer entry as an ag-search"
      (setq split-list (split-string name-of-buffer "text:\\| dir:"))
      (setq search-for (nth 1 split-list))
      (format "%1$s%2$s" 
        "~~~search : " 
        search-for))
      
  (defun my-menu-buff-show (name-of-buffer)
    "ignore emacs buffers on mouse-buffer-menu"
    (if (or (equal name-of-buffer "*dashboard*" )
            (equal name-of-buffer "*scratch*")
            (equal name-of-buffer "*cider-ns-refresh-log*")
            (string-prefix-p "*nrepl-server" name-of-buffer)  
            (string-prefix-p ":" name-of-buffer  )
            (equal name-of-buffer "*:Buffers:*")
            (equal name-of-buffer "*Messages*"))
        nil
        t))     
     
  (defun mouse-buffer-menu-alist (the-buffers)      
    "Buffer menu displayed when control left click"
    (let ((mouse-buf-menu)                                                              
          (the-dashboard-buffer))
      (setq sorted-buffers (sort the-buffers `my-menu-no-case-sort))
      (dolist (a-buffer sorted-buffers mouse-buf-menu)
        (progn
          (setq name-of-buffer (buffer-name a-buffer))
          (if (eq "*dashboard*" name-of-buffer)
            (setq the-dashboard-buffer a-buffer))
          (if (my-menu-buff-show name-of-buffer)
            (if (my-menu-no-start-blank name-of-buffer)
              (progn
                (setq menu-label (cond 
                  ((my-menu-is-ag name-of-buffer) (my-menu-label-search name-of-buffer))         
                  ((my-menu-is-clj name-of-buffer) (my-menu-label-core-clj a-buffer))                
                  (t                            (my-menu-label-default a-buffer))))
                (setq menu-entry (cons menu-label a-buffer))                       
                (setq mouse-buf-menu (cons menu-entry mouse-buf-menu)))))))
      (if (eq 0 (length mouse-buf-menu))
          (cons "*dashboard*" the-dashboard-buffer) ; if dashboard only buffer
          mouse-buf-menu)))

  (setq mouse-buffer-menu-mode-mult 1234))  ;; deter sub-menus on control-click 

(defun my-buffers (start-dir)
  (switch-to-buffer "*Messages*")
  (kill-buffer "*scratch*")
  (cd start-dir)) 

(defun my-load-first (my-start-dir my-start-file)
  (my-buffers my-start-dir)   
  (setq initial-buffer-choice (concat my-start-dir my-start-file))
  (setq split-lang (split-string my-start-file "\\."))
  (car (last split-lang)))

(defun my-rewrite-keys ()
 (global-set-key (kbd "C-z") 'undo)
 (global-set-key (kbd "C-v") 'ignore)       ; turn off page down
 (global-set-key (kbd "C-s") 'save-buffer))
 
(defun my-go-emacs (my-start-dir my-start-file my-start-command)
  (lexical-let ((my-start-mess (concat "Starting in " my-start-dir my-start-file)))
   (defun display-startup-echo-area-message ()
    (message my-start-mess)))
  (setq lang-extension (my-load-first my-start-dir my-start-file) )
  (my-funckeys lang-extension)
  (my-chords lang-extension)
  (my-sidebar-toggle)
  (my-particulars)
  (command-execute (intern my-start-command)))

(my-ibuffer)
(my-tabs)
(my-modes)
(my-focus "snow2")
(my-boundry)
(my-autosave 2)
(my-mousewheel)
(my-cider)
(my-modeline)
(my-sidebar)
(my-buffermenu)
(my-parens-colors)
(my-white-spaces)
(my-rewrite-keys) 

; F1:eval SEXP             F6:del-win            F10:COMMENT-code     
;   F2:eval BUFFER           F7:ver-split           F11:FORMAT-code
;     F3:set NS                F8:hor-split            F12:FILE-SEARCH
;       F4:eval ALL              F9:FULL-SCREEN  
;         F5:JACK-IN               
;
;  C-<SPC>:start mark  MOUSE-3:end region  C-x C-f:create file
;
;  C-w:cut  M-w:copy  C-y:Paste                control-q <tab>
;
; highlight-and-count-text:~~             zoom-out:--   zoom-in:==
; clear-repl:qq          para-mode:[[   select-all:]]   \\:kill-searches                
;                   kill-line:KK      kill-non-core.cljs:''
;                                       sidebar-toggle:,,  
 
;(my-go-emacs "C:/_progs_/status" "/src/core.clj" "cider-jack-in") 

(my-go-emacs "C:/_progs_/clojure-text-diff" "/src/text_diff.clj" "cider-jack-in") 



 
 
 