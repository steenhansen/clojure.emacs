
;; .emacs or init.el
;; C-h v 'user-init-file' will show where init.el/.emacs file is
;; https://github.com/flyingmachine/emacs-for-clojure/		

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

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Define he following variables to remove the compile-log warnings
;; when defining ido-ubiquitous
;; (defvar ido-cur-item nil)
;; (defvar ido-default-item nil)
;; (defvar ido-cur-list nil)
;; (defvar predicate nil)
;; (defvar inherit-input-method nil)

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

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

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

    ;; colorful parenthesis matching
    rainbow-delimiters

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
(if (eq system-type 'darwin)
    (add-to-list 'my-packages 'exec-path-from-shell))

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
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

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
    (flycheck-clojure dashboard centaur-tabs hiwin key-chord ibuffer-sidebar dired-sidebar smart-mode-line-atom-one-dark-theme ag use-package magit tagedit rainbow-delimiters projectile smex ido-completing-read+ cider clojure-mode-extra-font-locking clojure-mode paredit exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(eval-after-load 'flycheck '(flycheck-clojure-setup))             
(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(require 'zoom-frm)    
(require 'key-chord)  
(require 'kmb)                    
(require 'highlight-symbol)
(require 'dashboard)
(dashboard-setup-startup-hook)                                    
(setq dashboard-items '((recents  . 10)
                        (bookmarks . 10)))

(require 'hiwin)
(hiwin-activate)                           ; active window has different background
(set-face-background 'hiwin-face "gray10")  
(set-face-foreground 'linum "DarkSlateGray")

(menu-bar-mode 1)
(tool-bar-mode -1) 
(scroll-bar-mode t)
(setq column-number-mode t)

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

;(add-to-list 'ibuffer-never-show-predicates "^\\:")          ;; hide ":d:/..."
(add-to-list 'ibuffer-never-show-predicates "*dashboard*")   
(add-to-list 'ibuffer-never-show-predicates "*nrepl")        
 
(defun e-sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))
   
(defun e-change-dired-sidebar ()          
  "move dired-sidebar root to current buffer"
  (interactive)
  (dired-sidebar-switch-to-dir (file-name-directory default-directory)))

(cua-mode t)                              
    (setq cua-auto-tabify-rectangles nil) 
    (transient-mark-mode 1)               
    (setq cua-keep-region-after-copy t)                            
  
(centaur-tabs-mode t)                
   (setq centaur-tabs-modified-marker "O")                         
   (setq centaur-tabs-style "bar")
   (setq centaur-tabs-height 32)
   (setq centaur-tabs-set-icons t)
   (setq centaur-tabs-set-bar 'over)
   (setq centaur-tabs-set-modified-marker t)
   (centaur-tabs-headline-match)
   (setq centaur-tabs-set-icons t)                                

(run-with-idle-timer 2 t (lambda () (save-some-buffers t)))     ; save changed files after 2 seconds

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; smooth scrolling
(setq mouse-wheel-progressive-speed nil) 
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1) 

(defun e-flip-comment-line-or-region ()
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))    
    
(defun e-kill-search-buffs ()
  (interactive)
   (dolist (buffer (buffer-list))
     (let ((name (buffer-name buffer)))
       (if  (string-prefix-p "*ag " name)
         (kill-buffer buffer)))))
       
(defun e-kill-non-core-cljs ()
  (interactive)
   (dolist (buffer (buffer-list))
     (let ((name (buffer-name buffer)))
       (if (string-suffix-p ".clj" name)
         (if (not (string-prefix-p "core" name) )
           (kill-buffer buffer))))))    

(setq cider-ns-save-files-on-refresh t)
(setq cider-repl-require-ns-on-set t)
(setq compilation-auto-jump-to-first-error t)
(setq cider-auto-select-error-buffer nil)     
         
(defun e-cider-eval-sexp ()           
  (interactive)
  (save-buffer)
  (kmb-kill-matching-buffers-no-ask "\*cider-error\*") 
  (cider-eval-defun-at-point))                                  
         
(defun e-cider-load-buffer ()           
  (interactive)
  (save-buffer)
  (kmb-kill-matching-buffers-no-ask "\*cider-error\*")   ;So can tell if there is NO CURRENT error by the absence of *cider-error* buffer"
 (cider-load-buffer))                           

(defun e-cider-reaload-all ()           
  (interactive)
  (save-buffer)
  (kmb-kill-matching-buffers-no-ask "\*cider-error\*") 
  (cider-ns-reload-all))                                     

(defun e-full-screen()           
  (interactive)
  (toggle-frame-fullscreen)
    (if menu-bar-mode
     (menu-bar-mode 0)
      (menu-bar-mode 1)))  
  
(global-set-key [f1]  'e-cider-eval-sexp)         
(global-set-key [f2]  'e-cider-load-buffer)       
(global-set-key [f3]  'e-cider-reaload-all)        
(global-set-key [f4]  'cider-repl-set-ns)
(global-set-key [f5]  'cider-jack-in)

(global-set-key [f6]  'delete-window)
(global-set-key [f7]  'split-window-below)
(global-set-key [f8]  'split-window-right)
(global-set-key [f9] 'e-full-screen)

(global-set-key [f10]  'e-flip-comment-line-or-region)
(global-set-key [f11] 'cider-format-buffer)
(global-set-key [f12]  'ag)                    

                                        
  (key-chord-mode 1)
  (key-chord-define-global "KK"   'kill-whole-line)
  (key-chord-define-global "kk"   'kill-whole-line)

  (key-chord-define-global "--"   'zoom-frm-out)
  (key-chord-define-global "=="   'zoom-frm-in)

  (key-chord-define-global "^^"   'e-change-dired-sidebar)          ;

  (key-chord-define-global "\\\\" 'e-kill-search-buffs )   
  (key-chord-define-global "''"   'e-kill-non-core-cljs )   
      
  (key-chord-define-global "``"   'highlight-symbol)
  
  (key-chord-define-global "QQ"  'paredit-mode) 
  (key-chord-define-global "qq"  'paredit-mode)                  
  
  (key-chord-define-global "AA"   'mark-whole-buffer)   
  (key-chord-define-global "aa"   'mark-whole-buffer)              
 
  (key-chord-define-global "ZZ"   'e-sidebar-toggle)              
  (key-chord-define-global "zz"   'e-sidebar-toggle)                  
      
;; start mouse-1 click menu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(defun e-menu-read-modifed (a-buff)
 (let ((read-only-char "")
       (modified-char ""))
    (with-current-buffer a-buff
      (if buffer-read-only 
        (setq read-only-char "%")))
    (if (buffer-modified-p a-buff)
      (setq modified-char "*"))
    (concat read-only-char modified-char)))

(defun e-label-core-clj(a-buff) 
  "format clojure files, highlight if starts with 'core'"
  (format "%1$s%2$s%3$s%4$s" 
    (e-menu-read-modifed a-buff)
    (if (string-prefix-p "test" (buffer-name a-buff) ) 
      "      " 
      "")                                  ;                  test_file.clj
    (buffer-name a-buff)
    (if (string-prefix-p "core" (buffer-name a-buff) )
      " ***" 
      "")))                                     ; core.clj ***

(defun e-menu-no-case-sort (buff-1 buff-2)
  "ensure mouse-buffer-menu is sorted with no-case"
  (string> (downcase (buffer-name buff-1)) (downcase (buffer-name buff-2))))

(defun e-menu-is-ag (name-of-buffer)
  "is the buffer an ag-search buffer?"
  (if (string-prefix-p "*ag " name-of-buffer)
     t
     nil))
     
(defun e-menu-is-clj (name-of-buffer)
  "is the buffer a clojure buffer?"
  (if (string-suffix-p ".clj" name-of-buffer)
     t
     nil))     
     
(defun e-menu-no-start-blank (name-of-buffer)
  "ignore buffers starting with white space"
  (if (/= (aref name-of-buffer 0) ?\s)
     t
     nil))
      
(defun e-label-default(a-buffer) 
  "format mouse-buffer entry as a default file"
  (format "  %1$s%2$s%3$s" 
    (e-menu-read-modifed a-buffer)
    (buffer-name a-buffer)    ; readme.txt ---
    " ---"))                 
     
(defun e-label-search(name-of-buffer) 
  "format mouse-buffer entry as an ag-search"
    (setq split-list (split-string name-of-buffer "text:\\| dir:"))
    (setq search-for (nth 1 split-list))
    (format "%1$s%2$s" 
      "~~~search : " 
      search-for))
      
(defun e-menu-buff-show (name-of-buffer)
  "ignore emacs buffers on mouse-buffer-menu"
  (if (or (equal name-of-buffer "*dashboard*" )
          (equal name-of-buffer "*scratch*")
          (string-prefix-p "*nrepl-server _clojure/" name-of-buffer)  
          (string-prefix-p ":" name-of-buffer  )
          (equal name-of-buffer "*:Buffers:*")
          (equal name-of-buffer "*Messages*"))
      nil
      t))     
     
(defun mouse-buffer-menu-alist (the-buffers)      
  "Buffer menu displayed when control left click"
  (let ((mouse-buf-menu)                                                              
        (the-dashboard-buffer))
    (setq sorted-buffers (sort the-buffers `e-menu-no-case-sort))
    (dolist (a-buffer sorted-buffers mouse-buf-menu)
      (progn
        (setq name-of-buffer (buffer-name a-buffer))
        (if (eq "*dashboard*" name-of-buffer)
          (setq the-dashboard-buffer a-buffer))
        (if (e-menu-buff-show name-of-buffer)
          (if (e-menu-no-start-blank name-of-buffer)
            (progn
              (setq menu-label (cond 
              	((e-menu-is-ag name-of-buffer) (e-label-search name-of-buffer))         
                ((e-menu-is-clj name-of-buffer) (e-label-core-clj a-buffer))                
                (t                            (e-label-default a-buffer))))
              (setq menu-entry (cons menu-label a-buffer))                       
              (setq mouse-buf-menu (cons menu-entry mouse-buf-menu)))))))
    (if (eq 0 (length mouse-buf-menu))
        (cons "*dashboard*" the-dashboard-buffer) ; if dashboard only buffer
        mouse-buf-menu)))
   
(setq mouse-buffer-menu-mode-mult 1234)  ;; deter sub-menus on control-click 

;; end mouse-1 click menu ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   

(defface e-my-compile-face
  '((t (:foreground "green" :background "orange")))
  "Compile buffer highlighting of busy signal so can see when done"
  :group 'faces)
 
(setq-default mode-line-format
    (quote("- "
           (:eval (if (eq buffer-file-name nil) 
                    (propertize "[" 'face 'e-my-compile-face)))  
             mode-line-process
           (:eval (if (eq buffer-file-name nil) 
                    (propertize "]" 'face 'e-my-compile-face)))
           " "
             mode-line-mule-info
           " "
             mode-line-modified
           " "    
             (:eval (substring (system-name) 0 (string-match "\\..+" (system-name))))
           "~"
             default-directory)))
 
; one-word/another-word have / be word boundry 
(modify-syntax-entry ?\/ "'" clojure-mode-syntax-table)      

; F1:eval SEXP             F6:del-win                 F11:full-screen
;   F2:eval BUFFER           F7:ver-split                F12:search-text
;     F3:eval ALL               F8:hor-split 
;       F4:set NS                 F9:comment-code  
;         F5:JACK-IN                F10:format-code 
;
; highlight-and-
;    -count-text:~~  C-<SPC>:start mark               --:zoom-out ==:zoom-in
;      para-mode:QQ  MOUSE-3:end region                  \\:kill-searches                
;     select-all:AA                     KK:kill-line     '':kill-non-core.cljs
; sidebar-toggle:ZZ  C-x C-f:create file  C-x C-e:Execute eLisp          

;;;;;;;;;;;;;;;;;;;;;;;;; current project ;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq default-directory "D:/_clojure/stat/src" )


  
