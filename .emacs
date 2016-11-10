
;;; .emacs --- Summary

;;; Commentory:

;;;;;;;; install packages ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
;;;Have this in the beginning of .emacs, to see which libraries are loaded on start up
(setq message-log-max t) ;;; keep message buffer complete.

(defvar my-packages '(
			 ; a library in which all functions and constructs are prefixed with dash(-)
			 dash
			 ; to invoke functions of common lisp
			 cl
			 ; a theme for emacs
			 zenburn-theme
			 ; to customize the mode line
			 powerline
			 ; making buffer name unique, useful when opening multiple files with the same name
			 ;uniquify comes with Emacs
			 ; highlight current line
			 ido
			 ido-ubiquitous
			 ; dired-x comes with Emacs
			 smex
			 smartparens

			 magit
			 ; similar toace-jump-mode
			 avy
			 company
			 company-irony
			 multiple-cursors
			 ; makes the grep, and ag buffers writable so you can make changes to your search results.
			 wgrep
			 ;a minor mode that allows Multiple Major Modes to coexist in one buffer
			 mmm-mode
			; syntax checking, Supports over 30 programming and markup languages, but not java
			 flycheck
			 yasnippet
			 ; support c, c++, java
			 ggtags
			 ; exporting to HTML while respecting display properties such as colors, fonts, etc. used when org publish, no need configure it.
			 htmlize
			 ))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq package-user-dir "~/.emacs.d/elpa")

(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)

(unless package-archive-contents
    (package-refresh-contents))

(mapc (lambda (pkg)
	(or (package-installed-p pkg))
	(package-install pkg))
      my-packages)

; to disable automatic package loading after init.el
(setq package-enable-at-startup nil) 

(add-to-list 'load-path "~/.emacs.d/elpa")
(require 'cl)
;;;;;; configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq default-directory "~/")
; 'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;;;;; Appearance ;;;;;;;;;
(toggle-frame-maximized)
; disable tool bar
(tool-bar-mode -1)
; disbale menu bar
(menu-bar-mode -1)
; disable scroll bar
;;(scroll-bar-mode -1)
; setting powerline
;;(powerline-default-theme)
;;(setq powerline-arrow-shape 'arrow14)
;;(setq display-time-day-and-date t)
;;(setq display-time-24hr-format t)
;;(display-time-mode 1)
; load theme zenburn 
;;(load-theme 'zenburn t)

;;; buffer management ;;;;
; setting for uniquify 
(setq uniquify-buffer-name-style 'forward)

;;; minibuffer ;;;;
; setting for smex
(global-set-key (kbd "M-x") 'smex)
; setting for ido mode
(ido-mode 1)
(ido-everywhere 1)
;; setting for ido-ubiquitous
(ido-ubiquitous-mode 1)
;; org and magit already provide their own interfaces to ido,
;; so ido-ubiquitous specifically avoids interfering with these.
(setq org-completion-use-ido t)
(setq magit-completing-read-function 'magit-ido-completing-read)

;;; file management ;;;;;;;;
(defun my-open-externally (file-name)
  (interactive "open externally")
  (cond
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
     (start-process "open-externally" nil "xdg-open" file-name)))
    
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "open" file-name))
   
   ((string-equal system-type "cygwin")
    (shell-command (concat "cygstart " file-name)))
   ))

; dired
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            ))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            ;; (dired-omit-mode 1)
	    (defun dired-open-externally ()
	      (interactive)
	      (my-open-externally (dired-get-filename)))
	    (local-set-key (kbd "M-RET") 'dired-open-externally)
            ))
(global-set-key (kbd "C-x C-d") 'dired)

;;; file editor ;;;;
(global-font-lock-mode 1)
; it totally replaces show-paren-mode and language specific modes like hirb
(show-smartparens-global-mode +1)
(global-set-key (kbd "C-;") 'avy-goto-line)

;No need configuration for multiple-cursors

;;; grep & edit ;;;
(eval-after-load 'grep
  '(define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))
(autoload 'wgrep-change-to-wgrep-mode "wgrep")
(eval-after-load 'wgrep
  '(define-key grep-mode-map
     (kbd "C-c C-c") 'wgrep-finish-edit))

;;; Version Control ;;;
(global-set-key (kbd "C-x g") 'magit-status)


(add-hook 'after-init-hook #'global-company-mode)
;(add-hook 'after-init-hook #'global-flycheck-mode)

; c, c++, and java mode
;; add .sig file as c source code
(setq auto-mode-alist (append '(("\\.sig$" . c-mode)) auto-mode-alist))
;; cc-mode is builtin, it has effect on c, c++, java, etc.
(defun my-c-mode-common-hook ()
	     (which-function-mode t)  ; show function name in mode line

	     (setq c-default-style
	     	   '((java-mode . "java")
	     	     (awk-mode . "awk")
	     	     (c-mode . "linux")
	     	     (c++-mode . "stroustrup")
	     	     (other . "linux")))
	     (setq c-basic-offset 2)
	     (setq tab-width 2
		   ;; this will make sure spaces are used instead of tabs
		   indent-tabs-mode nil)

	     (hs-minor-mode t) ; hide and show block with builtin feature Hideshow
	     (read-only-mode 1)

	     ; gtags
	     ;; (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
	     ;;   (ggtags-mode 1))

	     ; rtags
	     (defun my-compile-func()
	       (interactive)
	       (save-buffer)
	       (compilation-mode)
	       (recompile))
	     (require 'rtags)  ;; optional
	     (rtags-restart-process) ;; to start rdm, or you can rtags-start-process-unless-running in emacs
	     (local-set-key (kbd "<f5>") 'my-compile-func)
	     
	     ; company-irony
	     (when (derived-mode-p 'c-mode 'c++-mode)
	       (setq company-backends '((company-irony company-gtags)))
	       (defun my-irony-mode-hook ()
		 (define-key irony-mode-map [remap completion-at-point]
		   'irony-completion-at-point-async)
		 (define-key irony-mode-map [remap complete-symbol]
		   'irony-completion-at-point-async))
	       (add-hook 'irony-mode-hook 'my-irony-mode-hook)
	       (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
	       (when (eq system-type 'windows-nt)
		 (setq w32-pipe-read-delay 0))
	       (irony-mode 1))
	       
	     (when (derived-mode-p 'c++-mode)
	       ;; useful for c++, help you expand the function headers defined in .h file to a .cpp file.
	       (require 'member-functions)
	       (setq mf--source-file-extension "cpp"))
	     
	     )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


; org mode
(defvar org-plantuml-jar-path (expand-file-name "C:/bin/plantuml.jar"))

(defun bh/display-inline-images ()
  (condition-case nil
      (org-redisplay-inline-images);; t t)
    (error nil)))

(defun my-org-mode ()  
  (org-babel-do-load-languages
     'org-babel-load-languages
     '((dot . t)
       (plantuml . t)
       (ditaa . t)
       (sh . t)
       (C . t)
       (python . t)
       (java . t)))
  ;; highlight keyword in code block
  (setq org-src-fontify-natively t)

  (setq org-confirm-babel-evaluate nil)

  (setq org-babel-results-keyword "results")

  (add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
  ;;(add-hook 'org-babel-after-execute-hook
  ;;         (lambda ()
  ;;           (when org-inline-image-overlays
  ;;             (org-redisplay-inline-images))))
  )
(add-hook 'org-mode-hook 'my-org-mode)

; org publish)
(setq org-publish-project-alist
      '(
	("org-notes"
	 :base-directory "~/github/Notes/"
	 :base-extension "org"
	 :publishing-directory "~/github/Blog/"
	 ;:publishing-directory "/ssh:user@server" ;export to server
	 :exclude "^temp.*.org"
	 :recursive t
	 :publishing-function my/org-html-publish-to-html
	 :headline-levels 4
	 :author "Howard Hou"
         :email "lianhong.hou@gmail.com"
	 :auto-sitemap t                
	 :sitemap-filename "index.org"  
	 :sitemap-title "Sitemap"
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"http://lianhonghou.github.io/css/norang.css\"/>"
	 :html-postamble "
                          <div id=\"disqus_thread\"></div>
                          <script type=\"text/javascript\">
                            var disqus_shortname = 'howardhou';
                            (function() {
                              var dsq = document.createElement('script'); 
                              dsq.type = 'text/javascript'; 
                              dsq.async = true;
                              dsq.src = '//' + disqus_shortname + '.disqus.com/embed.js';
                              (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
                            })();
                          </script>
                          <script>
                            (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                            (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                            m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                            })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
                            ga('create', 'UA-38087210-2', 'auto');
                            ga('send', 'pageview');
                          </script>
                          <p class=\"postamble\">Last Updated %C. </p>
                          <p class=\"postamble\">Created by %a with %c</p>
                         ")

	("org-static"                ;Used to publish static files
	 :base-directory "~/github/Notes/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "~/github/Blog/"
	 :recursive t
	 :publishing-function org-publish-attachment)
	("org" :components ("org-notes" "org-static"))))

(defun my/org-html-publish-to-html (plist filename pub-dir)
  (let ((org-confirm-babel-evaluate nil))
    (org-html-publish-to-html plist filename pub-dir)))

(defun my/org-publish-and-browse ()
  (interactive)
  (save-buffer)
  (let ((org-confirm-babel-evaluate nil)
	(default-directory (concat default-directory "../temp_html"))
	(browse-url-chromium-program "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
	(browse-url-browser-function #'browse-url-chromium)
	(org-html-head (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\"" (expand-file-name "~") "/github/Blog/css/norang.css\"/>")))
    ;(when (member "image" (directory-files ".")) (copy-directory "./image" default-directory)) it does not work, need debug it in future
    (org-html-export-to-html)
    (browse-url (org-export-output-file-name ".html" nil default-directory))))


;;;;;;;;;;;to be used in future;;;;;;;;;;;;;;;;;;;;;;;;;;
; yasnippet
;; (add-to-list 'load-path
;;               "~/.emacs.d/plugins/yasnippet")
;; (yas-global-mode 1)

;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;; 	(backward-char 1)
;; 	(if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;; 	    (null (do-yas-expand)))
;; 	(if (check-expansion)
;; 	    (company-complete-common)
;; 	  (indent-for-tab-command)))))
;; ; solving conflicts in Company and Yasnippet
;; (global-set-key [tab] 'tab-indent-or-complete)

;;; .emacs ends here
