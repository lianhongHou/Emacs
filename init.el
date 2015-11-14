
;;; init.el --- Summary

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
			 hl-line
			 ido
			 ido-ubiquitous
			 ; dired-x comes with Emacs
			 smex
			 smartparens
			 ; similar toace-jump-mode
			 avy
			 company
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

(require 'cl)
;;;;;; configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 'y' for 'yes', 'n' for 'no'
(fset 'yes-or-no-p 'y-or-n-p)
; disable tool bar
(tool-bar-mode -1)
; disbale menu bar
(menu-bar-mode -1)
; disable scroll bar
(scroll-bar-mode -1)
; load theme zenburn 
(load-theme 'zenburn t)
; setting powerline
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow14)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode 1)
;set transparent effect
(global-set-key [(f11)] 'loop-alpha)
(defvar alpha-list '((100 100) (95 65) (85 55) (75 45) (65 35)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))                ;; head value will set to
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    ))

; setting for uniquify 
(setq uniquify-buffer-name-style 'forward)
; setting for ido mode
(ido-mode 1)
(ido-everywhere 1)
;; setting for ido-ubiquitous
(ido-ubiquitous-mode 1)
;; org and magit already provide their own interfaces to ido,
;; so ido-ubiquitous specifically avoids interfering with these.
(setq org-completion-use-ido t)
(setq magit-completing-read-function 'magit-ido-completing-read)

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
            ))

; setting for smex
(global-set-key (kbd "M-x") 'smex)

; setting hl-line
(global-hl-line-mode)
(set-face-background 'hl-line "#073642")
;
(global-font-lock-mode 1)
; it totally replaces show-paren-mode and language specific modes like hirb
(show-smartparens-global-mode +1)

(global-set-key (kbd "C-;") 'avy-goto-line)

(eval-after-load 'grep
  '(define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))
(autoload 'wgrep-change-to-wgrep-mode "wgrep")
(eval-after-load 'wgrep
  '(define-key grep-mode-map
     (kbd "C-c C-c") 'wgrep-finish-edit))

; to load it whenever openning an appropriate file
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
;; an example of  mmm-mode on http://jblevins.org/log/mmm

(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

; yasnippet
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
	(backward-char 1)
	(if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
	    (null (do-yas-expand)))
	(if (check-expansion)
	    (company-complete-common)
	  (indent-for-tab-command)))))
; solving conflicts in Company and Yasnippet
(global-set-key [tab] 'tab-indent-or-complete)

; c, c++, and java mode
;; add .sig file as c source code
(setq auto-mode-alist (append '(("\\.sig$" . c-mode)) auto-mode-alist))
;; cc-mode is builtin, it has effect on c, c++, jave, etc.
(defun my-c-mode-common-hook ()
	     (which-function-mode t)  ; show function name in mode line

	     (setq c-default-style
	     	   '((java-mode . "java")
	     	     (awk-mode . "awk")
	     	     (c-mode . "k&r")
	     	     (c++-mode . "stroustrup")
	     	     (other . "linux")))
	     (setq c-basic-offset 2)
	     (setq tab-width 2)

	     (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

     	     (local-set-key (kbd "M-<right>") 'hs-show-block)
	     (local-set-key (kbd "M-<left>")  'hs-hide-block)
	     (local-set-key (kbd "M-<up>")    'hs-hide-all)
	     (local-set-key (kbd "M-<down>")  'hs-show-all)
	     (hs-minor-mode t) ; hide and show block with builtin feature Hideshow
 
	     (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
	     (ggtags-mode 1)))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

; c++ mode
;; (defun my-c++-mode-hook ()
;;    ; extemely useful for c++, help you expand the function headers defined in .h file to a .cpp file.
;;   (require 'member-functions) 
;;   (setq mf--source-file-extension "cpp"))
;; (add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;; git 
;(use-package magit
;  :defer t
;  :bind ("C-x g" . magit-status))

;(use-package git-gutter+
;  :defer t
;  :config
;  (global-git-gutter+-mode))

; org mode
(defvar org-plantuml-jar-path (expand-file-name "C:/bin/plantuml.jar"))

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images t t)
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
  )
(add-hook 'org-mode-hook 'my-org-mode)

; org publish
(setq org-publish-project-alist
      '(
	("org-notes"
	 :base-directory "~/blog/draft/"
	 :base-extension "org"
	 :publishing-directory "~/blog/publish/"
	 ;:publishing-directory "/ssh:user@server" ;export to server
	 :recursive t
	 :publishing-function my/org-html-publish-to-html)

	("org-static"                ;Used to publish static files
	 :base-directory "~/blog/draft"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "~/blog/publish/"
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
	(default-directory (concat default-directory "../temp"))
	(browse-url-chromium-program "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
	(browse-url-browser-function #'browse-url-chromium))
    (org-html-export-to-html)
    (browse-url (org-export-output-file-name ".html" nil default-directory))))

(provide 'init)
;;; init.el ends here
