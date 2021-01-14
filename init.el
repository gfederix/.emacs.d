;; Global configurations
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(setq use-dialog-box '())

(setq use-package-verbose t)
;; Straight with use-package
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(setq settings-dir  (expand-file-name "settings" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; create empy file if it not exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; Font & Geometry
(dolist (opt '((font . "Inconsolata-14")
	       (line-spacing . 0)
	       (width . 90) ))
  (add-to-list 'default-frame-alist opt))
;; (if                                     ; for large and wide descktop
;;     (= (display-pixel-width) 1920)
;;     (add-to-list 'default-frame-alist '(font . "Inconsolata-10")))

;; (dolist (opt '((font . "Inconsolata-10") (line-spacing . 0) (width . 84) ))
;;   (add-to-list 'default-frame-alist opt))
;; (require 'highlight-beyond-fill-column)
;; (setq-default fill-column 80)

;; (add-hook 'prog-mode-hook 'highlight-beyond-fill-column)
;; (custom-set-faces '(highlight-beyond-fill-column-face
;;                     ((t (:foreground "red" )))))
;; (add-hook 'prog-mode-hook 'highlight-beyond-fill-column)

;; Theme
;; (use-package material-theme :straight t)
(use-package spacemacs-theme
  :straight t
  :ensure t
  :defer t
  :init (load-theme 'spacemacs-dark t))
;; (straight-use-package 'spacemacs-theme)
;; (load-theme 'spacemacs-dark t)

;; Numerate Window
;; Switching windows:
;; (use-package ace-window :straight t)
;; (global-set-key (kbd "M-o") 'ace-window)
;; (use-package switch-window :straight t)

(use-package highlight-beyond-fill-column
  :load-path "lisp")

(use-package use-ttf
  :straight t
  :init
  (setq use-ttf-default-ttf-fonts
	(directory-files
	 (expand-file-name "fonts" user-emacs-directory)
	 t "\\.[ot]tf"))
  (use-ttf-install-fonts))

(use-package winum
  :straight t				; C-w [number]
  :ensure t
  :init (winum-mode)
  :bind (:map winum-keymap
	 ("C-`" . winum-select-window-by-number)
	 ("M-1" . winum-select-window-1)
	 ("M-2" . winum-select-window-2)
	 ("M-3" . winum-select-window-3)
	 ("M-4" . winum-select-window-4)
	 ("M-5" . winum-select-window-5)
	 ("M-6" . winum-select-window-6)
	 ("M-7" . winum-select-window-7)
	 ("M-8" . winum-select-window-8)
	 ("M-9" . winum-select-window-9)
	 ("M-0" . winum-select-window-0-or-10)))

;; Parentes
(show-paren-mode)
(electric-pair-mode)
(use-package rainbow-delimiters
  :straight t
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package smartparens
  :straight t
  :ensure t
  :init
  (require 'smartparens-config)
  :hook (prog-mode . smartparens-mode)
  )

;; Line & Column numbers
(global-linum-mode)
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(line-number-mode)
(column-number-mode)

;; Save point position between sessions
(use-package saveplace
  :straight t
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "places" user-emacs-directory)))

;; Automatic key binding help
(use-package which-key
  :straight t
  :ensure t
  :init (which-key-mode))

;; Helm
(use-package helm
  :straight t
  :ensure t
  :init (helm-mode)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)))

(use-package helm-projectile
  :straight t
  :ensure t
  :requires (helm prjectile)
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))
(use-package helm-ag
  :straight t
  :config
  ;; (custom-set-variables
  ;;  '(helm-follow-mode-persistent t))
  )
;; Git
(use-package magit :straight t :ensure t)
(use-package projectile
  :straight t
  :ensure t
  :init (projectile-mode)
  :bind (( "s-p" . projectile-command-map)
	 ("C-c p" . projectile-command-map)))

;; Flycheck
(use-package flycheck
  :straight t
  :ensure t
  :init (global-flycheck-mode))

(use-package pyvenv :straight t :ensure t)
(use-package auto-virtualenv :straight t :ensure t
  :requires (pyvenv)
  :hook ((python-mode .  auto-virtualenv-set-virtualenv)
	 (window-configuration-change . auto-virtualenv-set-virtualenv)
	 (focus-in .  auto-virtualenv-set-virtualenv)
	 ))
;; Snippets
(use-package yasnippet
  :straight t
  :hook (prog-mode . yas-minor-mode))
(use-package yasnippet-snippets :straight t)


;; Python
;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
;; https://realpython.com/emacs-the-best-python-editor/
;; (use-package elpy
;;   :straight t
;;   :ensure t
;;   :init (elpy-enable))
(setq elpy-from-git nil)
(if elpy-from-git
    (progn
      (use-package company :straight t :ensure t)
      (use-package highlight-indentation :straight t :ensure t)
      (use-package pyvenv :straight t :ensure t)
      (use-package yasnippet :straight t :ensure t)
      (use-package s :straight t :ensure t)
      (add-to-list 'load-path "/home/federix/.emacs.d/elpy")
      (load "elpy")
      (load "elpy-rpc")
      (load "elpy-shell")
      (load "elpy-profile")
      (load "elpy-refactor")
      (load "elpy-django")
      (elpy-enable))
  (add-hook 'elpy-mode-hook
            (lambda () (local-set-key (kbd "M-.") 'elpy-goto-definition)))
;; 
  (use-package elpy
    :straight (elpy :fork (:host github :repo "gfederix/elpy" :branch "dev"))
    :config
    (elpy-enable)
    (when (load "flycheck" t t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode)) ;activate flycheck
    (add-hook
     'elpy-mode-hook
     (lambda () (local-set-key (kbd "M-.") 'elpy-goto-definition)))
    ))
(use-package ein :straight t :ensure t)
