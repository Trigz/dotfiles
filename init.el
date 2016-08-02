(setq auto-save-default nil)
(setq make-backup-files nil)
(menu-bar-mode -1)
(setq show-paren-delay 0)
(show-paren-mode 1)

(add-hook 'before-save-hook 'whitespace-cleanup)

;; install packages
(setq package-list '(evil evil-leader flycheck company powerline
			  color-theme-solarized rust-mode racer
			  nlinum dired+ haskell-mode ghc company-ghc
			  csharp-mode omnisharp projectile helm-projectile
			  flycheck-rust))

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'mouse)
(xterm-mouse-mode t)
(global-set-key [mouse-4] (lambda ()
			    (interactive)
			    (scroll-down 1)))
(global-set-key [mouse-5] (lambda ()
			    (interactive)
			    (scroll-up 1)))
(defun track-mouse (e))

;; theme
(load-theme 'solarized t)

;; flycheck
(global-flycheck-mode)

;; line numbers
(require 'nlinum)
(setq nlinum-format "%d \u2502")
(global-nlinum-mode)

;; powerline
(require 'powerline)
(powerline-center-evil-theme)

;; dired
(require 'dired+)
(toggle-diredp-find-file-reuse-dir 1)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "RET")
	      (lambda () (interactive) (find-alternate-file)))))

;; helm
(add-to-list 'load-path "/Users/nick/.emacs-plugins/emacs-async")
(add-to-list 'load-path "/Users/nick/.emacs-plugins/helm")
(require 'helm-config)

;; projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; evil mode
(require 'evil)
(evil-mode 1)

(eval-after-load "evil"
  '(progn
     (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
     (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
     (define-key evil-normal-state-map "  " 'projectile-find-file)
     (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
     (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
     (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)))

(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "u" 'undo-tree-visualize
  "ss" 'evil-window-vsplit
  "su" 'evil-window-split
  "cw" 'delete-window
  "cb" (lambda () (interactive) (kill-buffer nil))
  "d" (lambda () (interactive) (dired nil))
  "n" 'next-normal-buffer
  "p" 'prev-normal-buffer)

;; company
(require 'company)
(setq company-tooltip-align-annotations t)
(setq company-idle-delay .3)
(setq company-minimum-prefix-length 2)

;; rust
(require 'rust-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq rust-format-on-save t)

;; haskell
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook 'company-mode)
(add-to-list 'company-backends 'company-ghc)

;; dotnet
(require 'cl)
(require 'csharp-mode)
(require 'omnisharp)
(setq omnisharp-eldoc-support nil)
(setq omnisharp-server-executable-path "/Users/nick/src/omnisharp/OmniSharp")
(add-to-list 'company-backends 'company-omnisharp)
(add-hook 'csharp-mode-hook (lambda ()
			      (interactive)
			      (company-mode)
			      (omnisharp-mode)))

(eval-after-load "omnisharp"
  '(progn
     (evil-define-key 'normal omnisharp-mode-map (kbd "<f12>") 'omnisharp-go-to-definition)
     (evil-define-key 'normal omnisharp-mode-map (kbd "g u") 'omnisharp-helm-find-usages)
     (evil-define-key 'normal omnisharp-mode-map (kbd "g I") 'omnisharp-find-implementations) ; g i is taken
     (evil-define-key 'normal omnisharp-mode-map (kbd "g o") 'omnisharp-go-to-definition)
     (evil-define-key 'normal omnisharp-mode-map (kbd "g r") 'omnisharp-run-code-action-refactoring)
     (evil-define-key 'normal omnisharp-mode-map (kbd "g f") 'omnisharp-fix-code-issue-at-point)
     (evil-define-key 'normal omnisharp-mode-map (kbd "g F") 'omnisharp-fix-usings)
     (evil-define-key 'normal omnisharp-mode-map (kbd ", i") 'omnisharp-current-type-information)
     (evil-define-key 'normal omnisharp-mode-map (kbd ", I") 'omnisharp-current-type-documentation)
     (evil-define-key 'normal omnisharp-mode-map (kbd "r r") 'omnisharp-rename)
     (evil-define-key 'normal omnisharp-mode-map (kbd ",rl") 'omnisharp-build-in-emacs)

     (evil-define-key 'normal omnisharp-mode-map (kbd "f f") (lambda ()
							       (interactive)
							       (omnisharp-code-format)
							       (indent-region (point-min) (point-max))))
     (evil-define-key 'normal omnisharp-mode-map (kbd ",rt")
       (lambda() (interactive) (omnisharp-unit-test-single)))
     (evil-define-key 'normal omnisharp-mode-map
       (kbd ",rf")
       (lambda() (interactive) (omnisharp-unit-test-fixture)))
     (evil-define-key 'normal omnisharp-mode-map
       (kbd ",ra")
       (lambda() (interactive) (omnisharp-unit-test-all)))))

(defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
      (interactive)
	(if (and delete-selection-mode transient-mark-mode mark-active)
		  (setq deactivate-mark  t)
	      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
		  (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(defun next-normal-buffer ()
  (interactive)
  (run-until-not-special (lambda () (next-buffer))))

(defun prev-normal-buffer ()
  (interactive)
  (run-until-not-special (lambda () (previous-buffer))))

(defun is-special-buffer ()
  (char-equal ?* (aref (buffer-name) 0)))

(defun run-until-not-special (cmd)
  (let ((bcrumb (buffer-name)))
	(funcall cmd)
	(while (and
		(not (string= bcrumb (buffer-name)))
		(is-special-buffer))
	  (funcall cmd))))

(put 'dired-find-alternate-file 'disabled nil)
