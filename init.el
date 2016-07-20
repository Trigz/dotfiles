(setq auto-save-default nil)

;; install packages
(setq package-list '(evil evil-leader flycheck))
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
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

;; flycheck
(global-flycheck-mode)

;; helm
(add-to-list 'load-path "/Users/nick/.emacs-plugins/emacs-async")
(add-to-list 'load-path "/Users/nick/.emacs-plugins/helm")
(require 'helm-config)

;; evil mode
(require 'evil)
(evil-mode 1)

(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "u" 'undo-tree-visualize
  "ss" 'evil-window-vsplit
  "su" 'evil-window-split
  "cw" 'delete-window
  "cb" (lambda () (interactive) (kill-buffer nil))
  "n" 'next-normal-buffer
  "p" 'prev-normal-buffer)

(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map "  " 'helm-mini)

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
