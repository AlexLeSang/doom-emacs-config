;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq fancy-splash-image "~/.doom.d/banner/emacs.png")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Oleksandr Halushko"
      user-mail-address "alexlesang@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'zenburn)
;; (setq doom-theme 'hc-zenburn)
;; (setq doom-theme 'doom-one-light)
;; (setq doom-theme 'modus-vivendi)
;; (setq doom-theme 'modus-operandi)

(require 'dash)
(setq doom-theme
      (let ((time (car (-select-by-indices '(2) (decode-time (current-time))))))
        (if (and (< time 19) (> time 7))
            'modus-operandi
          'modus-vivendi)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type 'relative)
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;; (setq doom-font (font-spec :family "Source Code Pro" :size 15))

(if (string= (system-name) "halushko-VirtualBox")
    (setq doom-font (font-spec :family "Source Code Pro" :size 14))
  (setq doom-font (font-spec :family "Source Code Pro" :size 14)))

(setq gc-cons-threshold (* 4 1024 1024 1024))
(setq gc-cons-percentage 0.2)
(setq truncate-lines nil)

;; spacemacs keybindings
(setq evil-escape-key-sequence "fd")
(setq-default evil-disable-insert-state-bindings t)
(map! :i "C-y" #'yank)
(map! :i "C-k" #'kill-line)

(load! "~/.doom.d/modules/spacemacs/+spacemacs")

(after! ivy
  ;; switch to buffer
  (map! :map evil-normal-state-map :g "<C-tab>" #'switch-to-buffer)
  (setq +ivy-buffer-preview t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))))

(after! ivy-posframe
  (setq ivy-posframe-border-width 1))

;; Remove evil inhibit
(after! evil-escape
  (setq evil-escape-inhibit-functions nil))

;; Colors
(add-hook! '(emacs-lisp-mode-hook
             c-mode-hook
             c++-mode-hook
             cmake-mode-hook
             compilation-mode-hook
             shell-mode-hook
             js2-minor-mode
             python-mode-hook
             makefile-mode-hook
             adoc-mode-hook)
           #'rainbow-delimiters-mode
           #'rainbow-identifiers-mode)
;; (add-hook! emacs-lisp-mode-hook #'color-identifiers-mode)

;; company settings
(setq compleiton-delay 0.8
      completion-box-doc-delay (* compleiton-delay 2))

(after! company
  (setq company-idle-delay compleiton-delay
        company-minimum-prefix-length 3
        company-show-numbers nil
        company-tooltip-idle-delay 3
        company-tooltip-timer 3)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))


(after! company-box-doc
  (setq company-box-doc-delay completion-box-doc-delay))

(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; (after! ivy-posframe
;;   ; Set frame position
;;   (setf (alist-get t ivy-posframe-display-functions-alist)
;;         #'ivy-posframe-display-at-frame-top-center))

(add-hook 'window-setup-hook #'toggle-frame-maximized)

;; magit-todos uses hl-todo-keywords
(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("HACK"  . ,(face-foreground 'warning))
          ("TEMP"  . ,(face-foreground 'warning))
          ("DONE"  . ,(face-foreground 'success))
          ("NOTE"  . ,(face-foreground 'success))
          ("DONT"  . ,(face-foreground 'error))
          ("DEBUG"  . ,(face-foreground 'error))
          ("FAIL"  . ,(face-foreground 'error))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX"   . ,(face-foreground 'error))
          ("XXXX"  . ,(face-foreground 'error)))))

(after! fish-completion
  (setq fish-completion-fallback-on-bash-p nil))

(after! eshell
  (setq +eshell-enable-new-shell-on-split nil
        +eshell-kill-window-on-exit nil)

  (defun my/toggle-shell-auto-completion-based-on-path ()
    "Deactivates automatic completion on remote paths.
Retrieving completions for Eshell blocks Emacs. Over remote
connections the delay is often annoying, so it's better to let
the user activate the completion manually."
    (if (file-remote-p default-directory)
        (setq-local company-idle-delay nil)
      (setq-local company-idle-delay compleiton-delay)))

  (add-hook 'eshell-directory-change-hook #'my/toggle-shell-auto-completion-based-on-path)

  (defun my/eshell-auto-end ()
    "Move point to end of current prompt when switching to insert state."
    (when (and (eq major-mode 'eshell-mode)
               ;; Not on last line, we might want to edit within it.
               (not (>= (point) eshell-last-output-end))
               ;; Not on the last sent command if we use smart-eshell so we can
               ;; edit it.
               )
      (goto-char (point-max))))

  (add-hook 'evil-insert-state-entry-hook #'my/eshell-auto-end)
  (add-hook 'evil-hybrid-state-entry-hook #'my/eshell-auto-end)

  ;; eshell-mode imenu index
  (defun eshell/e (file) (find-file file))
  (defun eshell/up (dir) (eshell-up dir))
  (defun eshell/pk (dir) (eshell-up-peek dir))
  (defun eshell/md (dir) (eshell/mkdir dir) (eshell/cd dir))

  ;; This is a key-command
  (defun my/eshell-clear-keystroke ()
    "Allow for keystrokes to invoke eshell/clear"
    (interactive)
    (eshell/clear-scrollback)
    (eshell-send-input))

  (defun my/ivy-eshell-history ()
    (interactive)
    (counsel-esh-history)
    (evil-insert-state))

  (defun my/init-ivy-eshell ()
    "Initialize ivy-eshell."
    (progn
      (map! :map eshell-mode-map :i "<tab>" #'completion-at-point)
      (map! :map eshell-mode-map :in "C-l" nil)
      (map! :map eshell-mode-map :in "C-l" #'my/eshell-clear-keystroke)
      ))

  (add-hook 'eshell-mode-hook #'my/init-ivy-eshell)

  (remove-hook 'eshell-mode-hook #'+eshell-remove-fringes-h)
  (remove-hook 'eshell-mode-hook #'hide-mode-line-mode))

(set-eshell-alias!
 "e"  "find-file $1"
 "ff" "find . -type f -iname \"*$1*\""
 "ff" "find . -type f -name \"*$1*\""
 "g"  "grep \"$*\""
 "l"  "ls -lah \"$*\""
 "l"  "ls -lah \"$1\""
 "sd" "svn diff $*"
 "st" "svn st -q $*")

;; evil snipe
(after! evil-snipe
  (setq evil-snipe-scope 'whole-visible))

;; lsp
(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-signature-auto-activate nil
        lsp-file-watch-threshold nil
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics nil))

(after! lsp-ui-peek
  (setq lsp-ui-peek-enable nil))

;; info-colors
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook #'info-colors-fontify-node)


(use-package! vlf-setup
  :defer-incrementally vlf vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff)

;; abbrev completion
(after! abbrev
  (setq-default abbrev-mode t)
  (setq abbrev-file-name (expand-file-name "abbrev.el" doom-private-dir)
        save-abbrevs 'silently)

  (defun tec/set-text-mode-abbrev-table ()
    (if (derived-mode-p 'text-mode)
        (setq local-abbrev-table org-mode-abbrev-table)))

  (add-hook 'abbrev-mode-hook #'tec/set-text-mode-abbrev-table))

;; Extend mode config
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile-build\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("conanfile.txt" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.prf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.ks\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.repo\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.as$" . js-mode))

;; (defun remove-dos-eol ()
;;   "Do not show ^M in files containing mixed UNIX and DOS line endings."
;;   (interactive)
;;   (setq buffer-display-table (make-display-table))
;;   (aset buffer-display-table ?\^M []))

;; (add-hook 'c++-mode-hook #'remove-dos-eol)


(after! avy
  (setq avy-all-windows 'all-frames
        avy-timeout-seconds 0.8))

(after! all-the-icons
  (setq all-the-icons-scale-factor 1.0))

;; modeline
(after! doom-modeline
  (setq doom-modeline-height 20
        doom-modeline-buffer-encoding nil
        doom-modeline-gnus nil
        doom-modeline-gnus-timer 0
        doom-modeline-irc nil))

;; company capf completion
(after! company-capf
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (yas-expand))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (check-expansion)
          (company-complete-common)
        (indent-for-tab-command))))

  (map! :map evil-insert-state-map :ig "C-n" nil)
  (map! :map evil-insert-state-map :ig "C-n" #'+company/dabbrev)
  (map! :map evil-insert-state-map :ig "C-p" nil)
  (map! :map evil-insert-state-map :ig "C-p" #'company-yasnippet)
  (map! :map evil-insert-state-map :ig "<tab>" nil)
  (map! :map evil-insert-state-map :ig "<tab>" #'tab-indent-or-complete))


;; Arduino
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))


;; clang-format
(defun init-clang-format-buffer ()
  (progn
    (map! :map (c-mode-map c++-mode-map)
          (:localleader
           :desc "clang-format buffer" "==" #'clang-format-buffer
           :desc "clang-format buffer" "=b" #'clang-format-buffer
           :desc "clang-format region" "=r" #'clang-format-region))))

(add-hook 'c++-mode-hook #'init-clang-format-buffer)
(add-hook 'c-mode-hook #'init-clang-format-buffer)

;; highlighting symbols
(after! auto-highlight-symbol
  (setq spacemacs--symbol-highlight-transient-state-doc
        (concat
         spacemacs--symbol-highlight-transient-state-doc
         "  Search: [_s_] swiper  [_b_] buffers  [_f_] files  [_/_] project"))
  (spacemacs/transient-state-register-add-bindings 'symbol-highlight
    '(("s" swiper-thing-at-point :exit t)
      ("b" swiper-all-thing-at-point :exit t)
      ("f" spacemacs/search-auto-region-or-symbol :exit t)
      ("/" spacemacs/search-project-auto-region-or-symbol :exit t)
      ))

  (defun my/expand-symbol-highligh-mode ()
    (spacemacs|define-transient-state symbol-highlight
      :title "Symbol Highlight Transient State"
      :hint-is-doc t
      :dynamic-hint (spacemacs//symbol-highlight-ts-doc)
      :before-exit (spacemacs//ahs-ts-on-exit)
      :bindings
      ("d" ahs-forward-definition)
      ("D" ahs-backward-definition)
      ("e" spacemacs/ahs-to-iedit :exit t)
      ("n" spacemacs/quick-ahs-forward)
      ("N" spacemacs/quick-ahs-backward)
      ("p" spacemacs/quick-ahs-backward)
      ("R" ahs-back-to-start)
      ("r" ahs-change-range)
      ("z" (progn (recenter-top-bottom)
                  (spacemacs/symbol-highlight)))
      ("q" nil :exit t)))

  (add-hook 'auto-highlight-symbol-mode-hook #'my/expand-symbol-highligh-mode)
  )
;; search maps
(map! :map doom-leader-map "/" #'spacemacs/search-project-auto)
(map! :map doom-leader-search-map "p" #'spacemacs/search-project-auto)

(map! :map doom-leader-map "sf" #'spacemacs/search-auto)
(map! :map doom-leader-search-map "f" #'spacemacs/search-auto)

;; (map! :map evil-normal-state-map :g "gs" #'swiper)

;; (map! :map doom-leader-map "sb" #'swiper)
;; (map! :map doom-leader-search-map "b" #'swiper)

(after! projectile
  (setq projectile-svn-command "fd -0 -t f"
        projectile-generic-command "fd -0 -t f"))


;; minibuffer performance improvements
(setq default-gc-cons-threshold gc-cons-threshold)

(defun set-gc-to-max ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-gc ()
  (setq gc-cons-threshold default-gc-cons-threshold))

(add-hook 'minibuffer-setup-hook #'set-gc-to-max)
(add-hook 'minibuffer-exit-hook #'restore-gc)

(add-hook 'evil-insert-state-entry-hook #'set-gc-to-max)
(add-hook 'evil-insert-state-exit-hook #'restore-gc)

;; (setq garbage-collection-messages t)

;; ivy posframe hook
(defun my/ivy-toggle-gc ()
  (if (eq default-gc-cons-threshold gc-cons-threshold)
      (set-gc-to-max)
    (progn
      (restore-gc)
      (garbage-collect)))
  )

(add-hook 'ivy-posframe-mode-hook #'my/ivy-toggle-gc)

;; Reduce flickering with which-key delay
(after! which-key
  (setq which-key-idle-delay completion-box-doc-delay
        which-key-idle-secondary-delay (* 3 compleiton-delay)))



(use-package! ggtags
  :defer t
  :init (progn
          (defvar gtags-enable-by-default t
            "Whether or not to enable ggtags-mode.")

          (defun spacemacs/ggtags-mode-enable ()
            "Enable ggtags and eldoc mode.

For eldoc, ggtags advises the eldoc function at the lowest priority
so that if the major mode has better support it will use it first."
            (when gtags-enable-by-default
              (ggtags-mode 1)
              (eldoc-mode 1)))
          )
  :hook ((c-mode c++-mode) . spacemacs/ggtags-mode-enable)
  )

(use-package! counsel-gtags
  :defer t
  :init
  (progn
    (setq counsel-gtags-ignore-case t
          counsel-gtags-auto-update t)
    )
  :hook ((c-mode c++-mode) . counsel-gtags-mode))

(defun init-gtags ()
   (progn
     (map! :map (c-mode-map c++-mode-map)
           (:localleader
            :desc "gtags dwim" "gg" #'counsel-gtags-dwim
            :desc "gtags dwim" "gr" #'counsel-gtags-find-reference
            :desc "gtags dwim" "gd" #'counsel-gtags-find-definition
            :desc "gtags dwim" "gb" #'counsel-gtags-go-backward
            :desc "gtags dwim" "gf" #'counsel-gtags-go-forward))))

(add-hook 'c++-mode-hook #'init-gtags)
(add-hook 'c-mode-hook #'init-gtags)

(after! electric
  (setq electric-indent-mode nil))

(add-hook 'c++-mode-hook '(lambda ()(setq electric-indent-mode nil)))
(add-hook 'c-mode-hook '(lambda ()(setq electric-indent-mode nil)))


(after! counsel
  (setq counsel-rg-base-command '("rg" "-m" "150" "-M" "240" "--with-filename" "--smart-case" "--no-heading" "--line-number" "--color" "never" "%s")))


;; friendly eshell
(use-package! friendly-shell-command
  :defer t)

(use-package! friendly-shell
  :defer t)

(use-package! friendly-remote-shell
  :defer t)

(use-package! friendly-tramp-path
  :defer t
  :after tramp)

(when (string= (system-name) "halushko-VirtualBox")
  (load! "~/.doom.d/modules/sgs/+sgs"))

(after! lsp-mode
  (map! :map (c-mode-map c++-mode-map)
        (:localleader
         :desc "Rename" "R" #'lsp-rename)))

(after! ccls
  (setq ccls-executable (expand-file-name "~/bin/ccls.sh")))

;; (after! ccls-semantic-highlight
;;   (setq ccls-sem-highlight-method 'overlay)
;;   (defun ccls-overlay-hook ()
;;     (progn
;;       (ccls-use-default-rainbow-sem-highlight)))

;;   (add-hook 'c++-mode-hook #'ccls-overlay-hook)
;;   (add-hook 'c-mode-hook #'ccls-overlay-hook))

(after! flycheck
  (setq flycheck-display-errors-delay 3
        flycheck-idle-change-delay 3
        flycheck-idle-buffer-switch-delay 3
        flycheck-display-error-at-point-timer 1))

;; Improve visibility for logs
(defun log-mode ()
  "Settins for log analysis mode"
  (interactive)
  (progn
    (prog-mode)
    (rainbow-identifiers-mode)))

(after! spell-fu-mode
  (setq spell-check-delay 3
   spell-fu-idle-delay spell-check-delay)

  (defun disable-spell-check ()
    (setq spell-fu-idle-delay 300))

  (defun enable-spell-check ()
    (setq spell-fu-idle-delay spell-check-delay))

  (add-hook 'evil-insert-state-entry-hook #'disable-spell-check)
  (add-hook 'evil-insert-state-exit-hook #'enable-spell-check))

;; vc configuration
(after! vc-dir
  (version-control/init-vc))

;; TODO
;; TODO custom faster modeline
;;      - remove git/svn version
;;      - remove name of the major mmode
;; TODO add ~,= and < as text object for evil mode
;; TODO version control for svn from spacemacs
;; TODO fix performance problems for avy jump line
;; TODO counsel jump directory to open file in subdirectory
;; TODO consistent save of eshell history

;; (setq modus-themes-mode-line 'borderless)
(setq modus-themes-mode-line 'borderless-moody)
(setq x-underline-at-descent-line t)
(setq modus-themes-completions 'opinionated)
(setq modus-themes-lang-checkers 'subtle-foreground-straight-underline)
(setq modus-themes-hl-line 'accented-background)
(setq modus-themes-diffs 'deuteranopia)
(setq modus-themes-org-blocks 'rainbow)

;; adoc-mode
(autoload 'adoc-mode "adoc-mode" nil t)
(add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
(add-hook 'adoc-mode-hook (lambda() (setq buffer-face-mode nil)))

(after! lsp-mode
  (setq lsp-clients-elixir-server-executable "elixir-ls"))

(message "Done loading config.el")
