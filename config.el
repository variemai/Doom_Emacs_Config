;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.

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

(setq doom-font (font-spec :family "JetBrains Mono" :size 20 :weight 'Medium)
      doom-variable-pitch-font (font-spec :family "Jet Brains Mono" :size 20))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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
;;use zsh s-f key to open shell
(defvar my-term-shell "/bin/zsh")
  (defadvice ansi-term (before force-bash)
    (interactive (list my-term-shell)))
  (ad-activate 'ansi-term)
(global-set-key (kbd "C-x t") 'ansi-term)

;;fullscreen at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(pinentry evil-mu4e mu4e-alert mu4e-maildirs-extension mu4e-overview flycheck))
 '(send-mail-function 'smtpmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'rtags)
(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)

(use-package rtags
  :ensure t
  :hook (c++-mode . rtags-start-process-unless-running)
  :config (setq rtags-completions-enabled t
                rtags-path "/home/vardas/sources/rtags/src/rtags.el"
                rtags-rc-binary-name "/home/vardas/.local/bin/rc"
                rtags-use-helm t
                rtags-rdm-binary-name "/home/vardas/.local/bin/rdm")
  :bind (("C-c E" . rtags-find-symbol)
  ("C-c e" . rtags-find-symbol-at-point)
  ("C-c O" . rtags-find-references)
  ("C-c o" . rtags-find-references-at-point)
  ("C-c s" . rtags-find-file)
  ("C-c v" . rtags-find-virtuals-at-point)
  ("C-c F" . rtags-fixit)
  ("C-c f" . rtags-location-stack-forward)
  ("C-c b" . rtags-location-stack-back)
  ("C-c n" . rtags-next-match)
  ("C-c p" . rtags-previous-match)
  ("C-c P" . rtags-preprocess-file)
  ("C-c R" . rtags-rename-symbol)
  ("C-c x" . rtags-show-rtags-buffer)
  ("C-c T" . rtags-print-symbol-info)
  ("C-c t" . rtags-symbol-type)
  ("C-c I" . rtags-include-file)
  ("C-c i" . rtags-get-include-file-for-symbol)))
(setq rtags-display-result-backend 'helm)

(require 'pinentry)
(require 'mu4e)
(require 'smtpmail)
;; General settings for mu4e
(setq mail-user-agent 'mu4e-user-agent)
;; (setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -a" emacs-version)
      epa-pinentry-mode 'ask)
(pinentry-start)
(setq mu4e-update-interval 120)
(setq mu4e-headers-auto-update t)
(setq mu4e-compose-signature-auto-include nil)
(setq mu4e-maildir (expand-file-name "~/Maildir"))
(setq mu4e-compose-in-new-frame t)
(setq mu4e-change-filenames-when-moving t)
(setq smtpmail-queue-mail nil)  ;; start in normal mode
(setq mu4e-attachment-dir  "~/Downloads")
(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)
(setq mu4e-view-show-addresses 't)
(defun my-mu4e-html2text (msg)
;;My html2text function; shows short message inline, show
;;long messages in some external browser (see `browse-url-generic-program')
  (let ((html (or (mu4e-message-field msg :body-html) "")))
    (if (> (length html) 20000)
      (progn
	(mu4e-action-view-in-browser msg)
	"[Viewing message in external browser]")
      (mu4e-shr2text msg))))
(setq mu4e-html2text-command 'my-mu4e-html2text)

(setq mu4e-sent-messages-behavior 'delete)
;; mu4e contexts for the different email addresses
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'always-ask)
(setq mu4e-contexts
      (list
      (make-mu4e-context
          :name "Forth"
          :enter-func (lambda () (mu4e-message "Entering Forth context"))
          :leave-func (lambda () (mu4e-message "Leaving Forth context"))
          :match-func (lambda (msg)
		 (when msg
		    (mu4e-message-contact-field-matches
		     msg '(:from :to :cc :bcc) "vardas@ics.forth.gr")))
          :vars '( ( user-mail-address . "vardas@ics.forth.gr")
                   ( user-full-name . "Ioannis Vardas")
                   ( message-send-mail-function . smtpmail-send-it )
                   ( smtpmail-smtp-user . "vardas@ics.forth.gr")
                   (gnutls-min-prime-bits . 128)
                   (smtpmail-stream-type . ssl)
                   (smtpmail-starttls-credentials . (("enigma.ics.forth.gr" 465 nil nil)))
                   (smtpmail-default-smtp-server . "enigma.ics.forth.gr")
                   (smtpmail-smtp-server . "enigma.ics.forth.gr")
	                 (smtpmail-auth-credentials  . '(expand-file-name "~/.authinfo.gpg") )
                   (smtpmail-smtp-service . 465)
                   (smtpmail-debug-info . t)
                   (smtpmail-debug-verbose . t)
                   (mu4e-drafts-folder . "/forth/Drafts")
                   (mu4e-sent-folder   . "/forth/Sent")
                   (mu4e-refile-folder . "/forth/Archive")
                   (mu4e-trash-folder . "/forth/Trash")
                   ))

      (make-mu4e-context
       :name "Hotmail"
       :enter-func (lambda () (mu4e-message "Entering Hotmail context"))
       :leave-func (lambda () (mu4e-message "Leaving Hotmail context"))
       :match-func (lambda (msg) (when msg (mu4e-message-contact-field-matches
                                            msg '(:from :to :cc :bcc) "johnvardas@hotmail.com")))
       :vars '( ( user-mail-address . "johnvardas@hotmail.com")
                ( user-full-name . "Ioannis Vardas")
                ( message-send-mail-function . smtpmail-send-it )
                ( smtpmail-smtp-user . "johnvardas@hotmail.com")
                (gnutls-min-prime-bits . 256)
                (smtpmail-stream-type . starttls)
                (smtpmail-starttls-credentials . (("smtp-mail.outlook.com" 587 nil nil)))
                (smtpmail-default-smtp-server . "smtp-mail.outlook.com")
                (smtpmail-smtp-server . "smtp-mail.outlook.com")
	              (smtpmail-auth-credentials  . '(expand-file-name "~/.authinfo.gpg") )
                (smtpmail-smtp-service . 587)
                ;; (smtpmail-debug-info . t)
                ;; (smtpmail-debug-verbose . t)
                (mu4e-trash-folder  . "/hotmail/Deleted")
                (mu4e-drafts-folder . "/hotmail/Drafts")
                (mu4e-sent-folder   . "/hotmail/Sent")
                (mu4e-refile-folder . "/hotmail/Archive")
                )))
      )
(require 'mu4e-alert)
(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook 'mu4e-alert-enable-notifications)
(require 'beacon)
(setq beacon-mode 1)
                ;; (smtpmail-auth-credentials     . '(("smtp-mail.outlook.com" 587 "johnvardas@hotmail.com" nil)))
