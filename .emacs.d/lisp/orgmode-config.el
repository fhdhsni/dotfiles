(use-package org-mode
  :mode "\\.org\\'"
  :defer 8
  :ensure org
  :bind (("C-c l" . org-store-link)
         ("C-c C-," . counsel-org-goto)
         ("C-c C-x" . nil)
         ("C-c I" . org-insert-link))
  :config
  ;; (define-key org-mode-map (kbd "C-c C-x") nil)
  ;; (define-key org-mode-map (kbd "C-c C-,") 'counsel-org-goto)
  ;; (define-key org-mode-map (kbd "C-c I") 'org-insert-link)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  ;; (setq org-goto-interface 'outline)
  ;; (require 'ob-js)
  (setq org-modules
        '(org-bbdb
         org-bibtex
         org-docview
         org-gnus
         org-info
         org-irc
         org-mhe
         org-rmail
         org-w3m))
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-agenda-files (list "~/Dropbox/org/agenda.org"))
  (setq org-todo-keywords '((type
                             "TODO(t)"
                             "WAITING(w)"
                             "NEXT(n)"
                             "IN-PROGRESS(i)"
                             "SOMEDAY(s)"
                             "|"
                             "CANCELED(c)"
                             "DONE(d)")))

  (setq org-tag-alist '(("@WATCH" . ?w)
                        ("@READ" . ?r)
                        ("@PERSONAL" . ?p)
                        ("@MERGE" . ?m)
                        ("@CODE" . ?c)
                        ("@DRUPZ" . ?d)
                        ("@DRYE" . ?r)
                        ("@PHONE" . ?h)
                        ("@BUG" . ?g)
                        ("@URGENT" . ?e)))

  (add-hook 'org-mode-hook (lambda()
                             (company-mode)
                             (hl-line-mode -1)
                             (setq org-agenda-custom-commands
                                   (quote
                                    (("n" "Agenda and all TODOs"
                                      ((agenda "" nil)
                                       (alltodo "" nil))
                                      nil)
                                     ("i" "Important: Urgent things and phone calls"
                                      ((tags "@URGENT"
                                             ((org-agenda-overriding-header "Urgents things to do")))
                                       (tags "@PHONE"
                                             ((org-agenda-overriding-header "Phone calls to make"))))
                                      nil nil))))

                             (set (make-local-variable 'electric-indent-functions)
                                  (list (lambda(org) 'no-indent)))))
  (setq org-src-fontify-natively t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (java . nil)
     (shell . t)
     (js . t))))
