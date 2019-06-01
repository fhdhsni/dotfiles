(use-package ivy-hydra
  :defer t
  :config
  (defhydra hydra-apropos (:color blue)
    "Apropos"
    ("a" apropos "apropos")
    ("c" apropos-command "cmd")
    ("d" apropos-documentation "doc")
    ("e" apropos-value "val")
    ("l" apropos-library "lib")
    ("o" apropos-user-option "option")
    ("u" apropos-user-option "option")
    ("v" apropos-variable "var")
    ("i" info-apropos "info")
    ("t" tags-apropos "tags")
    ("z" hydra-customize-apropos/body "customize"))

  (defhydra hydra-customize-apropos (:color blue)
    "Apropos (customize)"
    ("a" customize-apropos "apropos")
    ("f" customize-apropos-faces "faces")
    ("g" customize-apropos-groups "groups")
    ("o" customize-apropos-options "options"))

  (global-set-key
   (kbd "C-x 9")
   (defhydra hydra-unicode (:hint nil)
     "
          Unicode  _e_ ê  _o_ ô  _i_ î  _a_ â  _u_ û _I_ ï  _U_ ü  _E_ ë  _A_ œ "
     ("e" (insert "ê"))
     ("o" (insert "ô"))
     ("i" (insert "î"))
     ("a" (insert "â"))
     ("u" (insert "û"))
     ("I" (insert "ï"))
     ("U" (insert "ü"))
     ("E" (insert "ë"))
     ("A" (insert "œ"))))

  (global-set-key
   (kbd "C-, C-f")
   (defhydra find-stuff (:hint nil)
     "
        _b_ash-find  _f_zf  _c_ounsel-file-jump
        _d_ired-jump _l_ocate(current dir) _L_ocate(counsel default)
        "
     ("b" (fhd/counsel-bash-find))
     ("f" (fhd/counsel-fzf))
     ("c" (counsel-file-jump))
     ("d" (counsel-dired-jump))
     ("l" (fhd/counsel-locate))
     ("L" (counsel-locate))))

  (bind-key "C-c q"
            (defhydra hydra-quickrun (:color teal
                                             :hint nil)
              "
_u_: compile + run     _c_: compile file                _s_: execute buffer in eshell
_r_: execute region    _e_: execute + replace region    _a_: execute with args
_q_: quit
"
              ("u" quickrun)
              ("r" quickrun-region)
              ("e" quickrun-replace-region)
              ("c" quickrun-compile-only)
              ("a" quickrun-with-arg)
              ("s" quickrun-shell)
              ("q" nil :color blue)))
  (global-set-key
   (kbd "C-c C-,")
   (defhydra launch-apps (:hint nil)
     "
         _c_hrome  _a_nki  _o_kular _g_oldendict _t_ermite t_e_legram "
     ("c" (start-process-shell-command "chromium" nil "chromium") :exit t)
     ("a" (start-process-shell-command "anki" nil "anki") :exit t)
     ("o" (start-process-shell-command "okular" nil "okular") :exit t)
     ("g" (start-process-shell-command "goldendict" nil "goldendict") :exit t)
     ("t" fhd/open-in-terminal :exit t)
     ("e" (start-process-shell-command "telegram-desktop" nil "telegram-desktop") :exit t))))
