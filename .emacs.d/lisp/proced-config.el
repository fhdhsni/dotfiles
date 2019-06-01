(use-package proced
  :ensure nil
  :commands proced
  :config
  (defun fhd/proced-format (a)
    (prin1 (car (split-string a))))
  (setq proced-grammar-alist
        (quote
         ((euid "EUID" "%d" right proced-< nil
                (euid pid)
                (nil t nil))
          (user "User" nil left proced-string-lessp nil
                (user pid)
                (nil t nil))
          (egid "EGID" "%d" right proced-< nil
                (egid euid pid)
                (nil t nil))
          (group "Group" nil left proced-string-lessp nil
                 (group user pid)
                 (nil t nil))
          (comm "Command" nil left proced-string-lessp nil
                (comm pid)
                (nil t nil))
          (state "Stat" nil left proced-string-lessp nil
                 (state pid)
                 (nil t nil))
          (ppid "PPID" "%d" right proced-< nil
                (ppid pid)
                ((lambda
                   (ppid)
                   (proced-filter-parents proced-process-alist ppid))
                 "refine to process parents"))
          (pgrp "PGrp" "%d" right proced-< nil
                (pgrp euid pid)
                (nil t nil))
          (sess "Sess" "%d" right proced-< nil
                (sess pid)
                (nil t nil))
          (ttname "TTY" proced-format-ttname left proced-string-lessp nil
                  (ttname pid)
                  (nil t nil))
          (tpgid "TPGID" "%d" right proced-< nil
                 (tpgid pid)
                 (nil t nil))
          (minflt "MinFlt" "%d" right proced-< nil
                  (minflt pid)
                  (nil t t))
          (majflt "MajFlt" "%d" right proced-< nil
                  (majflt pid)
                  (nil t t))
          (cminflt "CMinFlt" "%d" right proced-< nil
                   (cminflt pid)
                   (nil t t))
          (cmajflt "CMajFlt" "%d" right proced-< nil
                   (cmajflt pid)
                   (nil t t))
          (utime "UTime" proced-format-time right proced-time-lessp t
                 (utime pid)
                 (nil t t))
          (stime "STime" proced-format-time right proced-time-lessp t
                 (stime pid)
                 (nil t t))
          (time "Time" proced-format-time right proced-time-lessp t
                (time pid)
                (nil t t))
          (cutime "CUTime" proced-format-time right proced-time-lessp t
                  (cutime pid)
                  (nil t t))
          (cstime "CSTime" proced-format-time right proced-time-lessp t
                  (cstime pid)
                  (nil t t))
          (ctime "CTime" proced-format-time right proced-time-lessp t
                 (ctime pid)
                 (nil t t))
          (pri "Pr" "%d" right proced-< t
               (pri pid)
               (nil t t))
          (nice "Ni" "%3d" 3 proced-< t
                (nice pid)
                (t t nil))
          (thcount "THCount" "%d" right proced-< t
                   (thcount pid)
                   (nil t t))
          (start "Start" proced-format-start 6 proced-time-lessp nil
                 (start pid)
                 (t t nil))
          (vsize "VSize" "%d" right proced-< t
                 (vsize pid)
                 (nil t t))
          (rss "RSS" "%d" right proced-< t
               (rss pid)
               (nil t t))
          (etime "ETime" proced-format-time right proced-time-lessp t
                 (etime pid)
                 (nil t t))
          (pcpu "%CPU" "%.1f" right proced-< t
                (pcpu pid)
                (nil t t))
          (pmem "%Mem" "%.1f" right proced-< t
                (pmem pid)
                (nil t t))
          (args "Args" fhd/proced-format left proced-string-lessp nil
                (args pid)
                (nil t nil))
          (pid "PID" "%d" right proced-< nil
               (pid)
               ((lambda
                  (ppid)
                  (proced-filter-children proced-process-alist ppid))
                "refine to process children"))
          (tree "Tree" proced-format-tree left nil nil nil nil)))))
