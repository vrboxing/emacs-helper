;;; eh-org.el --- Tumashu's org-mode configuation

;; * Header
;; Copyright (c) 2012-2016, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.2

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; * 简介                                                  :README:
;;  这个文件是tumashu个人专用的emacs配置文件，emacs中文用户可以参考。

;;; Code:

;; * 代码                                                       :code:
(use-package org
  :ensure nil
  :config

  (defvar eh-org-directory
    (if (eq system-type 'windows-nt)
        "d:/org/"
      "~/org/"))

  (use-package ox
    :ensure nil
    :config
    ;; Export language
    (setq org-export-default-language "zh-CN"))

  (use-package ox-html
    :ensure nil
    :config
    ;; html
    (setq org-html-coding-system 'utf-8)
    (setq org-html-head-include-default-style t)
    (setq org-html-head-include-scripts t))

  (use-package ox-latex
    :ensure nil
    :config
    ;; 不要在latex输出文件中插入\maketitle
    (setq org-latex-title-command "")
    (setq org-latex-date-format "%Y-%m-%d")
    ;; (setq org-latex-create-formula-image-program 'imagemagick)  ;默认支持中文
    (setq org-latex-create-formula-image-program 'dvipng)          ;速度较快，但默认不支持中文
    (setq org-format-latex-options
          (plist-put org-format-latex-options :scale 2.5))
    (setq org-format-latex-options
          (plist-put org-format-latex-options :html-scale 2.5)))

  (use-package org2ctex
    :ensure nil
    :config (org2ctex-toggle t))

  (use-package ox-odt :ensure nil)
  (use-package ox-ascii :ensure nil)
  (use-package ox-beamer :ensure nil)
  (use-package ox-md :ensure nil)
  (use-package ox-deck :ensure nil)
  (use-package ox-rss :ensure nil)
  (use-package ox-s5 :ensure nil)
  (use-package org-mime :ensure nil)
  (use-package org-bookmark :ensure nil)
  (use-package org-protocol :ensure nil)
  (use-package org-screenshot :ensure nil)
  (use-package ox-bibtex :ensure nil)
  ;; (use-package ob-R :ensure nil)

  ;; org-plus-contrib
  (use-package ox-extra
    :ensure nil
    :config
    ;; 如果一个标题包含TAG: “ignore” ,导出latex时直接忽略这个标题，
    ;; 但对它的内容没有影响。
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))

  (use-package ox-bibtex-chinese
    :ensure nil
    :config (ox-bibtex-chinese-enable))

  (use-package ob-plantuml
    :ensure nil
    :config
    (setq org-plantuml-jar-path "~/bin/plantuml.jar"))

  (use-package parse-time
    :ensure nil
    :config
    (setq parse-time-months
          (append '(("yiy" . 1) ("ery" . 2) ("sany" . 3)
                    ("siy" . 4) ("wuy" . 5) ("liuy" . 6)
                    ("qiy" . 7) ("bay" . 8) ("jiuy" . 9)
                    ("shiy" . 10) ("shiyiy" . 11) ("shiery" . 12)
                    ("yiyue" . 1) ("eryue" . 2) ("sanyue" . 3)
                    ("siyue" . 4) ("wuyue" . 5) ("liuyue" . 6)
                    ("qiyue" . 7) ("bayue" . 8) ("jiuyue" . 9)
                    ("shiyue" . 10) ("shiyiyue" . 11) ("shieryue" . 12))
                  parse-time-months))
    (setq parse-time-weekdays
          (append '(("zri" . 0) ("zqi" . 0)
                    ("zyi" . 1) ("zer" . 2) ("zsan" . 3)
                    ("zsi" . 4) ("zwu" . 5) ("zliu" . 6)
                    ("zr" . 0) ("zq" . 0)
                    ("zy" . 1) ("ze" . 2) ("zs" . 3)
                    ("zsi" . 4) ("zw" . 5) ("zl" . 6))
                  parse-time-weekdays)))

  (use-package org-agenda
    :ensure nil
    :bind (("C-c a" . org-agenda))
    :config
    (setq org-agenda-span 'week)
    (setq org-agenda-window-setup 'only-window)
    (setq org-agenda-files `(,eh-org-directory))
    (setq org-agenda-include-diary nil)
    (setq org-agenda-format-date 'eh-org-agenda-format-date-aligned)

    (defun eh-org-agenda-format-date-aligned (date)
      (require 'cal-iso)
      (let* ((dayname (calendar-day-name date))
             (day (cadr date))
             (day-of-week (calendar-day-of-week date))
             (month (car date))
             (monthname (calendar-month-name month))
             (year (nth 2 date))
             (iso-week (org-days-to-iso-week
                        (calendar-absolute-from-gregorian date)))
             (weekyear (cond ((and (= month 1) (>= iso-week 52))
                              (1- year))
                             ((and (= month 12) (<= iso-week 1))
                              (1+ year))
                             (t year)))
             (cn-date (calendar-chinese-from-absolute
                       (calendar-absolute-from-gregorian date)))
             (cn-year (cadr cn-date))
             (cn-month (cl-caddr cn-date))
             (cn-day (cl-cadddr cn-date))
             (cn-month-name
              ["正月" "二月" "三月" "四月" "五月" "六月"
               "七月" "八月" "九月" "十月" "冬月" "腊月"])
             (cn-day-name
              ["初一" "初二" "初三" "初四" "初五" "初六" "初七" "初八" "初九" "初十"
               "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九"  "廿"
               "廿一" "廿二" "廿三" "廿四" "廿五" "廿六" "廿七" "廿八" "廿九" "三十"
               "卅一" "卅二" "卅三" "卅四" "卅五" "卅六" "卅七" "卅八" "卅九" "卅十"])
             (extra (format "(%s%s%s%s)"
                            (if (= day-of-week 1)
                                (aref cn-month-name (1-  (floor cn-month)))
                              "")
                            (if (= day-of-week 1)
                                (if (integerp cn-month) "" "[闰]")
                              "")
                            (aref cn-day-name (1- cn-day))
                            (if (= day-of-week 1)
                                (format "，第%02d周" iso-week)
                              ""))))
        (format "%04d-%02d-%02d %s %s"
                year month day dayname extra)))

    (use-package holidays
      :ensure nil
      :config
      (use-package cal-china-x
        :ensure nil
        :config
        (defvar eh-calendar-holidays nil)
        (setq eh-calendar-holidays
              '(;;公历节日
                (holiday-fixed 1 1 "元旦")
                (holiday-fixed 2 14 "情人节")
                (holiday-fixed 3 8 "妇女节")
                (holiday-fixed 3 14 "白色情人节")
                (holiday-fixed 4 1 "愚人节")
                (holiday-fixed 5 1 "劳动节")
                (holiday-fixed 5 4 "青年节")
                (holiday-float 5 0 2 "母亲节")
                (holiday-fixed 6 1 "儿童节")
                (holiday-float 6 0 3 "父亲节")
                (holiday-fixed 9 10 "教师节")
                (holiday-fixed 10 1 "国庆节")
                (holiday-fixed 12 25 "圣诞节")
                ;; 农历节日
                (holiday-lunar 1 1 "春节" 0)
                (holiday-lunar 1 2 "春节" 0)
                (holiday-lunar 1 3 "春节" 0)
                (holiday-lunar 1 15 "元宵节" 0)
                (holiday-solar-term "清明" "清明节")
                (holiday-solar-term "小寒" "小寒")
                (holiday-solar-term "大寒" "大寒")
                (holiday-solar-term "立春" "立春")
                (holiday-solar-term "雨水" "雨水")
                (holiday-solar-term "惊蛰" "惊蛰")
                (holiday-solar-term "春分" "春分")
                (holiday-solar-term "谷雨" "谷雨")
                (holiday-solar-term "立夏" "立夏")
                (holiday-solar-term "小满" "小满")
                (holiday-solar-term "芒种" "芒种")
                (holiday-solar-term "夏至" "夏至")
                (holiday-solar-term "小暑" "小暑")
                (holiday-solar-term "大暑" "大暑")
                (holiday-solar-term "立秋" "立秋")
                (holiday-solar-term "处暑" "处暑")
                (holiday-solar-term "白露" "白露")
                (holiday-solar-term "秋分" "秋分")
                (holiday-solar-term "寒露" "寒露")
                (holiday-solar-term "霜降" "霜降")
                (holiday-solar-term "立冬" "立冬")
                (holiday-solar-term "小雪" "小雪")
                (holiday-solar-term "大雪" "大雪")
                (holiday-solar-term "冬至" "冬至")
                (holiday-lunar 5 5 "端午节" 0)
                (holiday-lunar 8 15 "中秋节" 0)
                (holiday-lunar 7 7 "七夕情人节" 0)
                (holiday-lunar 12 8 "腊八节" 0)
                (holiday-lunar 9 9 "重阳节" 0)
                (holiday-lunar 12 22 "冬至" 0)))
        (setq calendar-holidays eh-calendar-holidays)))

    (use-package calendar
      :ensure nil
      :config
      (setq calendar-week-start-day 0) ; 一周第一天，0表示星期天, 1表示星期一
      (setq calendar-month-name-array
            ["一月" "二月" "三月" "四月" "五月" "六月"
             "七月" "八月" "九月" "十月" "十一月" "十二月"])
      (setq calendar-day-name-array
            ["星期日" "星期一" "星期二" "星期三" "星期四" "星期五" "星期六"])
      (defun eh-org-chinese-anniversary (year lunar-month lunar-day &optional mark)
        (if year
            (let* ((d-date (diary-make-date lunar-month lunar-day year))
                   (a-date (calendar-absolute-from-gregorian d-date))
                   (c-date (calendar-chinese-from-absolute a-date))
                   (cycle (car c-date))
                   (yy (cadr c-date))
                   (y (+ (* 100 cycle) yy)))
              (diary-chinese-anniversary lunar-month lunar-day y mark))
          (diary-chinese-anniversary lunar-month lunar-day year mark))))

    (use-package org-capture
      :ensure nil
      :bind (("C-c c" . org-capture))
      :config
      (setq org-capture-templates
            (let ((org-file (concat (file-name-as-directory eh-org-directory)
                                    "i-org.org")))
              `(("n" "Note" entry (file ,org-file)
                 "* %?\n%i\n %a")
                ("a" "Appointment" entry (file ,org-file)
                 "* %?\n  %t\n%i\n %a")
                ("s" "Schedule" entry (file ,org-file)
                 "* TODO %?\nSCHEDULED: %t\n%i\n %a")
                ("k" "Schedule" entry (file ,org-file)
                 "* TODO %?\nSCHEDULED: %t\n%i\n %a")
                ("d" "Deadline" entry (file ,org-file)
                 "* TODO %?\nDEADLINE: %t\n%i\n %a")
                ("t" "TODO" entry (file ,org-file)
                 "* TODO %?\n%i\n %a")
                ("A" "Anniversary" entry (file ,org-file)
                 "* 阳历生日提醒
:PROPERTIES:
:CATEGORY: 生日
:END:
\%\%%(or \"(org-anniversary 1985 4 17)\") 今天是%? %d 岁生日")
                ("C" "Chinese Anniversary" entry (file ,org-file)
                 "* 农历生日提醒
:PROPERTIES:
:CATEGORY: 生日
:END:
\%\%%(or \"(eh-org-chinese-anniversary 1985 4 17)\") 今天是%? %d 岁生日")
                ("D" "Diary" entry (file ,org-file)
                 "* 节假日提醒
:PROPERTIES:
:CATEGORY: 节假日
:END:
\%\%%(or \"(org-calendar-holiday)\")"))))))

  (use-package ob-core
    :ensure  nil
    :config
    (setq org-confirm-babel-evaluate nil))

  ;; 自定义变量
  (setq eh-org-mathtoweb-file "~/bin/mathtoweb.jar")
  (setq org-latex-to-mathml-convert-command
        "java -jar %j -unicode -force -df %o %I"
        org-latex-to-mathml-jar-file
        eh-org-mathtoweb-file)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
  (setq org-insert-heading-respect-content nil)
  (setq org-log-done t)
  (setq org-startup-indented nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-export-backends
        '(ascii beamer html latex md deck rss s5 odt))
  (add-to-list 'auto-mode-alist
               '("\.\(org\|org_archive\)$" . org-mode))

  ;; org默认使用"_下标"来定义一个下标，使用"^上标"定义一个上标，
  ;; 但这种方式在中文环境中与下划线冲突。
  ;; 这里强制使用"_{下标}"来定义一个下标。"^{上标}"来定义一个上标。
  (setq org-export-with-sub-superscripts '{})
  (setq org-use-sub-superscripts '{})

  ;; org-bable设置
  (setq org-src-fontify-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((org . t)
     ;; (R . t)
     (ditaa . nil)
     (dot . nil)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (mscgen . t)
     (latex . t)
     (ocaml . nil)
     (perl . t)
     (python . nil)
     (ruby . nil)
     (screen . nil)
     ;; (shell . nil)
     (sql . nil)
     (sqlite . nil)))

  ;; Use Cairo graphics device by default,which can get better graphics quality.
  ;; you shoule add below lines to you ~/.Rprofile
  ;;    require("Cairo")
  ;;    CairoFonts(regular="SimSun:style=Regular",
  ;;             bold="SimHei:style=Regular",
  ;;             italic="KaiTi_GB2312:style=Regular",
  ;;             symbol="Symbol")
  ;;
  ;; (setq org-babel-R-graphics-devices
  ;;   '((:bmp "bmp" "filename")
  ;;     (:jpg "jpeg" "filename")
  ;;     (:jpeg "jpeg" "filename")
  ;;     (:tikz "tikz" "file")
  ;;     (:tiff "tiff" "filename")
  ;;     (:png "CairoPNG" "filename")
  ;;     (:svg "CairoSVG" "file")
  ;;     (:pdf "CairoPDF" "file")
  ;;     (:ps "CairoPS" "file")
  ;;     (:postscript "postscript" "file")))

  ;; Add new easy templates
  (setq org-structure-template-alist
        (append '(("r" "#+BEGIN_SRC R\n?\n#+END_SRC")
                  ("e" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC")
                  ("ex" "#+BEGIN_EXAMPLE\n?\n#+END_EXAMPLE")
                  ("rh" "#+PROPERTY: header-args:R  :session *R* :tangle yes :colnames yes :rownames no :width 700 :height 500 :exports both")
                  ("rv" "#+BEGIN_SRC R :results value\n?\n#+END_SRC")
                  ("ro" "#+BEGIN_SRC R :results output verbatim\n?\n#+END_SRC")
                  ("rg" "#+BEGIN_SRC R :results graphics :file ?\n\n#+END_SRC")
                  ("rs" "#+BEGIN_SRC R :results output silent\n?\n#+END_SRC")
                  ("rd" "#+BEGIN_SRC R :colnames no :results value drawer\n`%c%` <- function(a,b){c(a,b)}\n?\n#+END_SRC"))
                org-structure-template-alist))

  (defun eh-org-fill-paragraph ()
    "Fill org paragraph"
    (interactive)
    (let ((fill-column 10000000))
      (org-fill-paragraph)))

  (defun eh-org-wash-text (text backend info)
    "导出 org file 时，删除中文之间不必要的空格。"
    (when (org-export-derived-backend-p backend 'html)
      (let ((regexp "[[:multibyte:]]")
            (string text))
        ;; org-mode 默认将一个换行符转换为空格，但中文不需要这个空格，删除。
        (setq string
              (replace-regexp-in-string
               (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
               "\\1\\2" string))
        ;; 删除粗体之后的空格
        (dolist (str '("</b>" "</code>" "</del>" "</i>"))
          (setq string
                (replace-regexp-in-string
                 (format "\\(%s\\)\\(%s\\)[ ]+\\(%s\\)" regexp str regexp)
                 "\\1\\2\\3" string)))
        ;; 删除粗体之前的空格
        (dolist (str '("<b>" "<code>" "<del>" "<i>" "<span class=\"underline\">"))
          (setq string
                (replace-regexp-in-string
                 (format "\\(%s\\)[ ]+\\(%s\\)\\(%s\\)" regexp str regexp)
                 "\\1\\2\\3" string)))
        string)))

  (defun eh-org-ctrl-c-ctrl-c (&optional arg)
    "根据光标处内容，智能折行，比如，在表格中禁止折行。"
    (interactive "P")
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      (pcase type
        ((or `table `table-cell `table-row `item `plain-list)
         (toggle-truncate-lines 1))
        (_ (toggle-truncate-lines -1))))
    (org-ctrl-c-ctrl-c arg))

  (defun eh-org-smart-truncate-lines (&optional arg)
    (interactive)
    (org-defkey org-mode-map "\C-c\C-c" 'eh-org-ctrl-c-ctrl-c))

  (defun eh-org-align-babel-table (&optional info)
    "Align all tables in the result of the current babel source."
    (interactive)
    (when (not org-export-current-backend)
      (let ((location (org-babel-where-is-src-block-result nil info)))
        (when location
          (save-excursion
            (goto-char location)
            (when (looking-at (concat org-babel-result-regexp ".*$"))
              (while (< (point) (progn (forward-line 1) (org-babel-result-end)))
                (when (org-at-table-p)
                  (toggle-truncate-lines 1)
                  (org-table-align)
                  (goto-char (org-table-end)))
                (forward-line))))))))

  (defun eh-org-visual-line-mode ()
    (setq visual-line-fringe-indicators '(nil nil))
    (visual-line-mode)
    (if visual-line-mode
        (setq word-wrap nil)))

  (defun eh-org-show-babel-image ()
    (when (not org-export-current-backend)
      (org-display-inline-images)))

  (defun eh-org-cdlatex ()
    (if (featurep 'cdlatex)
        (turn-on-org-cdlatex)
      (message "Fail to active org-cdlatex, you should load cdlatex first.")))

  (add-hook 'org-mode-hook 'eh-org-cdlatex)
  (add-hook 'org-mode-hook 'eh-org-visual-line-mode)
  (add-hook 'org-mode-hook 'eh-org-smart-truncate-lines)
  (add-hook 'org-babel-after-execute-hook #'eh-org-show-babel-image)
  (add-hook 'org-babel-after-execute-hook #'eh-org-align-babel-table)
  (add-hook 'org-export-filter-headline-functions #'eh-org-wash-text)
  (add-hook 'org-export-filter-paragraph-functions #'eh-org-wash-text)

  )

;; * Footer
(provide 'eh-org)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8-unix
;; End:

;;; eh-org.el ends here
