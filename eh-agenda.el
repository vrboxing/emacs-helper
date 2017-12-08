;;; eh-agenda.el --- Tumashu's org-agenda configuation

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
(use-package parse-time
  :after org-agenda
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

(use-package cal-china-x
  :after org-agenda
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
          (holiday-fixed 10 24 "程序员节")
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
  (setq calendar-holidays eh-calendar-holidays))

(use-package calendar
  :after org-agenda
  :ensure nil
  :config
  (setq calendar-week-start-day 0) ; 一周第一天，0表示星期天, 1表示星期一
  (setq calendar-month-name-array
        ["一月" "二月" "三月" "四月" "五月" "六月"
         "七月" "八月" "九月" "十月" "十一月" "十二月"])
  (setq calendar-day-name-array
        ["周日" "周一" "周二" "周三" "周四" "周五" "周六"])

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

(use-package org-archive
  :after org-agenda
  :ensure nil
  :config
  (setq org-archive-default-command 'org-archive-set-tag))

(use-package org-attach
  :after org-agenda
  :ensure nil
  :config
  (setq org-attach-commit nil))

(use-package autorevert
  :after org-agenda
  :config
  (add-hook 'org-mode-hook #'turn-on-auto-revert-mode))

(use-package org-agenda
  :bind (("C-c a" . org-agenda)
         :map org-agenda-mode-map
         ("g" . eh-org-agenda-redo-all))
  :ensure nil
  :config

  (defvar eh-org-directory
    (if (eq system-type 'windows-nt)
        "d:/org/org-files"
      "~/org/org-files"))

  (defun eh-revert-org-buffers ()
    "Refreshes all opened org buffers."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name)
                   (string-match-p "org$" (buffer-file-name))
                   (file-exists-p (buffer-file-name))
                   (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed all opened org files."))

  (defun eh-org-agenda-redo-all (&optional exhaustive)
    (interactive "P")
    (eh-revert-org-buffers)
    (funcall-interactively #'org-agenda-redo-all)
    (message (substitute-command-keys
              "刷新完成，记得按快捷键 '\\[org-save-all-org-buffers]' 来保存更改。")))

  (setq org-agenda-span 'week)
  (setq org-agenda-window-setup 'only-window)
  (setq org-agenda-files `(,eh-org-directory))
  (setq org-agenda-include-diary nil)

  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-time-leading-zero nil)

  (setq org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          ""
          "----------------"))

  (setq  org-agenda-current-time-string
         "now - - - - - - - - - - - - - - - - - - - - - - - - -")

  (setq org-agenda-prefix-format
        '((agenda  . " %-6t%s")
          (todo  . " %i")
          (tags  . " %i")
          (search . " %i")))

  (setq org-agenda-scheduled-leaders
        '("预 " "应%02d天前开始 "))

  (setq org-agenda-deadline-leaders
        '("止 " "过%02d天后到期 " "已经过期%02d天 "))

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
             "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九" "二十"
             "廿一" "廿二" "廿三" "廿四" "廿五" "廿六" "廿七" "廿八" "廿九" "三十"
             "卅一" "卅二" "卅三" "卅四" "卅五" "卅六" "卅七" "卅八" "卅九" "卅十"])
           (extra (format "(%s%s%s%s)"
                          (if (or (eq org-agenda-current-span 'day)
                                  (= day-of-week 1)
                                  (= cn-day 1))
                              (aref cn-month-name (1-  (floor cn-month)))
                            "")
                          (if (or (= day-of-week 1)
                                  (= cn-day 1))
                              (if (integerp cn-month) "" "[闰]")
                            "")
                          (aref cn-day-name (1- cn-day))
                          (if (or (= day-of-week 1)
                                  (eq org-agenda-current-span 'day))
                              (format "，第%02d周" iso-week)
                            ""))))
      (format "%04d-%02d-%02d %s %s"
              year month day dayname extra))))

(use-package org-capture
  :after org-agenda
  :ensure nil
  :bind (("C-c c" . org-capture)
         :map org-capture-mode-map
         ("C-c c" . org-capture-finalize)
         ("C-c k" . org-capture-kill)
         ("C-c w" . org-capture-refile))
  :config

  (add-hook 'org-capture-mode-hook #'eh-org-capture-mode-hook)

  (defun eh-org-capture-mode-hook ()
    (setq-local header-line-format
                (list "Capture: "
                      (propertize
                       "1.完成 "
                       'mouse-face 'mode-line-highlight
                       'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key map [header-line mouse-1] 'org-capture-finalize)
                         map))
                      (propertize
                       "2.丢弃 "
                       'mouse-face 'mode-line-highlight
                       'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key map [header-line mouse-1] 'org-capture-kill)
                         map))
                      (propertize
                       "3.Refile"
                       'mouse-face 'mode-line-highlight
                       'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key map [header-line mouse-1] 'org-capture-refile)
                         map)))))

  (setq org-capture-templates
        (let ((gtd-file (concat (file-name-as-directory eh-org-directory) "i-gtd.org")))
          `(("n" "Note" entry (file ,gtd-file)
             "* %?\n%i")
            ("a" "Appointment" entry (file ,gtd-file)
             "* %?\n  %t\n%i")
            ("s" "Schedule" entry (file ,gtd-file)
             "* TODO %?\nSCHEDULED: %t\n%i")
            ("k" "Schedule" entry (file ,gtd-file)
             "* TODO %?\nSCHEDULED: %t\n%i")
            ("d" "Deadline" entry (file ,gtd-file)
             "* TODO %?\nDEADLINE: %t\n%i")
            ("t" "TODO" entry (file ,gtd-file)
             "* TODO %?\n%i")
            ("A" "Anniversary" plain (file+headline ,gtd-file "阳历生日")
             "\%\%%(or \"(org-anniversary 1985 4 17)\") 今天是%?%d阳历岁生日")
            ("C" "Chinese Anniversary" plain (file+headline ,gtd-file "农历生日")
             "\%\%%(or \"(eh-org-chinese-anniversary 1985 4 17)\") 今天是%?%d岁农历生日")
            ("D" "Diary" plain (file+headline ,gtd-file "节假日")
             "\%\%%(or \"(org-calendar-holiday)\")")))))


;; * Footer
(provide 'eh-agenda)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8-unix
;; End:

;;; eh-agenda.el ends here
