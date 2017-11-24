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
  :commands org-mode
  :mode ("\\.org\\'" . org-mode)
  :mode ("\\.org_archive\\'" . org-mode)
  :ensure nil
  :config

  ;; 自定义变量
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
  (setq org-insert-heading-respect-content nil)
  (setq org-log-done t)
  (setq org-startup-indented nil)
  (setq org-edit-src-content-indentation 0)

  (defun eh-org-fill-paragraph ()
    "Fill org paragraph"
    (interactive)
    (let ((fill-column 10000000))
      (org-fill-paragraph)))

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

  (defun eh-org-visual-line-mode ()
    (setq visual-line-fringe-indicators '(nil nil))
    (visual-line-mode)
    (if visual-line-mode
        (setq word-wrap nil)))

  (defun eh-org-cdlatex ()
    (when (featurep 'cdlatex)
      (turn-on-org-cdlatex)))

  (add-hook 'org-mode-hook 'eh-org-cdlatex)
  (add-hook 'org-mode-hook 'eh-org-visual-line-mode)
  (add-hook 'org-mode-hook 'eh-org-smart-truncate-lines))

(use-package org-mime
  :after org
  :ensure nil)

(use-package org-bookmark
  :after org
  :ensure nil)

(use-package org-protocol
  :after org
  :ensure nil)

(use-package org-screenshot
  :after org
  :ensure nil)

(use-package ox
  :commands (org-export-as
             org-export-to-file)
  :ensure nil
  :config
  ;; Export language
  (setq org-export-default-language "zh-CN")
  (setq org-export-backends
        '(ascii beamer html latex md deck rss s5 odt))

  ;; org默认使用"_下标"来定义一个下标，使用"^上标"定义一个上标，
  ;; 但这种方式在中文环境中与下划线冲突。
  ;; 这里强制使用"_{下标}"来定义一个下标。"^{上标}"来定义一个上标。
  (setq org-export-with-sub-superscripts '{})
  (setq org-use-sub-superscripts '{}))

(use-package ox-html
  :after ox
  :ensure nil
  :config

  (setq eh-org-mathtoweb-file "~/bin/mathtoweb.jar")
  (setq org-latex-to-mathml-convert-command
        "java -jar %j -unicode -force -df %o %I"
        org-latex-to-mathml-jar-file
        eh-org-mathtoweb-file)

  (setq org-html-coding-system 'utf-8)
  (setq org-html-head-include-default-style t)
  (setq org-html-head-include-scripts t)

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

  (add-hook 'org-export-filter-headline-functions #'eh-org-wash-text)
  (add-hook 'org-export-filter-paragraph-functions #'eh-org-wash-text))

(use-package ox-latex
  :after ox
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
  :after ox-latex
  :ensure nil
  :config (org2ctex-toggle t))

(use-package ox-odt
  :after ox
  :ensure nil)

(use-package ox-ascii
  :after ox
  :ensure nil)

(use-package ox-beamer
  :after ox
  :ensure nil)

(use-package ox-md
  :after ox
  :ensure nil)

(use-package ox-deck
  :after ox
  :ensure nil)

(use-package ox-rss
  :after ox
  :ensure nil)

(use-package ox-s5
  :after ox
  :ensure nil)

(use-package ox-bibtex
  :after org
  :ensure nil)

;; (use-package ob-R
;;   :ensure nil)

;; org-plus-contrib
(use-package ox-extra
  :after org
  :ensure nil
  :config
  ;; 如果一个标题包含TAG: “ignore” ,导出latex时直接忽略这个标题，
  ;; 但对它的内容没有影响。
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package ox-bibtex-chinese
  :after ox
  :ensure nil
  :config (ox-bibtex-chinese-enable))

(use-package ob-core
  :commands (org-babel-execute-maybe
             org-babel-execute-safely-maybe)
  :ensure nil
  :config
  ;; org-bable设置
  (setq org-confirm-babel-evaluate nil)
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

  (defun eh-org-show-babel-image ()
    (when (not org-export-current-backend)
      (org-display-inline-images)))

  (add-hook 'org-babel-after-execute-hook #'eh-org-show-babel-image)
  (add-hook 'org-babel-after-execute-hook #'eh-org-align-babel-table))

(use-package ob-plantuml
  :after ob
  :ensure nil
  :config
  (setq org-plantuml-jar-path "~/bin/plantuml.jar"))

;; * Footer
(provide 'eh-org)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8-unix
;; End:

;;; eh-org.el ends here
