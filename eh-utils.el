;;; eh-utils.el --- A org-mode utils manager for Chinese users

;; * Header
;; Copyright (c) 2016, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/emacs-helper.git
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; * eh-utils README                                 :README:

;; ** 介绍
;; eh-utils 是一个小工具管理器，当前包含的小工具：

;; 1. 将 org 文件导出为 HTML 文件时，删除不必要的空格。
;; 2. 按 'C-c C-c', 根据当前内容智能折行。
;; 3. 如果 org-babel 结果中包含表格时，对表格进行对齐处理。
;; 4. ...

;; ** 激活
;; #+BEGIN_EXAMPLE
;; (require 'org)
;; (require 'eh-utils)
;; (eh-utils-enable)
;; #+END_EXAMPLE

;; ** 设置
;; 运行下面的命令后，会弹出一个选择器，用户用鼠标或者回车选择需要激活的 utils 就可以了。

;; #+BEGIN_EXAMPLE
;; M-x eh-utils
;; #+END_EXAMPLE

;; ** 管理个人 utils
;; 用户可以使用 eh-utils 管理自己的小工具，比如：

;; #+BEGIN_EXAMPLE
;; (add-hook 'org-mode-map 'my-hello-world)
;; (defun my-hello-world ()
;;   (message "Hello world!"))
;; #+END_EXAMPLE

;; 可以转换为 eh-utils 配置：

;; #+BEGIN_EXAMPLE
;; (push '(hello-world
;;         :document "Hello world!"
;;         :function my-hello-world
;;         :hook org-mode-hook)
;;       eh-utils-config)
;; #+END_EXAMPLE

;;; Code:
;; * Code                                                                 :code:
(require 'cl-lib)
(require 'cus-edit)

(defgroup eh-utils nil
  "Emacs-helper utils."
  :group 'org)

(defcustom eh-utils-config nil
  "A utils list that can be enabled or disabled by `eh-utils'.

A utils is a plist, which form is like:

  (NAME :document DOC :function FUNC :hook HOOK)

1. NAME is a symbol, which can be passed to `eh-utils-activate'
   or `eh-utils-deactivate'.
2. FUNC is a function which will be added to HOOK.
3. DOC will be showed in eh-utils chooser."
  :group 'eh-utils)

(defconst eh-utils-buildin-config
  '((clean-paragraph-space
     :document "Org 文件导出为 HTML 文件时，删除中文段落中多余的空格。"
     :function eh-utils:clean-useless-space
     :hook org-export-filter-paragraph-functions)
    (clean-headline-space
     :document "Org 文件导出为 HTML 文件时，删除中文标题中多余的空格。"
     :function eh-utils:clean-useless-space
     :hook org-export-filter-headline-functions)
    (align-babel-table
     :document "让 org-babel 运行结果中包含的 org 表格对齐。"
     :function eh-utils:align-babel-table
     :hook org-babel-after-execute-hook)
    (smart-truncate-lines
     :document "按 'C-c C-c' 快捷键时，根据光标处的内容智能折行，（禁用后需重启 emacs）。"
     :function eh-utils:smart-truncate-lines
     :hook org-mode-hook)
    (show-babel-image
     :document "让 org-babel 运行结果中包含的图片链接自动显示。"
     :function eh-utils:show-babel-image
     :hook org-babel-after-execute-hook)
    (visual-line-mode
     :document "打开 org 文件时，激活 visual-line-mode, （禁用后需重启 emacs）。"
     :function eh-utils:visual-line-mode
     :hook org-mode-hook)
    (org-cdlatex
     :document "打开 org 文件时，激活 cdlatex 功能。"
     :function eh-utils:org-cdlatex
     :hook org-mode-hook))
  "This variable include buildin utils, which is similar to `eh-utils-config'.")

(defvar eh-utils-enabled-utils
  '(clean-paragraph-space
    align-babel-table
    smart-truncate-lines
    show-babel-image
    visual-line-mode)
  "The utils of eh-utils which will be activated.")

(defvar eh-utils-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map (make-composed-keymap widget-keymap
                                                 special-mode-map))
    (suppress-keymap map)
    (define-key map "\C-x\C-s" 'eh-utils-save-setting)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    map)
  "Keymap for `eh-utils-mode'.")

(define-derived-mode eh-utils-mode special-mode "EH-UTILS"
  "Major mode for selecting eh-utils.
Do not call this mode function yourself.  It is meant for internal use."
  (use-local-map eh-utils-mode-map)
  (custom--initialize-widget-variables)
  (set (make-local-variable 'revert-buffer-function)
       (lambda (_ignore-auto noconfirm)
         (when (or noconfirm (y-or-n-p "Discard current choices? "))
           (eh-utils (current-buffer))))))
(put 'eh-utils-mode 'mode-class 'special)

;;;###autoload
(defun eh-utils (&optional buffer)
  (interactive)
  (switch-to-buffer (get-buffer-create (or buffer "*eh-utils chooser*")))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (eh-utils-mode)
  (setq truncate-lines t)
  (widget-insert "Type RET or click to enable/disable utils of eh-utils.\n\n")
  (widget-create 'push-button
                 :tag " Save settings! "
                 :help-echo "Save the selected utils for future sessions."
                 :action 'eh-utils-save-setting)
  (widget-insert "\n\nAvailable utils of eh-utils:\n\n")
  (let ((help-echo "mouse-2: Enable this utils for this session")
        (config-list (eh-utils-return-all-config))
        widget)
    (dolist (utils (mapcar 'car config-list))
      (setq widget (widget-create 'checkbox
                                  :value (memq utils eh-utils-enabled-utils)
                                  :utils-name utils
                                  :help-echo help-echo
                                  :action 'eh-utils-checkbox-toggle))
      (widget-create-child-and-convert widget 'push-button
                                       :button-face-get 'ignore
                                       :mouse-face-get 'ignore
                                       :value (format " %s " utils)
                                       :action 'widget-parent-action
                                       :help-echo help-echo)
      (widget-insert " -- " (plist-get (cdr (assq utils config-list))
                                       :document)
                     ?\n)))
  (goto-char (point-min))
  (widget-setup))

(defun eh-utils-return-all-config ()
  `(,@eh-utils-config ,@eh-utils-buildin-config))

(defun eh-utils-save-setting (&rest _ignore)
  (interactive)
  (customize-save-variable
   'eh-utils-enabled-utils eh-utils-enabled-utils)
  (message "Setting of eh-utils is saved."))

(defun eh-utils-checkbox-toggle (widget &optional event)
  (let ((utils (widget-get widget :utils-name)))
    (widget-toggle-action widget event)
    (if (widget-value widget)
        (progn (push utils eh-utils-enabled-utils)
               (eh-utils-activate (list utils)))
      (setq eh-utils-enabled-utils
            (remove utils eh-utils-enabled-utils))
      (eh-utils-deactivate (list utils)))))

;;;###autoload
(defun eh-utils-activate (utils-list)
  "Activate certain utils of eh-utils.

UTILS-LIST should be a list of utils which should be activated."
  (dolist (utils utils-list)
    (let* ((plist (cdr (assq utils (eh-utils-return-all-config))))
           (fn (plist-get plist :function))
           (hook (plist-get plist :hook)))
      (when (and fn hook)
        (add-hook hook fn)))))

;;;###autoload
(defun eh-utils-deactivate (&optional utils-list)
  "Deactivate certain utils of eh-utils.

This function is the opposite of `eh-utils-deactive'.  UTILS-LIST
should be a list of utils which should be activated."
  (let* ((config-list (eh-utils-return-all-config))
         (utils-list (or utils-list (mapcar 'car config-list))))
    (dolist (utils utils-list)
      (let* ((plist (cdr (assq utils config-list)))
             (fn (plist-get plist :function))
             (hook (plist-get plist :hook)))
        (when (and fn hook)
          (remove-hook hook fn))))))

;;;###autoload
(defun eh-utils-enable ()
  "Enable all eh-utils."
  (interactive)
  (eh-utils-deactivate)
  (eh-utils-activate eh-utils-enabled-utils))

(defun eh-utils:clean-useless-space (text backend info)
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

(defun eh-utils:smart-truncate-lines (&optional arg)
  (interactive)
  (org-defkey org-mode-map "\C-c\C-c" 'eh-utils:ctrl-c-ctrl-c))

(defun eh-utils:ctrl-c-ctrl-c (&optional arg)
  "根据光标处内容，智能折行，比如，在表格中禁止折行。"
  (interactive "P")
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    (pcase type
      ((or `table `table-cell `table-row `item `plain-list)
       (toggle-truncate-lines 1))
      (_ (toggle-truncate-lines -1))))
  (org-ctrl-c-ctrl-c arg))

(defun eh-utils:align-babel-table (&optional info)
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

(defun eh-utils:visual-line-mode ()
  (setq visual-line-fringe-indicators '(nil nil))
  (visual-line-mode)
  (if visual-line-mode
      (setq word-wrap nil)))

(defun eh-utils:show-babel-image ()
  (when (not org-export-current-backend)
    (org-display-inline-images)))

(defun eh-utils:org-cdlatex ()
  (if (featurep 'cdlatex)
      (turn-on-org-cdlatex)
    (message "Fail to active org-cdlatex, you should load cdlatex first.")))


;; * Footer
(provide 'eh-utils)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; eh-utils.el ends here
