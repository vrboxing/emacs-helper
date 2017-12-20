;;; eh-termux.el --- Tumashu's emacs termux configuation

;; * Header
;; Copyright (c) 2011-2016, Feng Shu

;; Author: Feng Shu <tumashu@gmail.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.3

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

;; * 代码                                                                 :code:

(defun eh-termux-enable ()
  (interactive)
  (setq-default
   mode-line-format
   (list " "
         mode-line-buffer-identification
         " "
         (propertize
          "[关闭]"
          'mouse-face 'mode-line-highlight
          'keymap
          (let ((map (make-sparse-keymap)))
            (define-key map [mode-line mouse-1] 'kill-this-buffer)
            map))
         " "
         (propertize
          "[保存]"
          'mouse-face 'mode-line-highlight
          'keymap
          (let ((map (make-sparse-keymap)))
            (define-key map [mode-line mouse-1] 'save-buffer)
            map))
         " "
         '(:eval
           (when (eq major-mode 'org-mode)
             (propertize
              "[C-c C-c]"
              'mouse-face 'mode-line-highlight
              'keymap
              (let ((map (make-sparse-keymap)))
                (define-key map [mode-line mouse-1] 'org-ctrl-c-ctrl-c)
                map)))))))

;; * Footer
(provide 'eh-termux)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8-unix
;; End:

;;; eh-termux.el ends here
