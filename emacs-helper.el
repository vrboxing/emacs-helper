;;; emacs-helper.el --- An emacs config collection for Non programmers

;; * Header
;; Copyright 2016 Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/emacs-helper
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; * emacs-helper 使用说明                                          :README:doc:
;; ** 简介
;; emacs-helper 是 [[https://github.com/tumashu][Tumashu]] 同学的个人配置，
;; 可以做为 emacs 中文用户的一个参考。

;; ** 使用
;; 将下面的代码粘贴到 "~/.emacs" , 然后 `eval-buffer', emacs-helper 加载时，
;; 其依赖的包会通过 use-package 自动安装。

;; #+BEGIN_EXAMPLE
;; (add-to-list 'load-path "d:/projects/emacs-packages/emacs-helper")
;; (setq use-package-always-ensure t)
;; (setq eh-enable-load-path-update nil)
;; (require 'emacs-helper)
;; #+END_EXAMPLE

;; * 代码                                                                 :code:
;; #+BEGIN_SRC emacs-lisp
(load-library "eh-basic")
(load-library "eh-org")
(load-library "eh-bbdb3")
(load-library "eh-complete")
(load-library "eh-functions")
(load-library "eh-elfeed")
(load-library "eh-misc")
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'emacs-helper)

;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:

;;; emacs-helper.el ends here
;; #+END_SRC
