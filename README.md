- [Emacs-helper 使用说明](#org89c1d29)
  - [简介](#orgb4caf2a)
  - [使用方法](#orgfa77830)


<a id="org89c1d29"></a>

# Emacs-helper 使用说明


<a id="orgb4caf2a"></a>

## 简介

Emacs-helper 是 [Tumashu](https://github.com/tumashu) 同学的个人配置， 可以做为 Emacs 中文用户的一个参考。


<a id="orgfa77830"></a>

## 使用方法

1.  下载: [emacs-helper](https://github.com/tumashu/emacs-helper/archive/master.zip)
2.  解压缩到目录："d:/projects/emacs-packages/emacs-helper"
3.  将下面的代码粘贴到 "~/.emacs"

        (add-to-list 'load-path "d:/projects/emacs-packages/emacs-helper")
        (setq use-package-always-ensure t)
        (setq eh-enable-load-path-update nil)
        (setq eh-enable-full-install t)
        (require 'emacs-helper)
4.  重启 Emacs, 第一次加载 Emacs-helper 的时候，它依赖的包会自动安装。