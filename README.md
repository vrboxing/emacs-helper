- [Emacs-helper 使用说明](#emacs-helper-使用说明)
  - [简介](#简介)
  - [使用方法](#使用方法)

# Emacs-helper 使用说明<a id="orgheadline3"></a>

## 简介<a id="orgheadline1"></a>

Emacs-helper 是 [Tumashu](https://github.com/tumashu) 同学的个人配置，可以做为 Emacs 中文用户的一个参考。

## 使用方法<a id="orgheadline2"></a>

1.  下载: [emacs-helper](https://github.com/tumashu/emacs-helper/archive/master.zip)
2.  解压缩到目录："d:/projects/emacs-packages/emacs-helper"
3.  将下面的代码粘贴到 "~/.emacs"

        (add-to-list 'load-path "d:/projects/emacs-packages/emacs-helper")
        (setq use-package-always-ensure t)
        (setq eh-enable-load-path-update nil)
        (setq eh-enable-full-install t)
        (require 'emacs-helper)
4.  重启 Emacs, 第一次加载 Emacs-helper 的时候，它依赖的包会自动安装。
