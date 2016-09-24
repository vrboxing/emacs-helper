- [emacs-helper 使用说明](#emacs-helper-使用说明)
  - [简介](#简介)
  - [使用](#使用)

# emacs-helper 使用说明<a id="orgheadline3"></a>

## 简介<a id="orgheadline1"></a>

emacs-helper 是 [Tumashu](https://github.com/tumashu) 同学的个人配置，可以做为 emacs 中文用户的一个参考。

## 使用<a id="orgheadline2"></a>

将下面的代码粘贴到 "~/.emacs" , 然后 \`eval-buffer', emacs-helper 加载时，其依赖的包会通过 use-package 自动安装。

    (add-to-list 'load-path "d:/projects/emacs-packages/emacs-helper")
    (setq use-package-always-ensure t)
    (require 'emacs-helper)
