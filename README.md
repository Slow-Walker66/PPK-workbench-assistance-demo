# PPK-workbench-assistance-demo
这是一个小的demo，用来辅助没有pirana，又觉得psn用完后没有相关结果参考的定量药理建模者

#文件说明
## scripts
其中为两个R代码，主要是调用xpose，生成相应的图片，还有生成表格。另一个起到调用其他的作用

## reports
里面主要是一个报告参数的代码模版，后续都会根据这个模版来生成，大家可以自己改模版

## output
这是输出诊断图和参数表的地方，可以直接浏览

# 怎么使用
可以用任何IDE平台（VSCode，Positron等）打开文件夹（就能自动定位文件夹），然后用PsN的语法直接就能跑NONMEM，命令参考如下：
execute run1.mod -dir=run1; Rscript scripts/postprocess.R run1

*为了方便使用，我上传了一个PPK的demo，大家可以玩一玩*
Demo里面是华法林的一个case，大家可以玩一玩，如果有问题欢迎讨论！！！留下你的issue and star！！！
