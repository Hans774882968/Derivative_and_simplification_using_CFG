[TOC]

# Derivative_and_simplification_using_CFG

用递归下降法生成抽象语法树，然后遍历抽象语法树实现求导和表达式化简。

v1.0不保证程序正确性：

1. 可以识别变量（和c语言规则相同），解析不带"^"（求幂）和括号的四则运算表达式。
2. 可以识别一部分靠前的连乘or连除或靠前的连加or连减，并合并。比如：`1*5*6+6*5*1`化简为`60`，但`1*5*6+6*5/5*2*3`化简为`30+30/(5)*2*3`。也就是说出现除法后，因为没做除法的简化模块，导致第一个子树返回的不是常数，于是后续的所有乘号都没法合并了。

“2”打算在v1.1里把抽象语法树的生成规则改造一下：现在是若干个减号只生成二叉树的形式，我们将升级为一个减号跟着若干子树，表示连减。其余符号也如此。这样就把连续的加或减看成了若干个区间，其中区间的第一个元素是特殊的。

正确性怎么保证？我的想法是，原先用深度越小（接近根）优先级越低来决定唯一的运算顺序，现在规定子树的`vector`也有运算顺序，这样抽象语法树仍然没有丢失运算顺序。理论上这一升级只需要改生成抽象语法树的模块，因为我一开始就是按多棵子树来写的表达式简化模块。

目前的实现里，除号相关的化简只是打了括号，等后续支持浮点数以后将优化它。

```cpp
_x*2/_y0_ - y_5*3 + x * y
Node type: 1 +
Node type: 1 -
Node type: 1 /
Node type: 1 *
Node type: 2 _x
Node type: 0 2
Node type: 2 _y0_
Node type: 1 *
Node type: 2 y_5
Node type: 0 3
Node type: 1 *
Node type: 2 x
Node type: 2 y

x*x/y*1/zz/1*x+1*1/2/1
// x*x/(y)/(zz)*x+1/(2)
3*54/1*3*1*1/2/1*1
// 486/(2)
0*x+x*0*2
// 0
1*x*1*y+1*xzzzz*0+_xx * zz0
// 1*x*y+_xx*zz0
2+0-0+21+9-5+0-0
// 27
1-1-1-2-3-4-5-6-7-8-9-10
// -55
1-1-1-2-3-4-5+30-6-7-8+40-9-10
// 15
```
