# golden-ratio.el

When working with many windows at the same time, each window has a size
that is not convenient for editing.

golden-ratio helps on this issue by resizing automatically the windows you are
working on to the size specified in the "Golden Ratio". The window that has the
main focus will have the perfect size for editing, while the ones that are
not being actively edited will be re-sized to a smaller size that doesn't get
in the way, but at the same time will be readable enough to know it's content.

![Golden Ratio](https://raw.github.com/roman/golden-ratio.el/assets/golden_ratio_el.gif)

For more info about the golden ratio check out

http://en.wikipedia.org/wiki/Golden_ratio

## Install

Use [el-get](https://github.com/dimitri/el-get) to install.

## Usage

To enable automatic resizing, put on your .emacs.d/init.el

```elisp
(require 'golden-ratio)

(golden-ratio-enable)
```

***

If you want to disable automatic resizing done by golden-ratio, just invoke
`(golden-ratio-disable)`

To call golden ratio manually just `M-x golden-ratio`

## Credits

Code inspired by ideas from [Tatsuhiro Ujihisa](http://twitter.com/ujm)
