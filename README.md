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

(golden-ratio-mode 1)
```

***

If you want to disable automatic resizing done by golden-ratio, just invoke
`M-x golden-ratio-mode`

To call golden ratio manually just `M-x golden-ratio`

## Wide Screens

If you use a large screen and have very wide frames golden-ratio makes very 
wide windows. This can be handled automatically by setting _golden-ratio-auto-scale_
to true. This does a good job of keeping windows at a reasonable width regardless of
how wide or narrow your frame size is. This works well on my laptop regardless of
which monitor or LCD I happen to be using.

`(setq golden-ratio-auto-scale t)` 

For those who wish for manual control,
If _golden-ratio-auto-scale_ is false, manual control can be exercised
through the _golden-ratio-adjust-factor_ variable.
setting it to something less than 1 will cause the windows to be less wide.
The golden-ratio-adjust function allows for experimentation with this value.

`M-x golden-ratio-adjust` 

It is also possible to toggle between widescreen and regular width window sizing
with

`M-x golden-ratio-toggle-widescreen`

The variable _golden-ratio-wide-adjust-factor_ can be set to the adjustment value 
you desire the widescreen toggle to use.

The following code will set up golden-ratio to adjust for a moderately wide screen
and also allow toggling between normal, with an adjustment factor of 1, and wide with
an adjustment factor of .8. For a very wide screen/frame of ~3400 px, .4 works well giving
screens with a width ~100 columns wide.

```elisp
(setq golden-ratio-adjust-factor .8
      golden-ratio-wide-adjust-factor .8)
```

## Credits

Code inspired by ideas from [Tatsuhiro Ujihisa](http://twitter.com/ujm)
