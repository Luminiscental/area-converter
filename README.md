# area-converter

Small cli utility for converting drawing tablet areas, e.g. for when changing drivers. Basically just assuming a linear transform.

## usage

Once built as `area-converter` the executable can be used without any parameters; any input is asked for at runtime.
For example:

```
$ area-converter
Please enter the old full area:
enter the dimensions as <left value> <top value> <right value> <bottom value>
0 0 1920 720
Please enter the old area:
enter the dimensions as <left value> <top value> <right value> <bottom value>
113 50 900 500
Please enter the new full area:
enter the dimensions as <left value> <top value> <right value> <bottom value>
0 1000 1000 0
I think the new area is:
(left, top) -> (right, bottom) : (58.854167, 930.555556) -> (468.750000, 305.555556)
```

## installation

Either use a released binary, or build yourself, either using `stack install` if you have stack installed, or run `Setup.hs`:

```
$ cd path/to/source
$ runhaskell Setup.hs build
$ runhaskell Setup.hs install
```
