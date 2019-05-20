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
113 50 800 900
Please enter the new full area:
enter the dimensions as <left value> <top value> <right value> <bottom value>
0 1000 1000 0
I think the new area is:
(left, top) -> (right, bottom) : (58.854167, 930.555556) -> (416.666667, -250.000000)
$ 
```
