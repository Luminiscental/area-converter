# area-converter

Small cli utility for converting drawing tablet areas, e.g. for when changing drivers. Basically just assuming a linear transform.

## usage

Once installed as `area-converter`, the executable can be used without any parameters; any input is asked for at runtime.
For example:

```
$ area-converter
Area values are taken as <left value> <top value> <right value> <bottom value>
Please enter the old full area:
> 0 0 1920 720
Please enter the old area:
> 113 50 900 500
Please enter the new full area:
> 0 1000 1000 0
I think the new area is:
(left, top) -> (right, bottom) : (58.854167, 930.555556) -> (468.750000, 305.555556)
```

## installation

To build, first clone the repository:

```
$ git clone https://github.com/Luminiscental/area-converter
```

The project uses [stack](https://docs.haskellstack.org/en/stable/README/), so you will need that to compile from source:

```
$ stack build
```

After the executable has been built you can run it in the source directory:

```
$ stack run
```

Or install it:

```
$ stack install
```
