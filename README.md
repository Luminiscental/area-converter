# area-converter

Small cli utility for converting drawing tablet areas, e.g. for when changing drivers. Basically just assuming a linear transform.

## usage

Once installed as `area-converter`, the executable can be used without any parameters; any input is asked for at runtime.
For example:

```
$ area-converter
What command would you like to run? (type "quit" to exit, "help" to display supported commands)
> convertArea
Please enter the old full area values
<left value> <top value> <right value> <bottom value>:
> 0 0 1000 1000
Please enter the old desired area values
<left value> <top value> <right value> <bottom value>:
> 23 27 720 410
Please enter the new full area values
<left value> <top value> <right value> <bottom value>:
> 0 800 600 0
I think the new desired area is:
(left, top) -> (right, bottom) : (23.000000, 333.400000) -> (441.200000, 27.000000)
What command would you like to run? (type "quit" to exit, "help" to display supported commands)
> quit
$
```

## installation

To build, first clone and enter the repository:

```
$ git clone https://github.com/Luminiscental/area-converter
$ cd area-converter
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
