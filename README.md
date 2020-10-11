# abl-helper

A little program to unify the output of multiple Marlin [G29
T](https://marlinfw.org/docs/gcode/G029-abl.html) commands. The output resembles the
report of the [M48](https://marlinfw.org/docs/gcode/M048.html) command. The program
is useful for diagnosing mechanical issues with a printer - while the M48 will
indicate any accuracy problems with the probe itself, it doesn't give you any
guarantee that the grid as a whole is accurate. Mechanical issues could result in
measured distances varying between different G29 runs. This program helps you
understand how big that variance is, and hopefully pinpoint the issue. Currently, the
program only works for cartesian printers.


## Usage
If you have a heated bed, set the temperature to the value you are usually printing
at. Then issue a `G29 T` command through a console (Pronterface, Octoprint). After the probing
procedure completes, a topology report will be generated. Repeat the procedure as
many times as you wish and paste these reports into a text file. Then pass the path of the
text file as an argument to `abl-helper` e.g. `abl-helper grids.txt`. The program
will automatically figure out the dimensions of the grid, whether it's 3x3, 5x5 or
something else entirely. Any extra text will be ignored.

In a nutshell, the program turns this:
```
Recv: Bilinear Leveling Grid:
Recv:       0      1      2
Recv:  0 +0.054 +0.022 +0.028
Recv:  1 +0.063 +0.018 +0.014
Recv:  2 +0.087 +0.042 +0.028
Recv:
Recv: Bilinear Leveling Grid:
Recv:       0      1      2
Recv:  0 +0.058 +0.019 +0.027
Recv:  1 +0.060 +0.014 +0.011
Recv:  2 +0.084 +0.035 +0.023
Recv:
Recv:       0      1      2
Recv:  0 +0.059 +0.022 +0.031
Recv:  1 +0.063 +0.018 +0.014
Recv:  2 +0.085 +0.037 +0.027
```

Into this:
```
Found 3 measurement grids...
┌─────────────────┬─────────────────┬─────────────────┐
│ Mean: +0.057000 │ Mean: +0.021000 │ Mean: +0.028667 │
│ Min:     +0.054 │ Min:     +0.019 │ Min:     +0.027 │
│ Max:     +0.059 │ Max:     +0.022 │ Max:     +0.031 │
│ Range:    0.005 │ Range:    0.003 │ Range:    0.004 │
│ SD:    0.002160 │ SD:    0.001414 │ SD:    0.001700 │
├─────────────────┼─────────────────┼─────────────────┤
│ Mean: +0.062000 │ Mean: +0.016667 │ Mean: +0.013000 │
│ Min:     +0.060 │ Min:     +0.014 │ Min:     +0.011 │
│ Max:     +0.063 │ Max:     +0.018 │ Max:     +0.014 │
│ Range:    0.003 │ Range:    0.004 │ Range:    0.003 │
│ SD:    0.001414 │ SD:    0.001886 │ SD:    0.001414 │
├─────────────────┼─────────────────┼─────────────────┤
│ Mean: +0.085333 │ Mean: +0.038000 │ Mean: +0.026000 │
│ Min:     +0.084 │ Min:     +0.035 │ Min:     +0.023 │
│ Max:     +0.087 │ Max:     +0.042 │ Max:     +0.028 │
│ Range:    0.003 │ Range:    0.007 │ Range:    0.005 │
│ SD:    0.001247 │ SD:    0.002944 │ SD:    0.002160 │
└─────────────────┴─────────────────┴─────────────────┘
```

For a printer without any outstanding mechanical issues, the standard deviation (`SD`)
value should be close to the one reported by the `M48` command.

## Installation
- Install [stack](https://docs.haskellstack.org/)
- Run `stack install` in the root directory of the project
