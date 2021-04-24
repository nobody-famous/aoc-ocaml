# Advent Of Code Using OCaml

My solution for Advent Of Code using the OCaml language.

## 2019

### Day 1

Nothing to see here. Part 1 is some straight forward math and part 2 is calling that math recursively.

### Day 2

Pretty straight forward again, implemented a small virtual machine with 2 op codes.

Part 2 had to brute force finding inputs that gave the expected output.

### Day 3

Had to find where 2 wires cross. May algorithm was

1. convert the direction input into x, y deltas
2. convert the deltas into a list of lines
3. for each line in wire 1, check if it crosses a line in wire 2

For example,

R8,U5,L5,D3 => [(0,8), (5, 0), (0.-5), (-3,0)]
[(0,8), (5, 0), (0.-5), (-3,0)] => [{(0,0):(0,8)}, {(0,8):(5,8), (5,8):5,3}, (5,3):(2,3)]

To determine where they cross, I assumed that lines didn't overlap. Meaning that they would only cross if one was vertical and the other horizontal. That assumption held up.

The X doesn't change for the vertical line and Y doesn't change for the horizontal. If the Y for the horizontal line falls between the Y's for the vertical line, and same thing for the X's, then the crossing point is the Y and X that don't change.

For part 2, needed to walk the lines and determine how far had been traveled for each collision. This was pretty straight forward as well, just had to remember that multiple crosses could happen on the same line.

### Day 4

It seemed like the brute force approach would be good enough, but I just couldn't let this one go. My algorithm is

1. Find the first possible candidate
2. Increment from there until the final target is reached
3. Check if each candidate is valid along the way

For example, my starting input number was 359282, which means the first possible candidate is 359999. Nothing between 359282 and 359999 would be ascending.

To increment from there, start with the ones digit and add one. If it rolls over to 10, the increment the next one to the left. Once one doesn't roll over, fill in the slots to the right of it with its value.

Incrementing 359999 would give 366666. Nothing between 360000 and 366666 can be ascending, so can't be a valid candidate.

For part 1, the criteria of it needing to have at least one run of 2 digits the same is easy to take into account. The only possible candidates that fail that are 012345, 123456, 234567, 345678, and 456789.

For part 2, it's a little more complicated. My input only had one invalid candidate for part 1, but close to 200 for part 2.

I'm still certain there's a mathy way to calculate most of part 1, but my solution runs in under 1 ms already so I see no reason to try to optimize it further. I'm not sure if the mathy approach would work for part 2.