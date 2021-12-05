# Doc's 2021 Advent of Code attempts

Compile any of these with `gnatmake dayX.adb`, run with `dayX`. I tested these on Ubuntu WSL2 with GNAT 2020 installed.

I've been fairly busy with family this holiday season, so
these are quick-n'-dirty and tend to be brute-force. 

I also write a lot of nasty hand-crafted parsers to 
handle the data in the inputs (and _nothing_ else).

## Day 1 Notes:

Part 1: Pretty straight forward.

Part 2: A little nastier, I use the modulo to keep track
of where I'm at in a given window.

## Day 2 Notes:

Part 2: I obliterated the stand-alone solution for Part 1 in the same
file to generate the answers I needed, but this one was fairly
straight-forward.

## Day 3 Notes:

Part 1: Separate array to keep track of bit counts for each bit 
position seemed to work well.

Part 2: Oof. What helped me here was keeping a separate "valid" flag
with each line of input, so as we filtered out each value I could just
mark that flag rather than try and update some kind of a data 
structure. Unfortunately I had to keep track of the number of remaining
valid entries as it goes rather than just reference a 'Length field or
something. But it works.

## Day 4 Notes:

Part 1: Brute-force. Just go through each checked number, mark that 
number off all the boards with it. At the end, do a brute-force check
for any horizontal, vertical and diagonal bingos.

Part 2: Instead of stopping at the first winner, continue checking, 
print each winner as you find them. Last one printed out is your 
answer.

## Day 5 Notes:

Part 1: It helped using the sample input here to test with. Most of
the difficulty here was debugging a cut-and-paste error I made. Instead
of using a hash table or something to store "drawn" coordinate pairs, 
I put all the input coords into a big linear array like you might have
with a framebuffer or something, since that's
kind of what we're doing here.

Part 2: Fairly straight-forward to generalize the "walkXYZ" functions
into one that worked diagonally. Basic line-drawing algorithm but your
slope will always be 1 or -1.
