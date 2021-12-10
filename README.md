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

## Day 6 Notes:

Part 1: My initial thought was to just keep a vector and model every fish as
a separate cell. But I had a feeling when the problem talked about an "exponential"
lanternfish growth model that this would get out of hand fairly quickly. All we
really care about is the number of fish of each age group. The mechanics of 
"aging" the fish and increasing the population is straightforward, sort of a
shift-register with addition to certain cells.

I'm actually pretty happy with this one, fairly clean implementation, and I was
able to make use of the new Ada 202X 'Reduce attribute for the final tally.

Part 2: Changed the type of the FishPopulations to Unsigned_64 and change number
of days to run the simulation to 256, worked after making sure to use Unsigned_64
throughout.

## Day 7 Notes:

Part 1: There's probably some kind of elegant way to do a binary search or something
of the various distances, but with the limited list of inputs, just brute-forcing
the sum of distances still results in an O((max_input - min_input) * n) runtime, which
is entirely reasonable. I had a feeling that fancy stuff might result in getting stuck
in a local minima as well. Ain't nobody got time for that.

Part 2: Individual fuel burn calculations become `d + (d - 1) + (d - 2) + ... + 1` for
a given distance d. Running a `for` loop to calculate this isn't the end of the world,
but I vaguely remembered a shortcut, which as the source code mentions, I thought I
read about in an anecdote about an interview question. In this case, the formula
`d(d-1)/2` gives you the sum of all integers up to `(d - 1)`, so using 
`d + d(d-1)/2` will give the fuel burn as defined in the problem.
`d(d-1)/2` is one of those handy things to keep in the back of your mind (for
interviews, at least).

Adding a running tally of the best fuel burn was a straightforward addition to
the Part 1 solution.

## Day 8 Notes:

Part 1: More ugly parsing code, but fairly straightforward

Part 2: Oof, this one was a doozy. There's probably a lot of ways to make this
more elegant.

## Day 9 Notes:

Part 1: There's probably a more elegant way of finding the low points, but the
massive wall of if-then-elsif I used to handle all the corner cases and edge cases
(pun intended) works fine.

Part 2: I'm actually pretty happy with the recursive solution I came up with. The
key to making it work was a separate `dirty` grid keeping track of whether an
element was already counted as part of a different basin. At first I thought
maybe I needed to also keep track of whether the neighboring squares were larger
than the one under consideration, but the problem statement didn't mention it,
which simplified things greatly.

## Day 10 Notes:

Part 1: Bracket matching is kind of a classic interview problem, the use of a
stack here is key.

Part 2: I love, love, love Ada's ability to index arrays using pretty much any
type. Having an easy mapping from `char on the stack` => `points` made this
straightforward. Ada's detection of overflow made it obvious that a larger
numeric type to hold the score total was essential. Big wins for Ada on this
part.
