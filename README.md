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

## Day 11 Notes:

Part 1: Just good old-fashioned brute-forcing. I think the trick here is to keep
a separate matrix of who flashed so they can be reset, and then the loop condition
of "did a flash occur this round"

Part 2: Since we keep the tally of flashes after each round, we know everybody
flashed if the number of flashes after the step is exactly 100 more than it was
before the step.

## Day 12 Notes:

Part 1: I decided to use BFS, algorithm mostly translated from C++ from the
geeksforgeeks site to count the number of unique paths. Modifying the "isVisited"
function to determine whether the node in question was an "uppercase" or "lowercase"
and always pretend that uppercase nodes are unvisited did the trick.

Part 2: Added a second function isVisited2 which uses some additional logic to
determine whether a lowercase node has already been visited twice. This takes
a surprisingly long time to run for the larger input graphs!

I suspect just generating a permutation of the various nodes with insertion of
uppercase nodes along the way and then doing validation of each "proposed" path
per the problem's rules might have worked too.

## Day 13 Notes:

This was a fun one.

Part 1: Use of Ada's Hashed_Sets package was critical here, as it made it very
quick to eliminate overlapping dots. The most troublesome piece here was goofing
up 0-based coordinates and finally realizing that the dots along the fold are
discarded.

Part 2: Initially I thought that I'd need to use the width/height of the folded
paper for part 1 to calculate the coordinates. That wasn't necessary, but thankfully
I kept them around because it made generating the part 2 answer very easy.

## Day 14 Notes:

Part 1: Took the naive approach initially, and just used an Unbounded_String to
update as we went. It worked great... and was totally not going to fly in Part 2!

Part 2: This took me a lot more work than I expected. I think I started with the
right idea, which was to keep track of _pairs_ of letters, and then update their
count, since each substitution creates essentially 2 new pairs. i.e. NN -> C would
make NC and CN. Each of those two pairs can be represented as a number, i.e.
AA = 00, AB = 01, BA = 26 ... ZZ = 675. This is easy for us to store in an array,
though a hash map could have worked here too. I tried to go back and calculate
the number of each individual letter from these pairs which was a fool's errand,
didn't work at all. The trick here was just updating the character counts as
each insertion was performed... doh! No letters are destroyed during insertion,
only pairs, so it ended up being a simple add.

I'm pretty happy with the end result, it runs quickly and is pretty clean
considering all the challenges I had getting it to work.

## Day 15 Notes:

Part 1: First thought was to use dynamic programming, but there's nothing that
says we have to only move down and right, so this makes the problem more of a
shortest-path graph algorithm. I translated a generic Dijkstra's algo and it
worked fine. Instead of using a priority queue though, I just re-scanned the whole
grid looking for unvisited vertices to try, which worked fine for the small input.

Part 2: Duplicating the grids is straight-forward, adding some logic to the
input loop to take whatever value is in the original grid and then duplicate it
in each of the other tiles with the appropriate increment factor (tilex + tiley).

The larger input set definitely exposed a weakness in my original algo though,
in that this thing was _sloooooow_. It took about a minute on my machine, which
is fairly beefy. I suspect the issue is my brute-force search for un-visited
nodes. Replacing this with a priority queue would probably help a lot... so I
went ahead and did that, and now it runs in about 100 ms. Much better! I played
around with converting it to an A* algorithm (which I learned about in my
Dijkstra's algo research) but runtime was essentially the same.

## Day 16 Notes:

Part 1: Took me a while to comprehend the problem statement, but once I did, I
turned to my trusty tool the Recursive Descent parser. As I was working it out,
I had a feeling that this was going to turn into some kind of expression
calculator...

Part 2: ...which it did! I had started to build up a fancy AST tree dealie, and
work through everything that way, but... I already had the machinery in place
with the recursive descent parser to just go ahead and calculate the expression
as it encountered the various sub-expressions. Once I got it to compile, it
actually worked the first try, which was a shock! There's probably a cleaner
more "functional" way to handle it, i.e. passing a function pointer or something
for each type of operation, but I ended up with 2 massive case statements in
the parseOperator function instead. Not the most "elegant", but it worked.

## Day 17 Notes:

Part 1: I decided to just brute force this, loop a bunch of different initial
x and y velocities, run the simulation for N steps and see what hit, if the peak
trajectory is better than the previous peak, then save it, etc. I struggled
with incorrect answers for a while, when I realized that I wasn't running the
simulation long enough! Giving a much bigger N gave me a good result for part 1,
since very high y-velocities can still hit the target but take a long time to
fall back down out of the stratosphere (or whatever the underwater equivalent
is). The input was small enough that I didn't bother with parsing.

Part 2: I expanded the scope of y velocities, since you can kind of shoot the
probe "downward" and still hit the target if your X is big enough. Widening the
aperture a bit gave me the correct answer to this problem just a couple
minutes after figuring out part 1. I changed the simulation to end once we
overshot the target rather than continuing to run for a set number of steps.
It's much quicker this way.

## Day 18 Notes:

Part 1: What a doozy. Lots of fighting with Ada's Multiway_Trees package, but
it provided everything I needed once I dug into it. Tricky parts were finding
the left and right (if they exist) elements of a deeply-nested pair, I just
ended up flattening the tree with a DFS and using some vector index magic to
figure it out. Took me a long time to get it all figured out. One hack that's
really dirty but I'm also kind of proud - I couldn't get the Multiway_Trees
package to splice/copy/whatever two rooted trees, so I just dumped 'em to
strings, concatenated and added the extra `[`, `,` and `]` and re-read it in.

I had the temptation of doing everything with mucking about with the textual
representation, but I'm glad I didn't, since a lot of the helper functions were
most easily written with a recursive solution.

Part 2: Fairly straightforward, just did an O(n^2) pairwise comparison of
everything. I'm not sure that there's a quicker way to do this, honestly.
The whole thing still finishes in less than a second.
