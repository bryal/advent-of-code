import std/setutils
import std/sequtils

proc priority(item_type: char): uint =
  return case item_type
  of 'a'..'z':
    cast[uint](item_type) - cast[uint]('a') + 1
  else:
    cast[uint](item_type) - cast[uint]('A') + 27

echo "Part 1"
block part1:
  var prio_sum: uint = 0
  for line in lines("../inputs/day3.txt"):
    let comp1: set[char] = to_set(line[0 .. len(line) /% 2 - 1])
    let comp2: set[char] = to_set(line[len(line) /% 2 .. len(line)-1])
    var overlapping: char
    for c in comp1 * comp2:
      overlapping = c
    prio_sum += priority(overlapping)
  echo prio_sum


echo "Part 2"
block part2:
  var prio_sum: uint = 0
  let lines1 = to_seq(lines("../inputs/day3.txt"))
  for group in lines1.distribute(len(lines1) /% 3):
    var overlapping: char
    for c in group.mapIt(to_set(it)).foldl(a * b):
      overlapping = c
    prio_sum += priority(overlapping)
  echo prio_sum
