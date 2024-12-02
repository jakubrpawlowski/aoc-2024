let input = open input
| lines
| split column "   "

let appearances = ($input | select column1)
| insert count {
  let a = $in.column1

  $input
  | get column2
  | where $it == $a
  | length
}
| insert score {
  ($in.column1 | into int) * ($in.count | into int)
}

let totals = $appearances
| math sum
| insert index total

echo $appearances | append $totals
