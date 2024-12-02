let input = open input
| lines
| split column "   "

def sortedColumn [table, column] {
$table
| select $column
| sort-by $column
}

let diff = sortedColumn $input column1
| merge (sortedColumn $input column2)
| insert difference {
  ($in.column1 | into int) - ($in.column2 | into int)
  | math abs
  }

let totals = $diff
| math sum
| insert index total

echo $diff | append $totals
