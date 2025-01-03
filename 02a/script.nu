let input = open input
| lines
| split column " "
| each {
  values
  | into int
}
| enumerate

def isSafe [num1, num2] {
  $num1 - $num2
  | math abs
  | $in in [1, 2, 3]
}

def isSafelyIncreasing [num1, num2] {
  $num2 > $num1 and (isSafe $num1 $num2)
}

def isSafelyDecreasing [num1, num2] {
  $num2 < $num1 and (isSafe $num1 $num2)
}

def compareConsecutiveItems [list, --increasing (-i)] {
  let el = $list.item

  $el
  | drop 1
  | enumerate
  | each {
    |i| if $increasing {
      isSafelyIncreasing $i.item ($el | get ($i.index + 1))
    } else {
      isSafelyDecreasing $i.item ($el | get ($i.index + 1))
    }
  }
}

let levels = $input
| insert isSafelyIncreasing {
  compareConsecutiveItems $in -i
}
| insert isSafelyDecreasing {
  compareConsecutiveItems $in
}
| insert allSafeInc {
  $in.isSafelyIncreasing
  | all {
    |i| $i == true
  }
}
| insert allSafeDec {
  $in.isSafelyDecreasing
  | all {
    |i| $i == true
  }
}
| insert isSafe {
  [$in.allSafeInc $in.allSafeDec] | any { |i| $i == true }
}

let totals = $levels.isSafe
| where { |i| $i == true }
| length

$levels
| append $totals
