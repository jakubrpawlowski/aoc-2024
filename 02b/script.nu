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

def compareConsecutiveItems [source, col:string, --increasing (-i)] {
  let el = $source
  | get $col

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
  compareConsecutiveItems $in item -i
}
| insert isSafelyDecreasing {
  compareConsecutiveItems $in item
}
| insert isSafe {
  [
    ($in.isSafelyIncreasing | all { |i| $i == true })
    ($in.isSafelyDecreasing | all { |i| $i == true })
  ] | any { |i| $i == true }
}

let afterFix = $levels
| where { |i| $i.isSafe == false }
| insert firstBadInc {
  $in.isSafelyIncreasing
  | enumerate
  | where { |i| $i.item == false }
  | first
  | get index
}
| insert badFixInc1 {
  $in.item
  | enumerate
  | where { |i| $i.index != $it.firstBadInc }
  | get item
}
| insert badFixInc2 {
  $in.item
  | enumerate
  | where { |i| $i.index != ($it.firstBadInc + 1) }
  | get item
}
| insert firstBadDec {
  $in.isSafelyDecreasing
  | enumerate
  | where { |i| $i.item == false }
  | first
  | get index
}
| insert badFixDec1 {
  $in.item
  | enumerate
  | where { |i| $i.index != $it.firstBadDec }
  | get item
}
| insert badFixDec2 {
  $in.item
  | enumerate
  | where { |i| $i.index != ($it.firstBadDec + 1) }
  | get item
}

let final = $afterFix
| insert isSafelyIncreasing1 {
  compareConsecutiveItems $in badFixInc1 -i
}
| insert isSafelyIncreasing2 {
  compareConsecutiveItems $in badFixInc2 -i
}
| insert isSafelyDecreasing1 {
  compareConsecutiveItems $in badFixDec1
}
| insert isSafelyDecreasing2 {
  compareConsecutiveItems $in badFixDec2
}
| insert isSafeFix {
  [
    ($in.isSafelyIncreasing1 | all { |i| $i == true })
    ($in.isSafelyIncreasing2 | all { |i| $i == true })
    ($in.isSafelyDecreasing1 | all { |i| $i == true })
    ($in.isSafelyDecreasing2 | all { |i| $i == true })
  ] | any { |i| $i == true }
}

let totals = $final.isSafeFix
| where { |i| $i == true }
| length

$final
| append $totals
