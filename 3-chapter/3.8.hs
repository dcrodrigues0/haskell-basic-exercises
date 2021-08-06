module Ex8 where


-- Create a function that remove all even numbers, multiple of seven numbers and negative numbers, return a reversed list 

identifyEvenAndSevenPlusPositiveNumber x = 
  odd x        &&
  mod x 7 /= 0 &&
  x > 0

getReversedAndFilteredList xs = reverse [ x | x <- xs, identifyEvenAndSevenPlusPositiveNumber x]

