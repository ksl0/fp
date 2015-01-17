--my initial code
import Data.Char (digitToInt)

intList::[Int]
intList = map digitToInt (show (2^1000))
ans = foldr (+) 0 intList
