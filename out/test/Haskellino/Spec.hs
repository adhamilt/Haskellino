import Test.HUnit
import Parser

main :: IO Counts
main = runTestTT tests

tests = TestList []
