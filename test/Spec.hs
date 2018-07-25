import Test.HUnit
import Parser

main :: IO Counts
main = runTestTT tests

tests = TestList (isCommentTests ++ stripCommentsTests)



---------------------------------------isComment--------------------------------------------------------------------
isCommentTests = [TestLabel "Basic Commnted Line Test" isCommentTest1, TestLabel "Basic Uncommnted Line Test" isCommentTest2]

isCommentExample1 = "--commented line"
isCommentResult1 = True

isCommentTest1 = TestCase (assertEqual "Failed to identify comment" isCommentResult1 (isComment isCommentExample1))


isCommentExample2 = "uncommented line"
isCommentResult2 = False

isCommentTest2 = TestCase (assertEqual "Failed to identify comment" isCommentResult2 (isComment isCommentExample2))


---------------------------------------stripComments--------------------------------------------------------------------

stripCommentsTests = [TestLabel "Single Line Test" stripCommentsTest1]

stripCommentsExample1 = ["--commented line", "uncommented line", "  --commented with two spaces", "  uncommented with two spaces"]
stripCommentsResult1 = ["uncommented line", "  uncommented with two spaces"]

stripCommentsTest1 = TestCase (assertEqual "Failed to uncomment single line" stripCommentsResult1 (stripComments stripCommentsExample1))


