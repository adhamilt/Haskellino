import Test.HUnit
import Parser

main :: IO Counts
main = runTestTT tests

tests = TestList (stripCommentsTests)



---------------------------------------stripComments--------------------------------------------------------------------

stripCommentsTests = [TestLabel "Single Line Test" stripCommentsTest1, TestLabel "Block Comment Without Text" stripCommentsTest2, TestLabel "Block Comment With Text" stripCommentsTest3]

stripCommentsExample1 = ["--commented line", "uncommented line", "  --commented with two spaces", "  uncommented with two spaces"]
stripCommentsResult1 = ["uncommented line", "    uncommented with two spaces"]

stripCommentsTest1 = TestCase (assertEqual "Failed to uncomment single line" stripCommentsResult1 (stripComments stripCommentsExample1))


stripCommentsExample2 = ["Uncommented Line", "{-", "multi-line commented", "  multi-line commented with two spaces", "  multi-line commented with two spaces", "-}", "Uncommented Line"]
stripCommentsResult2 = ["Uncommented Line", "", "", "Uncommented Line"]

stripCommentsTest2 = TestCase (assertEqual "Failed to uncomment multiline comments" stripCommentsResult2 (stripComments stripCommentsExample2))


stripCommentsExample3 = ["Not inside comment{-","Inside of comment","Second line inside of comment","Inside end of comment-}Outside of comment","Not inside Comment"]
stripCommentsResult3 = ["Not inside comment","Outside of comment","Not inside Comment"]

stripCommentsTest3 = TestCase (assertEqual "Failed to uncomment multiline comments" stripCommentsResult3 (stripComments stripCommentsExample3))
