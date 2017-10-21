import Solutions

import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList [testEuler1, 
                             testEuler2,
                             testEuler3,
                             testEuler4,
                             testEuler5,
                             testEuler6,
                             testEuler7]--,
                             --testEuler8]

testEuler1 :: Test
testEuler1 = TestCase $ assertEqual "Should pass" 233168 (euler1 1000)

testEuler2 :: Test
testEuler2 = TestCase $ assertEqual "Should pass" 4613732 (euler2 4000000)

testEuler3 :: Test
testEuler3 = TestCase $ assertEqual "Should pass" 6857 (euler3 600851475143)

testEuler4 :: Test
testEuler4 = TestCase $ assertEqual "Should pass" 906609 (euler4 3)

testEuler5 :: Test
testEuler5 = TestCase $ assertEqual "Should pass" 232792560 (euler5 20)

testEuler6 :: Test
testEuler6 = TestCase $ assertEqual "Should pass" 25164150 (euler6 100)

testEuler7 :: Test
testEuler7 = TestCase $ assertEqual "Should pass" 104743 (euler7 10001)

-- TODO(bkaylor): Fix this one!
-- testEuler8 :: Test
-- testEuler8 = TestCase $ assertEqual "Should pass" 0 (euler8 13)
