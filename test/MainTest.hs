module Main (main) where

import Test.HUnit
import System.Exit
import Parser (eval)
import Environment (Variable(..))

testEmptyProgram = TestCase(assertEqual
                             "Empty program"
                             []
                             (eval "")
                           )

testSkip = TestCase(assertEqual
                     "Testing 'skip'"
                     []
                     (eval "skip")
                    )

testAssignment = TestCase(assertEqual
                           "Testing 'x:=3'"
                           [Variable {name="x", vtype="int", value=3}]
                           (eval "x:=3")
                         )

testSequence = TestCase(assertEqual
                         "Testing 'x:=3;y:=4'"
                          [ Variable {name="x", vtype="int", value=3}
                          , Variable {name="y", vtype="int", value=4}
                          ]
                          (eval "x:=3;y:=4")
                        )

testIfTrueCase = TestCase(assertEqual
                           "Testing 'if true then x:=1 else x:=0 end'"
                           [Variable {name="x", vtype="int", value=1}]
                           (eval "if true then x:=1 else x:=0 end")
                         )

testIfFalseCase = TestCase(assertEqual
                           "Testing 'if false then x:=1 else x:=0 end'"
                           [Variable {name="x", vtype="int", value=0}]
                           (eval "if false then x:=1 else x:=0 end")
                         )

testWhileNoRun = TestCase(assertEqual
                            "Testing 'x:=1;while x<=0 do x:=x+1 end"
                            [Variable {name="x", vtype="int", value=1}]
                            (eval "x:=1;while x<=0 do x:=x+1 end")
                             )

testWhileSingleRun = TestCase(assertEqual
                               "Testing 'x:=1;while x<=1 do x:=x+1 end"
                               [Variable {name="x", vtype="int", value=2}]
                               (eval "x:=1;while x<=1 do x:=x+1 end")
                             )

testWhileMultipleRun = TestCase(assertEqual
                               "Testing 'x:=1;while x<=5 do x:=x+1 end"
                               [Variable {name="x", vtype="int", value=6}]
                               (eval "x:=1;while x<=5 do x:=x+1 end")
                             )

main :: IO()
main = do counts <- runTestTT(test [ testEmptyProgram
                                   , testSkip
                                   , testAssignment
                                   , testSequence
                                   , testIfTrueCase
                                   , testIfFalseCase
                                   , testWhileNoRun
                                   , testWhileSingleRun
                                   , testWhileMultipleRun
                                   ])
          if errors counts + failures counts == 0
            then exitSuccess
            else exitFailure
