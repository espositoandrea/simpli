module Main (main) where

import           Control.Exception
import           Environment       (Variable (..))
import           Parser            (eval)
import           System.Exit
import           Test.HUnit

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
                           [Variable {name="x", vtype="int", value=Left 3}]
                           (eval "x:=3")
                         )

testSequence = TestCase(assertEqual
                         "Testing 'x:=3;y:=4'"
                          [ Variable {name="x", vtype="int", value=Left 3}
                          , Variable {name="y", vtype="int", value=Left 4}
                          ]
                          (eval "x:=3;y:=4")
                        )

testIfTrueCase = TestCase(assertEqual
                           "Testing 'if true then x:=1 else x:=0 end'"
                           [Variable {name="x", vtype="int", value=Left 1}]
                           (eval "if true then x:=1 else x:=0 end")
                         )

testIfFalseCase = TestCase(assertEqual
                           "Testing 'if false then x:=1 else x:=0 end'"
                           [Variable {name="x", vtype="int", value=Left 0}]
                           (eval "if false then x:=1 else x:=0 end")
                         )

testWhileNoRun = TestCase(assertEqual
                            "Testing 'x:=1;while x<=0 do x:=x+1 end"
                            [Variable {name="x", vtype="int", value=Left 1}]
                            (eval "x:=1;while x<=0 do x:=x+1 end")
                             )

testWhileSingleRun = TestCase(assertEqual
                               "Testing 'x:=1;while x<=1 do x:=x+1 end"
                               [Variable {name="x", vtype="int", value=Left 2}]
                               (eval "x:=1;while x<=1 do x:=x+1 end")
                             )

testWhileMultipleRun = TestCase(assertEqual
                                 "Testing 'x:=1;while x<=5 do x:=x+1 end"
                                 [Variable {name="x", vtype="int", value=Left 6}]
                                 (eval "x:=1;while x<=5 do x:=x+1 end")
                             )

testArrayAssignment = TestCase(assertEqual
                                 "Testing 'x:=[1,2,3]'"
                                 [ Variable {name="x[0]", vtype="int[]", value=Left 1}
                                 , Variable {name="x[1]", vtype="int[]", value=Left 2}
                                 , Variable {name="x[2]", vtype="int[]", value=Left 3}
                                 ]
                                 (eval "x:=[1,2,3]")
                              )

testArrayConcatenation = TestCase(assertEqual
                                    "Testing 'x:=[1]++[2,3]'"
                                    [ Variable {name="x[0]", vtype="int[]", value=Left 1}
                                    , Variable {name="x[1]", vtype="int[]", value=Left 2}
                                    , Variable {name="x[2]", vtype="int[]", value=Left 3}
                                    ]
                                    (eval "x:=[1]++[2,3]")
                                 )
testArrayLength = TestCase(assertEqual
                             "Testing 'x:=|[2,3]|'"
                             [Variable {name="x", vtype="int", value=Left 2}]
                             (eval "x:=|[2,3]|")
                           )

testArrayUpdate = TestCase(assertEqual
                             "Testing 'x:=[0,0,0];x[1]:=1'"
                             [ Variable {name="x[0]", vtype="int[]", value=Left 0}
                             , Variable {name="x[1]", vtype="int[]", value=Left 1}
                             , Variable {name="x[2]", vtype="int[]", value=Left 0}
                             ]
                             (eval "x:=[0,0,0];x[1]:=1")
                          )
testArrayWhile = TestCase(assertEqual
                             "Testing 'x:=[2,3,5];i:=0;while i < |x| do x[i]:=x[i]*x[i];i:=i+1;end'"
                             [ Variable {name="x[0]", vtype="int[]", value=Left 4}
                             , Variable {name="x[1]", vtype="int[]", value=Left 9}
                             , Variable {name="x[2]", vtype="int[]", value=Left 25}
                             , Variable {name="i", vtype="int", value=Left 3}
                             ]
                             (eval "x:=[2,3,5];i:=0;while i < |x| do x[i]:=x[i]*x[i];i:=i+1;end")
                          )

testStringLength = TestCase(assertEqual
                              "Testing 'x:=|\"ciao\"|"
                              [ Variable {name="x", vtype="int", value=Left 4} ]
                              $ eval "x:=|\"ciao\"|"
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
                                   , testArrayAssignment
                                   , testArrayConcatenation
                                   , testArrayLength
                                   , testArrayUpdate
                                   , testArrayWhile
                                   , testStringLength
                                   ])
          if errors counts + failures counts == 0
            then exitSuccess
            else exitFailure
