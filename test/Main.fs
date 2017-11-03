#if INTERACTIVE
#load "LoadModules.fsx"
#load "FsCheck.fs"
#load "DistTest.fs"
#load "Attacker.fs"
#load "Normalization.fs"
#load "OperationTest.fs"
#load "ReduceTest.fs"
#else
module ExpectoTests
#endif
open Expecto

#if INTERACTIVE
Tests.runTests defaultConfig <|
    testList "All Tests" 
        [ DistTests.tests
          OperationTests.tests
          ReduceTests.tests
          AttackerTests.tests
          Normalization.tests ]    
#else
[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv
#endif
