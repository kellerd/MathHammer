#if INTERACTIVE
#load "LoadModules.fsx"
#load @"..\paket-files\haf\expecto\Expecto.FsCheck\FsCheck.fs"
#load "DistTest.fs"
#load "Attacker.fs"
#load "Normalization.fs"
#load "OperationTest.fs"
#load "ReduceTest.fs"
#r @"c:\Users\diese\source\repos\mathhammer\packages\System.Threading\lib\netstandard1.3\System.Threading.dll"
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
