#if INTERACTIVE
#load "LoadModules.fsx"
#load "FsCheck.fs"
#load "FsCheckGen.fs"
#load "Attacker.fs"
#load "OperationTest.fs"
#load "DistTest.fs"
#load "ReduceTest.fs"
#load "Normalization.fs"
#load "EvalTest.fs"
#load "RepeatTest.fs"
#load "TypeChecker.fs"
#else
module ExpectoTests
#endif
open Expecto
#if INTERACTIVE
Tests.runTests defaultConfig AttackerTests.tests
Tests.runTests defaultConfig OperationTests.tests
Tests.runTests defaultConfig DistTests.tests
Tests.runTests defaultConfig ReduceTests.tests
Tests.runTests defaultConfig NormalizationTests.tests
Tests.runTests defaultConfig EvalTests.tests
Tests.runTests defaultConfig RepeatTests.tests
Tests.runTests defaultConfig TypeChecker.tests
#else
[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv
#endif
