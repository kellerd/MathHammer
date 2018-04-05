#if INTERACTIVE
#load @"LoadModules.fsx"
open GameActions.Primitives.Types
open GameActions.Primitives.State
open GameActions.GameActionsList.Types
open GameActions.GameActionsList.State
open GameActions.Types
open GameActions.State
open MathHammer.Models.Types
open MathHammer.Models.State
open MathHammer.UnitList.Types
open MathHammer.UnitList.State
open MathHammer.Types
open MathHammer.State
//open Probability.View
// open GameActions.Primitives.View
// open GameActions.GameActionsList.View
// open GameActions.View
// open MathHammer.Models.View
// open MathHammer.UnitList.View
// open MathHammer.View
open Global
open App.Types
open App.State
#load "FsCheck.fs"
#load "FsCheckGen.fs"
open Expecto
#load "TypeChecker.fs"
Tests.runTests defaultConfig TypeChecker.tests
#load "DistTest.fs"
Tests.runTests defaultConfig DistTests.tests
#load "Normalization.fs"
Tests.runTests defaultConfig NormalizationTests.tests
#load "OperationTest.fs"
Tests.runTests defaultConfig OperationTests.tests
#load "ReduceTest.fs"
Tests.runTests defaultConfig ReduceTests.tests
#load "RepeatTest.fs"
Tests.runTests defaultConfig RepeatTests.tests
#load "EvalTest.fs"
Tests.runTests defaultConfig EvalTests.tests
#load "Attacker.fs"
Tests.runTests defaultConfig AttackerTests.tests
#else
module ExpectoTests
open Expecto
[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv
#endif

// (Check (Check.Fail (ParamArray [Value (Int 0)])) + Check (Check.Pass (ParamArray [])))

// (Check
//      (Check.Fail
//         (ParamArray
//            [Value (Int 1); Value (Int 0); Value (Int 0); Value (Int 0);
//             Value (Int 8); Value (Int 1); Value (Int 1)])) +
//    Dist {Probabilities = [(ParamArray [], 4.940656458e-324)];})