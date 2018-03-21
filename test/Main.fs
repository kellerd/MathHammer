#if INTERACTIVE
#load @"LoadModules.fsx"
open Probability.View
open GameActions.Primitives.Types
open GameActions.Primitives.State
open GameActions.Primitives.View
open GameActions.GameActionsList.Types
open GameActions.GameActionsList.State
open GameActions.GameActionsList.View
open GameActions.Types
open GameActions.State
open GameActions.View
open MathHammer.Models.Types
open MathHammer.Models.State
open MathHammer.Models.View
open MathHammer.UnitList.Types
open MathHammer.UnitList.State
open MathHammer.UnitList.View
open MathHammer.Types
open MathHammer.State
open MathHammer.View
open Global
open App.Types
open App.State
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
