-- | Building the final test report and computing statistics.
--
-- This module assembles a 'TestReport' from the results of test execution,
-- computes aggregate statistics, and builds the per-category success-rate
-- histogram.
module SOLTest.Report
  ( buildReport,
    groupByCategory,
    computeStats,
    computeHistogram,
    rateToBin,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import SOLTest.Types

-- ---------------------------------------------------------------------------
-- Top-level report assembly
-- ---------------------------------------------------------------------------

-- | Assemble the complete 'TestReport'.
--
-- Parameters:
--
-- * @discovered@ – all 'TestCaseDefinition' values that were successfully parsed.
-- * @unexecuted@ – tests that were not executed for any reason (filtered, malformed, etc.).
-- * @executionResults@ – 'Nothing' in dry-run mode; otherwise the map of test
--   results keyed by test name.
-- * @selected@ – the tests that were selected for execution (used for stats).
-- * @foundCount@ – total number of @.test@ files discovered on disk.
buildReport ::
  [TestCaseDefinition] ->
  Map String UnexecutedReason ->
  Maybe (Map String TestCaseReport) ->
  [TestCaseDefinition] ->
  Int ->
  TestReport
buildReport discovered unexecuted mResults selected foundCount =
  let mCategoryResults = fmap (groupByCategory selected) mResults
      stats = computeStats foundCount (length discovered) (length selected) mCategoryResults
   in TestReport
        { trDiscoveredTestCases = discovered,
          trUnexecuted = unexecuted,
          trResults = mCategoryResults,
          trStats = stats
        }

-- ---------------------------------------------------------------------------
-- Grouping and category reports
-- ---------------------------------------------------------------------------

-- | Group a flat map of test results into a map of 'CategoryReport' values,
-- one per category.
--
-- The @definitions@ list is used to look up each test's category and points.
--
-- Working with maps was a pain.
-- At the beginning, I used insert into an empty map as a default way to create a map.
-- Then I found out about fromList and fromListWith, and life was prettier.
groupByCategory ::
  [TestCaseDefinition] ->
  Map String TestCaseReport ->
  Map String CategoryReport
groupByCategory definitions results = Map.fromListWith combine (map toEntry definitions)
  where
    getPassedPoints :: TestCaseDefinition -> Map String TestCaseReport -> Int
    getPassedPoints d res = case Map.lookup (tcdName d) res of
      Just report | tcrResult report == Passed -> tcdPoints d
      _ -> 0

    getTestResult :: TestCaseDefinition -> Map String TestCaseReport -> Maybe TestCaseReport
    getTestResult d res = Map.lookup (tcdName d) res

    toEntry def =
      ( tcdCategory def,
        CategoryReport
          { crTotalPoints = tcdPoints def,
            crPassedPoints = getPassedPoints def results,
            crTestResults = case getTestResult def results of
              Just r -> Map.fromList [(tcdName def, r)]
              Nothing -> Map.empty
          }
      )

    -- combines those that have the same category
    combine r1 r2 =
      CategoryReport
        { crTotalPoints = crTotalPoints r1 + crTotalPoints r2,
          crPassedPoints = crPassedPoints r1 + crPassedPoints r2,
          crTestResults = Map.union (crTestResults r1) (crTestResults r2)
        }

-- ---------------------------------------------------------------------------
-- Statistics
-- ---------------------------------------------------------------------------

-- | Compute the 'TestStats' from available information.
--
-- It was pretty simple until I got to the tsPassedTests. I didn't know how to calculate that.
-- A good way to practice foldl, I guess.
-- The formatting is pretty weird, but Ormolu did that, so blame it.
computeStats ::
  -- | Total @.test@ files found on disk.
  Int ->
  -- | Number of successfully parsed tests.
  Int ->
  -- | Number of tests selected after filtering.
  Int ->
  -- | Category reports (Nothing in dry-run mode).
  Maybe (Map String CategoryReport) ->
  TestStats
computeStats foundCount loadedCount selectedCount mCategoryResults =
  TestStats
    { tsFoundTestFiles = foundCount,
      tsLoadedTests = loadedCount,
      tsSelectedTests = selectedCount,
      tsPassedTests = getPassedTests mCategoryResults, -- CategoryReport -> TestCaseReport -> TestResult if Passed then + 1
      tsHistogram = computeHistogram (fromMaybe Map.empty mCategoryResults)
    }
  where
    getPassedTests :: Maybe (Map String CategoryReport) -> Int
    getPassedTests c = case c of
      Nothing -> 0
      Just a ->
        Map.foldlWithKey'
          ( \acc _ report ->
              acc
                + Map.foldlWithKey'
                  (\acc2 _ res -> acc2 + if tcrResult res == Passed then 1 else 0)
                  0
                  (crTestResults report)
          )
          0
          a

-- ---------------------------------------------------------------------------
-- Histogram
-- ---------------------------------------------------------------------------

-- | Compute the success-rate histogram from the category reports.
--
-- For each category, the relative pass rate is:
--
-- @rate = passed_test_count \/ total_test_count@
--
-- The rate is mapped to a bin key (@\"0.0\"@ through @\"0.9\"@) and the count
-- of categories in each bin is accumulated. All ten bins are always present in
-- the result, even if their count is 0.
--
-- Creating the map "statically" sounded weird but idk what else to do.
-- Love the insertWith function.
computeHistogram :: Map String CategoryReport -> Map String Int
computeHistogram categories = Map.foldlWithKey' updateHistogram mapWithBins categories
  where
    mapWithBins :: Map String Int
    mapWithBins = Map.fromList [("0.0", 0), ("0.1", 0), ("0.2", 0), ("0.3", 0), ("0.4", 0), ("0.5", 0), ("0.6", 0), ("0.7", 0), ("0.8", 0), ("0.9", 0)]

    updateHistogram :: Map String Int -> String -> CategoryReport -> Map String Int
    updateHistogram acc _ report =
      let total = fromIntegral (crTotalPoints report)
          passed = fromIntegral (crPassedPoints report)

          rate = if total == 0 then 0 else passed / total
          bin = rateToBin rate
       in Map.insertWith (+) bin 1 acc

-- | Map a pass rate in @[0, 1]@ to a histogram bin key.
--
-- Bins are defined as @[0.0, 0.1)@, @[0.1, 0.2)@, ..., @[0.9, 1.0]@.
-- A rate of exactly @1.0@ maps to the @\"0.9\"@ bin.
rateToBin :: Double -> String
rateToBin rate =
  let binIndex = min 9 (floor (rate * 10) :: Int)
      -- Format as "0.N" for bin index N
      whole = binIndex `div` 10
      frac = binIndex `mod` 10
   in show whole ++ "." ++ show frac
