-- | Filtering test cases by include and exclude criteria.
--
-- The filtering algorithm is a two-phase set operation:
--
-- 1. __Include__: if no include criteria are given, all tests are included;
--    otherwise only tests matching at least one include criterion are kept.
--
-- 2. __Exclude__: tests matching any exclude criterion are removed from the
--    included set.
module SOLTest.Filter
  ( filterTests,
    matchesCriterion,
    matchesAny,
    trimFilterId,
  )
where

import Data.Char (isSpace)
import SOLTest.Types
import Data.List (partition)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Apply a 'FilterSpec' to a list of test definitions.
--
-- Returns a pair @(selected, filteredOut)@ where:
--
-- * @selected@ are the tests that passed both include and exclude checks.
-- * @filteredOut@ are the tests that were removed by filtering.
--
-- The union of @selected@ and @filteredOut@ always equals the input list.
--
-- FLP: Implement this function using @matchesAny@ and @matchesCriterion@.
filterTests ::
  FilterSpec ->
  [TestCaseDefinition] ->
  ([TestCaseDefinition], [TestCaseDefinition])
filterTests spec tests = partition selected tests
  where
    include :: TestCaseDefinition -> Bool
    include t = 
      null (fsIncludes spec)
      || matchesAny (fsUseRegex spec) (fsIncludes spec) t

    exclude :: TestCaseDefinition -> Bool
    exclude = matchesAny (fsUseRegex spec) (fsExcludes spec)

    selected :: TestCaseDefinition -> Bool
    selected t = include t && not (exclude t)
  

-- | Check whether a test matches at least one criterion in the list.
matchesAny :: Bool -> [FilterCriterion] -> TestCaseDefinition -> Bool
matchesAny useRegex criteria test =
  any (matchesCriterion useRegex test) criteria

-- | Check whether a test matches a single 'FilterCriterion'.
--
-- When @useRegex@ is 'False', matching is case-sensitive string equality.
-- When @useRegex@ is 'True', the criterion value is treated as a POSIX
-- regular expression matched against the relevant field(s).
--
-- FLP: Implement this function. If you're not implementing the regex matching
-- bonus extension, you can either remove the first argument and update the usages,
-- or you can simply ignore the value.
matchesCriterion :: Bool -> TestCaseDefinition -> FilterCriterion -> Bool
matchesCriterion useRegex test criterion = a criterion
  where 
    a (ByAny anyThing) = tcdName test == trimFilterId anyThing || elem anyThing (tcdTags test) || tcdCategory test == trimFilterId anyThing
    a (ByCategory cat) = tcdCategory test == trimFilterId cat
    a (ByTag tag) = trimFilterId tag `elem` tcdTags test


-- | Trim leading and trailing whitespace from a filter identifier.
trimFilterId :: String -> String
trimFilterId = reverse . dropWhile isSpace . reverse . dropWhile isSpace
