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
filterTests spec tests = (selected spec tests, filteredOut (selected spec tests) tests)
  where 
    include :: [FilterCriterion] -> [TestCaseDefinition] -> [TestCaseDefinition]
    include _ [] = []
    include [] t = t
    include incs (x:xs) = if matchesAny False incs x then x : include incs xs else include incs xs

    exclude :: [FilterCriterion] -> [TestCaseDefinition] -> [TestCaseDefinition]
    exclude _ [] = []
    exclude [] _ = []
    exclude excs (x:xs) = if matchesAny False excs x then x : exclude excs xs else exclude excs xs

    selected :: FilterSpec -> [TestCaseDefinition] -> [TestCaseDefinition]
    selected (FilterSpec incs excs _) t = exclude excs (include incs t)

    filteredOut :: [TestCaseDefinition] -> [TestCaseDefinition] -> [TestCaseDefinition]
    filteredOut [] t = t
    filteredOut (sel:sels) ts = if sel `notElem` ts then sel : filteredOut sels ts else filteredOut sels ts

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
    a (ByAny anyThing) = tcdName test == anyThing || elem anyThing (tcdTags test) || tcdCategory test == anyThing
    a (ByCategory cat) = tcdCategory test == cat
    a (ByTag tag) = tag `elem` tcdTags test


-- | Trim leading and trailing whitespace from a filter identifier.
trimFilterId :: String -> String
trimFilterId = reverse . dropWhile isSpace . reverse . dropWhile isSpace
