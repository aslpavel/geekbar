module Tests (tests) where

import Distribution.TestSuite.QuickCheck (Test)

-- | Test submodules
import qualified Tests.Css (tests)

-- | All tests
tests :: IO [Test]
tests = mconcat `fmap` sequence [Tests.Css.tests]
