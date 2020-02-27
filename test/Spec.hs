import           Language.SMT2.Parser
import           Test.HUnit

-- * Parsing tests
-- Test cases are from
--   1. the original [spec](http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.6-r2017-07-18.pdf).
--   2. HORN clause samples from [SV-COMP](https://github.com/sosy-lab/sv-benchmarks) & [benchmarks](https://github.com/hopv/benchmarks/).
--   3. Examples of horn-like clauses with disjuction on heads.

-- | parse and test a string, expecting @a@
pe :: (Eq a, Show a) => GenStrParser () a -> String -> a -> Test
pe p s expected = TestCase $ case parseStringEof p s of
                    Right actual -> expected @=? actual
                    Left _ -> assertFailure $ "should succeed for " <> s

-- | parse and test a string, expecting a parser error (@Left _@)
pf :: GenStrParser () a -> String -> Test
pf p s = TestCase $ case parseStringEof p s of
           Left _  -> pure ()
           Right _ -> assertFailure $ "should fail for " <> s

tokenTest = TestList [ pe numeral "0" ("0" :: Numeral) ]

specTest = TestList [ tokenTest ]

hornTest = TestCase $ pure ()

disjuctionTest = TestCase $ pure ()

tests = TestList [ TestLabel "spec" specTest
                 , TestLabel "horn" hornTest
                 , TestLabel "disjuction" disjuctionTest
                 ]

main :: IO ()
main = do
  counts <- runTestTT tests
  print counts
