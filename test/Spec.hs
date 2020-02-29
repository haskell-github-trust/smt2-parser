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

-- | Sec 3.1
lexiconTest = TestList [ pN "0" ("0" :: Numeral)
                       , pN "42" ("42" :: Numeral)
                       , pf numeral "02"   -- ^ should not start with 0
                       , pf numeral "221b" -- ^ should not contain letters
                       , pD "0.0" ("0.0" :: Decimal)
                       , pD "0.00" ("0.00" :: Decimal)
                       , pD "0.1" ("0.1" :: Decimal)
                       , pD "13.37" ("13.37" :: Decimal)
                       , pD "1.010" ("1.010" :: Decimal)
                       , pf decimal ".5"
                       , pH "#x0" ("0" :: Hexadecimal)
                       , pH "#xa04" ("a04" :: Hexadecimal)   -- ^ alphabet
                       , pH "#xA04" ("a04" :: Hexadecimal)   -- ^ alphabet, to lower case
                       , pH "#x01Ab" ("01ab" :: Hexadecimal) -- ^ mixture
                       , pH "#x61ff" ("61ff" :: Hexadecimal)
                       , pH "#xdeadbeef" ("deadbeef" :: Hexadecimal)
                       , pf hexadecimal "#x#x"    -- ^ signs
                       , pf hexadecimal "#xA1G01" -- ^ letter is not hex degit
                       , pB "#b0" ("0" :: Binary)
                       , pB "#b1" ("1" :: Binary)
                       , pB "#b001" ("001" :: Binary)
                       , pB "#b101011" ("101011" :: Binary)
                       , pf binary "#b02"
                       , pL "\"\"" ("" :: StringLiteral)
                       , pL "\"this is a string literal\"" ("this is a string literal" :: StringLiteral)
                       , pL "\"one\\n two\"" ("one\\n two" :: StringLiteral) -- ^ non-escape
                       , pL "\"She said: \\\"Hello!\\\"\"" ("She said: \"Hello!\"" :: StringLiteral)
                       , pL "\"Here is a backslash: \\\\\"" ("Here is a backslash: \\" :: StringLiteral)
                       ]
  where
    pN = pe numeral
    pD = pe decimal
    pH = pe hexadecimal
    pB = pe binary
    pL = pe stringLiteral
    pR = pe reservedWord
    pS = pe symbol
    pK = pe keyword

specTest = TestList [ lexiconTest ]

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
