{-# LANGUAGE OverloadedStrings #-}
import           Data.List.NonEmpty   (fromList)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Language.SMT2.Parser
import           Language.SMT2.Syntax
import           Test.HUnit
import           Text.Parsec.Text     (GenParser)

-- * Parsing tests
-- Test cases are from
--   1. the original [spec](http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.6-r2017-07-18.pdf).
--   2. HORN clause samples from [SV-COMP](https://github.com/sosy-lab/sv-benchmarks) & [benchmarks](https://github.com/hopv/benchmarks/).
--   3. Examples of horn-like clauses with disjuction on heads.

-- | parse and test a string, expecting @a@
pe :: (Eq a, Show a) => GenParser () a -> T.Text -> a -> Test
pe p s expected = TestCase $ case parseStringEof p s of
                    Right actual -> expected @=? actual
                    Left _ -> assertFailure $ "should succeed for " <> T.unpack s

parseTest :: (T.Text -> Assertion) -> (T.Text -> Assertion) -> GenParser () a -> T.Text -> Assertion
parseTest success failure p s = case parseStringEof p s of
                                  Left _  -> failure s
                                  Right _ -> success s

-- | parse and success, an assertion
pas :: GenParser () a -> T.Text -> Assertion
pas = parseTest (\_ -> pure ()) (\s -> assertFailure $ "should succeed for " <> T.unpack s)

-- | parse and failure, an assertion
paf :: GenParser () a -> T.Text -> Assertion
paf = parseTest (\s -> assertFailure $ "should fail for " <> T.unpack s) (\_ -> pure ())

-- | parse and success
ps :: GenParser () a -> T.Text -> Test
ps p s = TestCase $ pas p s

-- | parse and failure
pf :: GenParser () a -> T.Text -> Test
pf p s = TestCase $ paf p s

-- | parse file success
pfs :: GenParser () a -> FilePath -> Test
pfs p f = TestCase $ do
  s <- TIO.readFile f
  pas (stripSpaces p) (removeComment s)


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
                       , pL "\"this is a string literal\"" ("this is a string literal" :: StringLiteral)
                       , pL "\"\"" ("" :: StringLiteral)
                       , pL "\"She said: \"\"Bye bye\"\" and left.\"" ("She said: \"Bye bye\" and left." :: StringLiteral)
                       , pL "\"this is a string literal\nwith a line break in it\"" ("this is a string literal\nwith a line break in it" :: StringLiteral)
                       , pR "par" ("par" :: ReservedWord)
                       , pR "NUMERAL" ("NUMERAL" :: ReservedWord)
                       , pR "_" ("_" :: ReservedWord)
                       , pR "!" ("!" :: ReservedWord)
                       , pR "as" ("as" :: ReservedWord)
                       , pR "set-logic" ("set-logic" :: ReservedWord)
                       , pf reservedWord "asleep" -- ^ prefix
                       , pf symbol "par" -- ^ symbol should not be a reserved word
                       , pf symbol "NUMERAL"
                       , pf symbol "_"
                       , pf symbol "!"
                       , pf symbol "as"
                       , pS "asleep" ("asleep" :: Symbol) -- ^ prefixed by a reserved word
                       , pS "+" ("+" :: Symbol)
                       , pS "<=" ("<=" :: Symbol)
                       , pS "x" ("x" :: Symbol)
                       , pS "**" ("**" :: Symbol)
                       , pS "$" ("$" :: Symbol)
                       , pS "<sas" ("<sas" :: Symbol)
                       , pS "<adf>" ("<adf>" :: Symbol)
                       , pS "abc77" ("abc77" :: Symbol)
                       , pS "*$s&6" ("*$s&6" :: Symbol)
                       , pS ".kkk" (".kkk" :: Symbol)
                       , pS ".8" (".8" :: Symbol)
                       , pS "+34" ("+34" :: Symbol)
                       , pS "-32" ("-32" :: Symbol)
                       , pS "|this is a single quoted symbol|" ("this is a single quoted symbol" :: Symbol)
                       , pS "|so is\nthis one|" ("so is\nthis one" :: Symbol)
                       , pS "||" ("" :: Symbol)
                       , pS "|\" can occur too|" ("\" can occur too" :: Symbol)
                       , pS "|af kljˆ∗(0asfsfe2(&∗)&(#ˆ$>>>?”’]]984|" ("af kljˆ∗(0asfsfe2(&∗)&(#ˆ$>>>?”’]]984" :: Symbol)
                       , pS "|abc|" ("abc" :: Symbol) -- ^ quoted simple symbol is the same
                       , pS "abc" ("abc" :: Symbol)
                       , pK ":date" ("date" :: Keyword)
                       , pK ":a2" ("a2" :: Keyword)
                       , pK ":foo-bar" ("foo-bar" :: Keyword)
                       , pK ":<=" ("<=" :: Keyword)
                       , pK ":56" ("56" :: Keyword)
                       , pK ":->" ("->" :: Keyword)
                       , pK ":~!@$%^&*_-+=<>.?/" ("~!@$%^&*_-+=<>.?/" :: Keyword)
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

syntaxTest = TestList [ pI "plus" (IdSymbol "plus")
                      , pI "+" (IdSymbol "+")
                      , pI "<=" (IdSymbol "<=")
                      , pI "Real" (IdSymbol "Real")
                      , pI "|John Brown|" (IdSymbol "John Brown")
                      , pI "(_ vector-add 4 5)" (IdIndexed ("vector-add" :: Symbol) (fromList [IxNumeral "4", IxNumeral "5"]))
                      , pI "(_ BitVec 32)" (IdIndexed ("BitVec" :: Symbol) (fromList [IxNumeral "32"]))
                      , pI "(_ move up)" (IdIndexed ("move" :: Symbol) (fromList [IxSymbol "up"]))
                      , pI "(_ move down)" (IdIndexed ("move" :: Symbol) (fromList [IxSymbol "down"]))
                      , pI "(_ move left)" (IdIndexed ("move" :: Symbol) (fromList [IxSymbol "left"]))
                      , pI "(_ move right)" (IdIndexed ("move" :: Symbol) (fromList [IxSymbol "right"]))
                      , pA ":left-assoc" (AttrKey ("left-assoc" :: Keyword))
                      , pA ":status unsat" (AttrKeyValue ("status" :: Keyword) (AttrValSymbol ("unsat" :: Symbol)))
                      , pA ":my_attribute (humpty dumpty)" (AttrKeyValue
                                                             ("my_attribute" :: Keyword)
                                                             (AttrValSList [ SESymbol "humpty"
                                                                           , SESymbol "dumpty"]))
                      , pA ":authors \"Jack and Jill\"" (AttrKeyValue
                                                          ("authors" :: Keyword)
                                                          (AttrValSpecConstant . SCString $ "Jack and Jill"))
                      , pT "Int" (SortSymbol . IdSymbol $ "Int")
                      , pT "Bool" (SortSymbol . IdSymbol $ "Bool")
                      , pT "(_ BitVec 3)" (SortSymbol . IdIndexed "BitVec" $ fromList [IxNumeral "3"])
                      , pT "(List (Array Int Real))" (SortParameter
                                                       (IdSymbol "List")
                                                       (fromList [SortParameter
                                                                   (IdSymbol "Array")
                                                                   (fromList [ SortSymbol . IdSymbol $ "Int"
                                                                             , SortSymbol . IdSymbol $ "Real"])]))
                      , pT "((_ FixedSizeList 4) Real)" (SortParameter
                                                          (IdIndexed "FixedSizeList" (fromList [IxNumeral "4"]))
                                                          (fromList [SortSymbol . IdSymbol $ "Real"]))
                      , pT "(Set (_ Bitvec 3))" (SortParameter
                                                  (IdSymbol "Set")
                                                  (fromList [SortSymbol . IdIndexed "Bitvec" $ fromList [IxNumeral "3"]]))
                      , ps term $ mkTerm [ "(forall ((x (List Int)) (y (List Int)))"
                                         , "(= (append x y)"
                                         , "(ite (= x (as nil (List Int)))"
                                         , "y"
                                         , "(let ((h (head x)) (t (tail x)))"
                                         , "(insert h (append t y))))))"
                                         ]
                      , ps term $ mkTerm [ "(forall ((l1 (List Int)) (l2 (List Int)))"
                                         , "(= (append l1 l2)"
                                         , "(match l1 ("
                                         , "(nil l2)"
                                         , "((cons h t) (cons h (append t l2)))))))"
                                         ] --  Axiom for list append: version 1
                      , ps term $ mkTerm [ "(forall ((l1 (List Int)) (l2 (List Int)))"
                                         , "(= (append l1 l2)"
                                         , "(match l1 ("
                                         , "((cons h t) (cons h (append t l2)))"
                                         , "(_ l2)))))"
                                         ] -- Axiom for list append: version 2
                      , psP "(+ Real Real Real :left-assoc)"
                      , psP "(and Bool Bool Bool :left-assoc)"
                      , psP "(par (X) (insert X (List X) (List X) :right -assoc))"
                      , psP "(< Real Real Bool :chainable)"
                      , psP "(equiv Elem Elem Bool :chainable)"
                      , psP "(par (X) (Disjoint (Set X) (Set X) Bool :pairwise))"
                      , psP "(par (X) (distinct X X Bool :pairwise))"
                      ]
  where
    pI = pe identifier
    pA = pe attribute
    pT = pe sort
    psP = ps parFunSymbolDecl
    mkTerm = T.intercalate "\n"

-- | test the theory declaration from Fig. 3.1 (http://smtlib.cs.uiowa.edu/theories-Core.shtml)
theoryCoreTest = pfs theoryDecl "test/files/Theories-Core.smt2"

logicLIATest = pfs logic "test/files/Logic-LIA.smt2"

specTest = TestList [ lexiconTest, syntaxTest, theoryCoreTest, logicLIATest ]

hornTest = TestList [ pfs script "test/files/sum.smt2"
                    , pfs script "test/files/buildheap.nts.smt2"
                    ]

disjuctionTest = pfs script "test/files/disjuction-head.smt2"

-- | remove the comments
commentTest = TestList [ onlyComment, commentWithString, commentWithSymbol, commentStringInSymbol, commentSymbolInString ]
  where
    onlyComment = rc "; this is a comment\n;so is this\r" " \n \r"
    commentWithString = rc "; this is a comment,\n\"but this isn't, even after ; it won't be\";however\n" " \n\"but this isn't, even after ; it won't be\" \n"
    commentWithSymbol = rc "|;here we go;\n|;not so fast\n\r" "|;here we go;\n| \n\r"
    commentStringInSymbol = rc "|;wait\n I speak \"symbolism;&others\n\";next|;finally\n" "|;wait\n I speak \"symbolism;&others\n\";next| \n"
    commentSymbolInString = rc "\"A |quoted symbol ;example\n| ;actually no\r\n\";unless\r" "\"A |quoted symbol ;example\n| ;actually no\r\n\" \r"
    rc before after = removeComment before ~?= after

extraTest = TestList [ commentTest ]

tests = TestList [ TestLabel "spec" specTest
                 , TestLabel "horn" hornTest
                 , TestLabel "disjuction" disjuctionTest
                 , TestLabel "extra" extraTest
                 ]

main :: IO ()
main = do
  counts <- runTestTT tests
  print counts

