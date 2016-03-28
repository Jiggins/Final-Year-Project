module Parser.SyntaxSpec where

import SpecHelper

import Data.Either
import Text.Parsec

env :: String
env = "HSpec test suite"

main :: IO ()
main = hspec . parallel $ spec

spec :: SpecWith ()
spec = do
  testSymbol
  testLiterals
  testLexpr
  testFunctionApplication
  testTopLevelDeclaration
  testTypeDef

-- * Symbols

testSymbol :: SpecWith ()
testSymbol = describe "Parser.Syntax.variable" $ do
  it "captures a variable, lower-case alpha-numeric word" $
    parse variable env "a" `shouldBe` Right "a"

  it "captures an operator" $
    parse variable env "(!!)" `shouldBe` Right "!!"

  it "should not capture a type, a word beginning with an uppercase letter" $
    parse variable env "Type" `shouldSatisfy` isLeft


testLiterals :: SpecWith ()
testLiterals = describe "Parser.Syntax.literal" $ do
  it "parses a character literal" $
    parse literal env "'c'" `shouldBe` Right (LChar 'c')

  it "should not parse multiple characters" $
    parse literal env "'character" `shouldSatisfy` isLeft

  it "parses a string literal" $
    parse literal env "\"String\"" `shouldBe` Right (LString "String")

  it "parses an empty string " $
    parse literal env "\"\"" `shouldBe` Right (LString "")

  it "parses a whole number as an Int" $
    parse literal env "42" `shouldBe` Right (LInt 42)

  it "parses a floating point number as a Float" $
    parse literal env "42.2" `shouldBe` Right (LFloat 42.2)

  it "will not parse a whole number as a float" $
    parse literal env "42" `shouldNotBe` Right (LFloat 42)


testLexpr :: SpecWith ()
testLexpr = describe "Parser.syntax.lexpr" $ do
  it "parses a simple declaration - `a = 2`" $
    parse expr env "a = 2"
    `shouldBe` Right (Decl (FunctionBinding (Symbol "a") [] (Lit (LInt 2))))

  it "parses a simple declaration - `const a b = a`" $
    parse expr env "const a b = a"
      `shouldBe` Right (Decl (FunctionBinding (Symbol "const") [Symbol "a", Symbol "b"] (Symbol "a")))

  it "parses a simple declaration with type - `a = 2 :: Integer`" $
    parse expr env "a = 2 :: Integer"
      `shouldBe` Right (Decl (FunctionBinding (Symbol "a") [] (TypeSignature (Lit (LInt 2)) Nothing (TypeFunction [Type "Integer" []]))))

  it "parses a complex declaration - `fib = 0:1 : zipWith (+) fib (tail fib)`" $
    parse expr env "fib = 0:1 : zipWith (+) fib (tail fib)"
      -- Starting to look a little like scheme
      `shouldBe` Right (Decl
        (FunctionBinding
          (Symbol "fib")
          []
          (Function (Symbol ":")
            (Function (Lit (LInt 0))
              (Function (Symbol ":")
                (Function (Lit (LInt 1))
                  (Function (Symbol "zipWith")
                    (Function (Symbol "+")
                      (Function (Symbol "fib")
                        (Function
                          (Symbol "tail")
                          (Symbol "fib")))))))))))


testFunctionApplication :: SpecWith ()
testFunctionApplication = describe "Parser.Syntax.fexpr" $ do
  it "parses a function call - `head (tail xs)`" $
    parse expr env "head (tail xs)"
      `shouldBe` Right (Function (Symbol "head") (Function (Symbol "tail") (Symbol "xs")))

  it "parses simple aritmetic - `1 + 1`" $
    parse expr env "1 + 1"
      `shouldBe` Right (Function (Symbol "+") (Function (Lit (LInt 1)) (Lit (LInt 1))))

  it "parses a function call - `f a (g b) c`" $
    parse expr env "f a (g b) c"
      `shouldBe` Right (Function (Symbol "f") (Function (Symbol "a") (Function (Function (Symbol "g") (Symbol "b")) (Symbol "c"))))

  it "parses a function call - `(max 20) (length xs)`" $
    parse expr env "(max 20) (length xs)"
      `shouldBe` Right (Function (Function (Symbol "max") (Lit (LInt 20))) (Function (Symbol "length") (Symbol "xs")))

  it "parses a function call - `zipWith (+) fib (tail fib)`" $
    parse expr env "zipWith (+) fib (tail fib)"
      `shouldBe` Right (Function (Symbol "zipWith") (Function (Symbol "+") (Function (Symbol "fib") (Function (Symbol "tail") (Symbol "fib")))))

-- * Declarations

testTopLevelDeclaration :: SpecWith ()
testTopLevelDeclaration = describe "Parser.Syntax.topLevelDeclaration" $ do
  it "parses a type synonym `type T a = A a`" $
    parse topLevelDeclaration env "type T a = A a"
      `shouldBe` Right (TypeSynonym (Type "T" ["a"]) (Type "A" ["a"]))

  it "parses a type synonym `type String = [Char]`" $
    parse topLevelDeclaration env "type String = [Char]"
      `shouldBe` Right (TypeSynonym (Type "String" []) (TypeList (Type "Char" [])))

  it "parses a data constructor `data Bool = True | False`" $
    parse topLevelDeclaration env "data Bool = True | False"
      `shouldBe` Right (DataCons Nothing (Type "Bool" []) [Type "True" [], Type "False" []] Nothing)

  it "parses a data constructor `data Maybe a = Nothing | Just a deriving (Eq, Show)`" $
    parse topLevelDeclaration env "data Maybe a = Nothing | Just a deriving (Eq, Show)"
      `shouldBe` Right (DataCons Nothing (Type "Maybe" ["a"]) [Type "Nothing" [], Type "Just" ["a"]] (Just ["Eq", "Show"]))

  it "parses a data constructor `data (Eq a, Ord a) => T a = T a`" $
    parse topLevelDeclaration env "data (Eq a, Ord a) => T a = T a"
      `shouldBe` Right (DataCons (Just [Context "Eq" "a", Context "Ord" "a"]) (Type "T" ["a"]) [Type "T" ["a"]] Nothing)

  it "parses a newtype constructor `newtype Ea a => T a b = T a b deriving (Eq)`" $
    parse topLevelDeclaration env "newtype Eq a => T a b = T a b deriving (Eq)"
      `shouldBe` Right (NewType (Just [Context "Eq" "a"]) (Type "T" ["a", "b"]) (Type "T" ["a", "b"]) (Just ["Eq"]))

  it "does not parse when deriving is missing parentheses" $
    parse topLevelDeclaration env "newtype Eq a => T a b = T a b deriving Eq"
      `shouldSatisfy` isLeft

testTypeDef :: SpecWith ()
testTypeDef = describe "Parser.Syntax.typeSignature" $ do
  it "parses a simple type definition - `a :: Int`" $
    parse expr env "a :: Int"
      `shouldBe` Right (TypeSignature (Symbol "a") Nothing (TypeFunction [Type "Int" []]))

  it "parses a simple type definition - `a :: Num a => a" $
    parse expr env "a :: Num a => a"
      `shouldBe` Right (TypeSignature (Symbol "a") (Just [Context "Num" "a"]) (TypeFunction [Type "a" []]))

  it "parses a simple type definition - `map :: (a -> b) -> [a] -> [b]`" $
    parse expr env "map :: (a -> b) -> a -> b"
      `shouldBe` Right (TypeSignature (Symbol "map") Nothing (TypeFunction [TypeFunction [Type "a" [], Type "b" []], Type "a" [], Type "b" []]))
