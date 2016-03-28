module Parser.Syntax where

import Util

import Parser.Lexer
import Parser.Types

import Text.Parsec

-- * Expressions

expr :: Parsec String u Expr
expr =  try typeDefinition
    <|> infixexp

infixexp :: Parsec String u Expr
infixexp =  try infixexp'
        <|> try (Negate <$> (reservedOp "-" *> infixexp))
        <|> lexp

lexp :: Parsec String u Expr
lexp =  lambdaExpression
    <|> try (Decl <$> declaration)
    <|> try letExpression 
    <|> try ifThenElse
    <|> fexpr

fexpr :: Parsec String u Expr
fexpr =  try (Function <$> aexpr <*> fexpr)
     <|> aexpr

aexpr :: Parsec String u Expr
aexpr =  try (parens expr)
     <|> try (Symbol <$> variable)
     <|> try (Constructor <$> constructor)
     <|> (Lit <$> literal)
     <|> tuple
     <|> try list
     <|> try listSequence
     <|> try sectionLeft
     <|> sectionRight

moduleSignature :: Parsec String u Module
moduleSignature =
  try (Module <$> (reserved "module" *> typeIdentifier)
              <*> (reserved "where"  *> many expr))
  <|> (Module <$> pure "Main" <*> many expr)

-- * Declarations

typeDefinition :: Parsec String u Expr
typeDefinition = TypeSignature
  <$> infixexp
  <*  reservedOp "::"
  <*> optionMaybe (try (context <* reservedOp "=>"))
  <*> (TypeFunction <$> sepBy1 typeExpr (reservedOp "->"))

topLevelDeclaration :: Parsec String u Declaration
topLevelDeclaration = typeSynonym
  <|> dataCons
  <|> newType
  <|> classCons
  <|> instanceCons

typeSynonym :: Parsec String u Declaration
typeSynonym = TypeSynonym
  <$> (reserved "type" *> typeExpr)
  <*> (reservedOp "="  *> typeExpr)

dataCons :: Parsec String u Declaration
dataCons = DataCons
  <$> (reserved "data"
   *> optionMaybe (try (context <* reservedOp "=>")))
  <*> typeExpr
  <*> (reservedOp "=" *> sepBy1 typeExpr (reservedOp "|"))
  <*> optionMaybe (reserved "deriving" *> parens (commaSep1 typeIdentifier))

newType :: Parsec String u Declaration
newType = NewType
  <$> (reserved "newtype"
   *> optionMaybe (try (context <* reservedOp "=>")))
  <*> typeExpr
  <*> (reservedOp "=" *> typeExpr)
  <*> optionMaybe (reserved "deriving" *> parens (commaSep1 typeIdentifier))

classCons :: Parsec String u Declaration
classCons = Class
  <$> (reserved "class"
   *> optionMaybe (try (context <* reservedOp "=>")))
  <*> (typeExpr <*  reserved "where")
  <*> many1 declaration

instanceCons :: Parsec String u Declaration
instanceCons = Instance
  <$> (reserved "instance"
   *> optionMaybe (try (context <* reservedOp "=>")))
  <*> (typeExpr <* reserved "where")
  <*> many1 declaration

typeExpr :: Parsec String u Type
typeExpr =  squares (TypeList <$> (Type <$> value <*> many identifier))
        <|> parens (TypeFunction <$> sepBy1 typeExpr (reservedOp "->"))
        <|> (Type <$> value <*> many identifier)

context :: Parsec String u [Context]
context = parens (commaSep1 (Context <$> constructor <*> identifier))
  <|> fmap (:[]) (Context <$> constructor <*> identifier)

declaration :: Parsec String u Declaration
declaration = FunctionBinding
  <$> (Symbol <$> variable)
  <*> many (Symbol <$> variable) <* reservedOp "="
  <*> expr

ifThenElse :: Parsec String u Expr
ifThenElse = IfThenElse
  <$> (reserved "if"   *> expr)
  <*> (reserved "then" *> expr)
  <*> (reserved "else" *> expr)

-- * Symbols, Constructors, Operators, and Literals

variable :: Parsec String u String
variable = identifier <|> parens operator

-- | Accepts any capitalised word, operator surrounded in parentheses,
-- `()` and `[]`
constructor :: Parsec String u String
constructor = typeIdentifier
  <|> char '[' *> char ']' *> return "[]"
  <|> try (char '(' *> char ')' *> return "()")
  <|> try (fmap (surround '(' ')' . concat) . parens $ many1 comma)
  <|> parens operator

value :: Parsec String u String
value = constructor <|> variable   

infixOperator :: Parsec String u String
infixOperator =  operator
             <|> char '`' *> identifier <* char '`'

literal :: Parsec String u Literal
literal = LChar <$> charLiteral
  <|> LString <$> stringLiteral
  <|> try (LFloat <$> float)
  <|> LInt <$> integer

-- * Lists and tuples

list, tuple :: Parsec String u Expr
list  = List <$> squares (sepBy expr comma)
tuple = Tuple <$> parens (sepBy expr comma)

listSequence :: Parsec String u Expr
listSequence = squares $ ListSequence
  <$> expr
  <*> optionMaybe (reservedOp "," *> expr)
  <*  reservedOp ".."
  <*> many expr

-- * Curried Applications and Lambda Abstractions

lambdaExpression :: Parsec String u Expr
lambdaExpression = do
  reservedOp "\\"
  variables  <- sepBy (identifier <|> parens operator) space
  reservedOp "->"
  expression <- expr
  return $ Lambda variables expression

infixexp' :: Parsec String u Expr
infixexp' = do
  left  <- fexpr
  op    <- Symbol <$> infixOperator
  right <- infixexp
  return (Function op (Function left right))

sectionLeft :: Parsec String u Expr
sectionLeft = parens $ do
  ex <- infixexp
  op <- Symbol <$> operator
  return $ Function op ex

sectionRight :: Parsec String u Expr
sectionRight = parens (Function <$> (Symbol <$> fmap (surround '(' ')') operator) <*> expr)

-- * Let expressions

letExpression :: Parsec String u Expr
letExpression = reserved "let" >> (try letExp <|> letInExp)

letExp :: Parsec String u Expr
letExp = Let <$> declaration

letInExp :: Parsec String u Expr
letInExp = LetIn <$> many1 declaration <*> (reserved "in" *> expr)
