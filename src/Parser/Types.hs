{-# LANGUAGE DeriveDataTypeable #-}

module Parser.Types where

import Data.Data

type Class       = String
type Constructor = String
type Var         = String

data Module = Module Class [Expr]
  deriving (Eq,Ord,Show,Data,Typeable)

data Expr
  = Expr Expr
  | Lit Literal
  | Negate Expr
  | Symbol String
  | Constructor String
  | InfixOperator String
  | Function Expr Expr
  | Lambda [Var] Expr
  | Decl Declaration
  | Let Declaration
  | LetIn [Declaration] Expr
  | IfThenElse Expr Expr Expr
  | TypeSignature Expr (Maybe [Context]) Type
  | Tuple [Expr]
  | List [Expr]
  | ListSequence Expr (Maybe Expr) [Expr]
  | Module' Module
  deriving (Eq,Ord,Show,Data,Typeable)

data Literal
  = LChar    Char     -- ^ character literal
  | LString  String   -- ^ string literal
  | LInt     Integer  -- ^ integer literal
  | LFloat   Double   -- ^ floating point literal
  deriving (Eq,Ord,Show,Data,Typeable)

data Declaration
  = TypeSynonym Type Type
  | FunctionBinding Expr [Expr] Expr
  | DataCons (Maybe [Context]) Type [Type] (Maybe [Class])
  | NewType  (Maybe [Context]) Type Type (Maybe [Class])
  | Class    (Maybe [Context]) Type [Declaration]
  | Instance (Maybe [Context]) Type [Declaration]
  deriving (Eq,Ord,Show,Data,Typeable)

data Context = Context Var Var
  deriving (Eq,Ord,Show,Data,Typeable)

data Type
  = Type Var [Var]       -- ^ a type with type variables `:: Num a`
  | TypeFunction [Type]  -- ^ a type function `:: (a -> b)`
  | TypeList Type
  deriving (Eq,Ord,Show,Data,Typeable)
