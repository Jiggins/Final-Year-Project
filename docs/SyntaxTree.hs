expr → infixexp :: [context =>] typeExpr
     | infixexp

infixexp → lexp operator infixoperator
         | - infixexp
         | lexp

lexp → \ {variable} -> expr
     | variable = expr
     | let declarations in expr
     | if expr then expr else expr
     | fexpr

fexpr → [fexpr] aexpr

aexpr → (expr)
      | variable
      | constructor
      | literal
      | ( expr , ... , expr )           (tuple)
      | [ expr , ... , expr ]           (list)
      | [ expr , expr .. [expr] ]       (list sequence)
      | ( infixexp infixOperator )      (sectionLeft)
      | ( infixOperator infixexp )      (sectionRight)
