module Olus.Ast.Conversions where 

import Olus.Ast.Syntax (Scope)
import Olus.Ir.Program (Program)
import Olus.Ast.Conversions.ToString (toString)
import Olus.Ast.Conversions.ToIr (toIr)

astToString :: Scope -> String
astToString = toString

astToIr :: Scope -> Program
astToIr = toIr
