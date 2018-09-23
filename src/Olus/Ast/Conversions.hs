module Olus.Ast.Conversions where 

import Olus.Ast.Syntax (Scope)
import Olus.Ir.Program (Program)
import Olus.Ir.Module (Module)
import Olus.Ast.Conversions.ToString (toString)
import Olus.Ast.Conversions.ToIr (toIr)
import Olus.Ast.Conversions.ToModule (toModule)

astToString :: Scope -> String
astToString = toString

astToIr :: Scope -> Program
astToIr = toIr

astToModule :: Scope -> Module
astToModule = toModule
