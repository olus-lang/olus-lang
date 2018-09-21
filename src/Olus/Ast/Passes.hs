module Olus.Ast.Passes where 

import Olus.Ast.Syntax (Scope)
import Olus.Ast.Utils.IdGen (runIdGen')
import Olus.Ast.Passes.Glucase as Glu
import Olus.Ast.Passes.Fructase as Fru
import Olus.Ast.Passes.Galactase as Gal
import Olus.Ast.Passes.Desugar as Des

glucase :: Scope -> Scope
glucase = runIdGen' Glu.glucase

fructase :: Scope -> Scope
fructase = runIdGen' Fru.fructase

galactase :: Scope -> Scope
galactase = runIdGen' Gal.galactase

desugar :: Scope -> Scope
desugar = Des.desugar
