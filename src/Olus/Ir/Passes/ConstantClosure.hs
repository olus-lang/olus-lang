module Olus.Ir.Passes.ConstantClosure where

import qualified Olus.Ir.Program as P

-- TODO Extract constant closures
--      Note that this has cascading effects, so we should solve the system
--      first.

-- Create a Closure constant. Replace instances.

constantClosure :: P.Program -> P.Program
constantClosure prog = undefined
