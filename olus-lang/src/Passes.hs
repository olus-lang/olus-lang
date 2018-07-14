import Program


-------------
-- Utilties -
-------------


--------------------
-- Analysis passes -
--------------------

-- computeClosures :: Program -> Program

-------------------
-- Cleanup passes -
-------------------

-- removeUnusedConstants :: Program -> Program

-- removeUnusedDeclarations :: Program -> Program

-- deduplicateConstants :: Program -> Program

-- deduplicateDeclarations :: Program -> Program

------------------------
-- Optimization passes -
------------------------

-- inlineConstantClosures :: Program -> Program

-- inlineBuiltinClosures :: Program -> Program

---------------------------
-- Target specific passes -
---------------------------

-- Limit the maximum number of arguments of a declaration.
-- Larger declarations are split and use a closure.

-- limitArgumentCount :: Integer -> Program -> Program


-- TODO: What is the reverse of this transformation?
