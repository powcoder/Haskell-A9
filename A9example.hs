https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
-- Test examples used by the autograder.
--
-- If you want to use them, just paste everything here into your 
-- file (anywhere) if they're not already there. 
--
-- Usage:
--
-- 1. Use the ones before the line directly. You don't need to read anything
-- after that.
--
-- 2. Type "showDefs" in ghci to see a list of all the examples. 
-- 
-- 3. To use the examples, you can just use the names in a term and then
-- run elimDefs to replace the names by the terms they stand for.
-- E.g. pp $ elimDefs $ parse "id id" = "(%x. x)(%x. x)".


[u,v,w,x,y,z] = words "u v w x y z"

f = False
t = True

defStrs = 
    [("id", "%x. x")
    ,("true", "%x,y. x")
    ,("false", "%x,y. y")
    ,("pair", "%x,y,f. f x y")
    ,("fst", "%p. p true")
    ,("snd", "%p. p false")
    ,("zero", "id")
    ,("one", "pair false zero")
    ,("two", "pair false one")
    ,("three", "pair false two")
    ,("isZero", "%x. fst x")
    ,("if", "%b,x,y. b x y")
    ,("s", "%n. pair false n")
    ,("p", "%n. snd n")
    ,("Y", "% f. (%x. f(x x)) (% x. f(x x))")
    ]

defs = map (fmap parse) defStrs

isDef :: String -> Bool
isDef = (`elem` (map fst defStrs))

-- replace all defined variables by the term they define
elimDefs :: L -> L
elimDefs m =
    let  substAll term = foldr (\ (name, n) m -> subst m n name) term defs
    in if null (freeVars m)
          then m
          else elimDefs (substAll m)

-- succeed Or Die
od :: Maybe a -> a
od (Just x) = x
od Nothing = error "od: Nothing"

def name = od $ lookup name defs

showDef :: String -> IO ()
showDef name = 
    putStrLn $ unwords [name, "=", pp $ def name]

showDefs :: IO ()
showDefs = 
    mapM showDef (map fst defStrs) >> return ()

lId = def "id"
lTrue = def "true"
lFalse = def "false"
lPair = def "pair"
lFst = def "fst"
lSnd = def "snd"
lZero = def "zero"
lOne = def "one"
lTwo = def "two"
lThree = def "three"
lIsZero = def "isZero"
lIf = def "if"
lS = def "s"
lP = def "p"
lY = def "Y"
