-- | compose epressions together
compose :: [Expr] -> Expr
compose = foldl1 (<>)

-- | Simplifies an expression
simplifyExpr :: Expr -> Expr
simplifyExpr expr =
  let loop 0 expr = expr
      loop n e@(Term {}) = e
      loop n e@(App { eLeft = l, eRight = r }) =
        let l' = simplifyExpr l
            r' = simplifyExpr r
            e' = l' <> r'
            e'' = foldl (Prelude.flip ($)) e' patterns
        in
         if e' /= e''
         then loop (n-1) e''
         else e''
  in loop 100 expr
  where
    patterns = map (\(str, proc) -> matchExpr (readExpr str) proc)
                   [ -- Combinator identities
                     ( "((K ?) ?)", \ [x, _] -> x),
                     ( "(((B ?) ?) ?)", \ [f, g, x] -> f <> (g <> x)),
                     ( "(((C ?) ?) ?)", \ [f, g, x] -> (f <> x) <> g),
                     ( "(((S ?) ?) ?d)", \ [f, g, x] -> (f <> x) <> (g <> x)),
                     ( "(I ?)", \ [x] -> x),
                     -- Arithmetic identities
                     ( "((+ 0) ?)", \ [x] -> x),
                     ( "((+ ?) 0)", \ [x] -> x),
                     ( "((* 1) ?)", \ [x] -> x),
                     ( "((* ?) 1)", \ [x] -> x),
                     ( "((* ?) 1)", \ [x] -> x),
                     ( "((* 0) ?)", \ [_] -> intToExpr 0),
                     ( "((* ?) 0)", \ [_] -> intToExpr 0),
                     -- Evaluation
                     ( "((+ ?t) ?t)", \ [x, y] -> intToExpr ((eval x) + (eval y))),
                     ( "((* ?t) ?t)", \ [x, y] -> intToExpr ((eval x) * (eval y))) ]

-- | Simplifies terms in the grammar, subject to the constraint that it reduces description length
-- Does a greedy, best-first search
simplifyLibrary :: [Expr] -> -- ^ library productions
                   ([Expr], -- ^ new library productions
                    [(Expr, Expr)]) -- ^ substitutions made
simplifyLibrary prods =
  let (newprods, subs) = simplify prods $ score prods
      newprods' = Set.toList $ foldl (\acc prod -> collectSubtrees acc prod) Set.empty newprods
  in (map (\prod -> prod { eType = doTypeInference_ prod}) newprods', subs)
  where -- Score counts # unique subtrees
        score = Set.size . foldl (\acc prod -> collectSubtrees acc prod) Set.empty
        -- Takes as input a library and its score
        simplify lib libScore =
          let simplifiedLib = filter (\ (x, y) -> x /= y) $ map (\prod -> (prod, simplifyExpr prod)) lib
              newLibs = map (\(prod, simpProd) -> let lib' = List.nub $ map (subExpr prod simpProd) lib
                                                  in ((lib', (prod, simpProd)), score lib')) simplifiedLib
              ((newLib, newSub), newScore) = List.minimumBy (compare `on` snd) newLibs
          in
           if newScore < libScore
           then trace ("Improved score from " ++ show libScore ++ " to " ++ show newScore)
                      (let (bestLib, bestSubs) = simplify newLib newScore in (bestLib, newSub:bestSubs))
           else (lib, [])

readExpr :: String -> Expr
readExpr input = case parse parseComb "CL" input of
     Left err -> error $ "No match: " ++ show err
     Right val -> val
     where symbol :: Parser Char
           symbol = oneOf "!#$%&|*+-/:<=>?@^_~.[]?'"
           parseAtom :: Parser Expr
           parseAtom = do
             hd <- letter <|> digit <|> symbol
             tl <- many (letter <|> digit <|> symbol)
             let atom = hd:tl
             case hd of
               '?' -> return $ mkTerm atom undefined undefined -- wildcard
               _ ->
                 return $ case List.find (\c -> show c == atom) basicExprs of
                   Nothing -> error $ "Could not find in library: " ++ show atom
                   Just e -> e
           parseApp :: Parser Expr
           parseApp = do
             char '('
             f <- parseComb
             char ' '
             a <- parseComb
             char ')'
             return $ f <> a
           parseComb :: Parser Expr
           parseComb = parseAtom <|> parseApp
