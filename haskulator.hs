{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
import Data.Char (isNumber, isSpace)


infixl |>
v |> f = f v

main = do
  print "Hey! I am ready to accept input :)"
  mainLoop

mainLoop = do
  inp <- getLine
  let cleanInp = filter (not . isSpace) inp
  case tryTokeniseExpr cleanInp of
    Just (a , _) -> print (resolvePrecendence a >>= verifyTree >>= Just . retract)
    Nothing -> print "oopsie! failed to parse that X_X"
  mainLoop


data RawItem =
  Subexpr [RawItem] |
  NumLit String |
  Op OpArg2
  deriving (Show, Eq)

data OpArg2 =
  Mul |
  Plus |
  Minus |
  Div
  deriving (Show, Eq)


takePrefixWhile :: (a -> Bool) -> [a] -> ([a], [a])
takePrefixWhile bc l = let (a , b) = aux [] l in (reverse a , b)
  where
    aux acc rest@(h : t) = if (bc h) then aux (h : acc) t else (acc , rest)
    aux [] [] = ([], l)
    aux acc [] = (acc , [])

unprefix arg1 arg2 =
  let aux (a : b) tail@(c : d) | a == c = aux b d | otherwise = Nothing
      aux [] rest = Just rest
  in aux arg1 arg2

prefixMatch a b = case unprefix a b of Just _ -> True ; Nothing -> False


tryStripNum str =
  takePrefixWhile (\i -> map (\p->p i) [isNumber ,(== '.')] |> any (==True)) str |> screen (not . null . fst)

screen :: (t -> Bool) -> t -> Maybe t
screen p d = if p d then Just d else Nothing


infixl ??
Just l ?? _ = Just l
Nothing ?? Just r = Just r
Nothing ?? Nothing = Nothing


tryTokeniseExpr str = expr [] str
  where
    expr tks [] = Just (reverse tks , [])
    expr tks (')' : tail) = Just $ (reverse tks , tail)
    expr tks str =
      (unprefix "+" str >>= expr (Op Plus : tks) ) ??
      (unprefix "*" str >>= expr (Op Mul : tks) ) ??
      (unprefix "-" str >>= expr (Op Minus : tks) ) ??
      (unprefix "/" str >>= expr (Op Div : tks) ) ??
      (tryStripNum str >>= \(lit, tail)-> expr (NumLit lit : tks) tail) ??
      subexpr tks str
    subexpr tks ('(' : str) = expr [] str >>= \(s,t) -> expr (Subexpr s : tks) t
    subexpr _ _ = Nothing




data ExprTree =
  Plus_ ExprTree ExprTree |
  Minus_ ExprTree ExprTree |
  Mul_ ExprTree ExprTree |
  Div_ ExprTree ExprTree |
  NumberLit String
  deriving Show


splitBy s seqv =
  let (a, b) = firstSplitBy s seqv in a : if null b then [] else splitBy s (tail b)

firstSplitBy s seqv = aux [] seqv
  where
    aux r rest@(h : t) | s h = (reverse r, rest) | otherwise = aux (h : r) t
    aux r [] = (reverse r , [])

tail_ [] = []
tail_ (_ : t) = t

resolvePrecendence :: [RawItem] -> Maybe ExprTree
resolvePrecendence (NumLit s : []) = Just $ NumberLit s
resolvePrecendence (Subexpr s : []) = resolvePrecendence s
resolvePrecendence items =
  (splitAux (Op Minus) Minus_ items) ??
  (splitAux (Op Plus) Plus_ items) ??
  (splitAux (Op Div) Div_ items) ??
  (splitAux (Op Mul) Mul_ items)
  where
    splitAux op ctor items =
      ((firstSplitBy (== op) items |> screen (\(f,s)->let c = not . null in c f && (c $ tail_ s)))
        >>= \(l, r) -> (resolvePrecendence l) >>= \l -> (resolvePrecendence $ tail_ r) >>= \r ->
          Just $ ctor l r)



data CheckedExprTree =
  ChPlus_ CheckedExprTree CheckedExprTree |
  ChMinus_ CheckedExprTree CheckedExprTree |
  ChMul_ CheckedExprTree CheckedExprTree |
  ChDiv_ CheckedExprTree CheckedExprTree |
  ChNumberLit Double
  deriving Show


verifyTree :: ExprTree -> Maybe CheckedExprTree
verifyTree = \case
  Plus_ a b -> chk a b ChPlus_
  Minus_ a b -> chk a b ChMinus_
  Mul_ a b -> chk a b ChMul_
  Div_ a b -> chk a b ChDiv_
  NumberLit s -> filter (=='.') s |> screen ((<= 1) . length) >>= \_ ->Just $ ChNumberLit $ read s
  where
    chk a b ctor = verifyTree a >>= \a->verifyTree b >>= \b-> Just $ ctor a b


retract :: CheckedExprTree -> Double
retract = \case
  (ChPlus_ a b) -> op (+) a b
  (ChDiv_ a b) -> op (/) a b
  (ChMinus_ a b) -> op (-) a b
  (ChMul_ a b ) -> op (*) a b
  (ChNumberLit n) -> n
  where op op_ a b = op_ (retract a) (retract b)