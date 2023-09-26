data PrefixFusionTree a =
  Fan [PrefixFusionTree a] |
  Branch a (PrefixFusionTree a) |
  Empty deriving (Show)

embbed :: Eq a => [a] -> PrefixFusionTree a -> PrefixFusionTree a
embbed (a : b) (Branch v for)
    | a == v = (Branch a (embbed b for))
    | True = Fan ((Branch a (unroll b)) : (Branch v for) : [])
embbed a Empty = (unroll a)
embbed [] _ = Empty
embbed k (Fan n) = Fan (htr k n)

htr :: Eq a => [a] -> [PrefixFusionTree a] -> [PrefixFusionTree a]
htr j@(a : b) (z@(Branch d e) : m)
    | a == d = (Branch a (embbed b e)) : m
    | True = z : htr j m
htr j [] = unroll j : []
htr [] (_ : _) = []

unroll :: [a] -> PrefixFusionTree a
unroll (a : []) = Branch a (Empty)
unroll (a : b) = Branch a (Fan [unroll b])

satisfies :: Eq a => [a] -> PrefixFusionTree a -> Bool
satisfies (_ : _) Empty = False
satisfies [] _ = True
satisfies k@(a : b) (Fan ((Branch c f): d))
    | a == c = satisfies b f
    | True = foldr (||) False (map (\e -> satisfies k e) d)
satisfies k@(a : b) (Branch v f)
    | a == v = satisfies b f
    | True = False