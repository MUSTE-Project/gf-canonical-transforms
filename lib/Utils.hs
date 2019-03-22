module Utils where
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Tuple(swap)

traverse2 f = traverse (traverse f)

mapSnd f = map (fmap f)

collectByFst xys = M.toList . M.fromListWith (++) $ map (fmap (:[])) xys

collectBySnd xys = map swap . collectByFst $ map swap xys

-- Result is delivered lazily, order is preserved,
-- nub xs == xs, if no duplicates
nub' xs = nubOn id xs

nubOn f xs = nubs S.empty xs
  where
    nubs s [] = []
    nubs s (x:xs) = if y `S.member` s
                    then nubs s xs
                    else x:nubs (S.insert y s) xs
     where y = f x

segments p [] = []
segments p xs = case span p xs of
                  (xs1,_:xs2) -> xs1:segments p xs2
                  _ -> [xs]
--------------------------------------------------------------------------------
{-
data Two a = Two a a deriving (Functor,Foldable,Traversable)

instance Applicative Two where
  pure a = Two a a
  Two f g <*> Two x y = Two (f x) (g y)
-}