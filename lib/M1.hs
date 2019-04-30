-- | Monad for floating tables to the top level
{-# LANGUAGE DeriveFunctor #-}
module M1(V(..),table,runV) where
import GF.Grammar.Canonical(LinType(..),LinPattern(..))
import GF.Text.Pretty(render80)
import Utils(nub')

data V a = Pure a | Table LinType [(LinPattern,a)] deriving Functor

table ty rows =
  case (ty,rows) of
    (TupleType [],[(TuplePattern [],v)]) -> Pure v
    _ -> Table ty rows

runV (Pure a) = [a]
runV (Table t bs) = map snd bs

instance Applicative V where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Table t bs = Table t [(p,f v) | (p,v)<-bs]
  Table t bs <*> Pure v = Table t [(p,f v) | (p,f)<-bs]
  Table tf fs <*> Table ta as =
    Table (TupleType [tf,ta]) [(TuplePattern [pf,pa],f a) | (pf,f)<-fs,(pa,a)<-as]

instance Monad V where
  return = pure
  Pure a >>= f = f a
  Table ty1 bs >>= f = table' (rows=<<bs)
    where
      table' tyrs = Table ty rows
        where
          (tys,rows) = unzip tyrs
          ty = case nub' (map simpT tys) of
                 [] -> error "empty table"
                 [ty2] -> TupleType [ty1,ty2]
                 tys -> error $ "heterogenous table:\n"++unlines (map render80 tys)
      rows (p1,v1) =
        case f v1 of
          Pure v2 -> [(TupleType [],(TuplePattern [p1,TuplePattern []],v2))]
          Table ty2 bs2 -> [(ty2,(TuplePattern [p1,p2],v2)) | (p2,v2)<-bs2]

      simpT (TupleType [t1,TupleType []]) = t1
      simpT t = t
