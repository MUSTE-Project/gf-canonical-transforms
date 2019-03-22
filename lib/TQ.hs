{-# OPTIONS_GHC -Wincomplete-patterns #-}
-- | Traverse qualified identifiers in a Canonical GF grammar
module TQ where
import Data.Functor.Const
import Data.Functor.Identity(runIdentity)
import GF.Grammar.Canonical

mapQ f = runIdentity . tq (pure . f)
collectQ f = getConst . tq (Const . f)
listQ s = collectQ (:[]) s

class TQ s where
  tq :: Applicative m => (QualId-> m QualId) -> s -> m s

instance TQ a => TQ [a]   where tq = traverse . tq
instance TQ b => TQ (a,b) where tq = traverse . tq -- hmm

instance TQ Grammar where
  tq f (Grammar abs cncs) = Grammar abs <$> tq f cncs

instance TQ Concrete where
  tq f (Concrete cm am flags params lincats linfuns) =
    Concrete cm am flags <$> tq f params <*> tq f lincats <*> tq f linfuns    

instance TQ ParamDef where
  tq f (ParamDef pn pds) = ParamDef <$> tq f pn <*> tq f pds
  tq f (ParamAliasDef pn lt) = ParamAliasDef <$> tq f pn <*> tq f lt

instance TQ LincatDef where
  tq f (LincatDef cat lt) = LincatDef cat <$> tq f lt

instance TQ LinDef where
  tq f (LinDef fun vs lv) = LinDef fun vs <$> tq f lv

instance TQ LinType where
  tq f (ParamType pt) = ParamType <$> tq f pt
  tq f (RecordType r) = RecordType <$> tq f r
  tq f (TableType lt1 lt2) = TableType <$> tq f lt1 <*> tq f lt2
  tq f (TupleType lts) = TupleType <$> tq f lts
  tq f lt = pure lt

instance TQ ParamType where
  tq f (ParamTypeId pn) = ParamTypeId <$> tq f pn

instance TQ LinValue where
  tq f v =
    case v of
      ConcatValue v1 v2 -> ConcatValue <$> tq f v1 <*> tq f v2
      ParamConstant pv -> ParamConstant <$> tq f pv
      RecordValue r -> RecordValue <$> tq f r
      TableValue lt trs -> TableValue <$> tq f lt <*> tq f trs
      TupleValue vs -> TupleValue <$> tq f vs
      VariantValue vs -> VariantValue <$> tq f vs
      VarValue vn -> VarValue <$> tq f vn
      PreValue alts def -> PreValue <$> tq f alts <*> tq f def
      Projection v l -> flip Projection l <$> tq f v
      Selection v1 v2 -> Selection <$> tq f v1 <*> tq f v2
      CommentedValue s v -> CommentedValue s <$> tq f v
      LiteralValue _ -> pure v
      ErrorValue _ -> pure v
      PredefValue _ -> pure v

instance TQ LinPattern where
  tq f p =
    case p of
      ParamPattern pp -> ParamPattern <$> tq f pp
      RecordPattern r -> RecordPattern <$> tq f r
      TuplePattern ps -> TuplePattern <$> tq f ps
      WildPattern -> pure WildPattern

instance TQ arg => TQ (Param arg) where
  tq f (Param pn as) = Param <$> tq f pn <*> tq f as

instance TQ rhs => TQ (RecordRow rhs) where tq = traverse . tq

instance TQ rhs => TQ (TableRow rhs) where
  tq f (TableRow p rhs) = TableRow <$> tq f p <*> tq f rhs

instance TQ VarValueId where
  tq f (VarValueId q) = VarValueId <$> f q

instance TQ ParamId where
  tq f (ParamId q) = ParamId <$> f q
