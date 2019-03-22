-- | Transform a (monolingual) grammar by eliminating some parameter
-- distinctions, introducing more top-level functions instead,
-- along the ideas outlined in
-- <https://github.com/MUSTE-Project/MULLE/wiki/Transforming-a-GF-grammar>.
module ConflateParams(conflateParams,Eqn) where
import Data.List(intercalate,partition,sortBy,transpose)
import Data.Function(on)
import Control.Monad(join)
import GF.Text.Pretty(render80,punctuate)
import GF.Grammar.Canonical
import GF(selection)
import Utils
import M1
--import Debug.Trace

conflateParams  :: [Eqn] -> Grammar -> Grammar
conflateParams1 :: Eqn   -> Grammar -> Grammar

-- | A list of (unqualified) parameter names that should be conflated,
-- e.g. @[\"Pl\",\"Sg\"]@
type Eqn = [String] -- representing e.g. P1==P2==P3 

conflateParams eqns gr = foldr conflateParams1 gr eqns

conflateParams1 eqn (Grammar abs [cnc]) =
    Grammar (transAbs fs abs) [cnc']
  where
    (fs,cnc') = transCnc cnc

    transAbs fs (Abstract an flags cats funs) =
        Abstract an flags cats (concatMap transFun funs)
      where
        transFun d@(FunDef f ty) = maybe [d] newfuns (lookup f fs)
          where newfuns fs = [FunDef f ty|f<-fs]

    transCnc (Concrete cn an flags params lincats lins) =
        (fs,Concrete cn an flags params' lincats lins')
      where
        params' = map transParamDef params
        (fss,linss') = unzip $ map transLin lins
        fs = concat fss
        lins' = concat linss'

    transParamDef p@(ParamAliasDef{}) = p
    transParamDef (ParamDef p pvs)    = ParamDef p (transParams [eqn] pvs)
--{-
    transLin (LinDef f xs v) =
     --trace (render80 f) $
       case trans v of
         Pure v' -> ([],[LinDef f xs v'])
         Table t bs -> ([(f,fs)],lins)
           where
              (fs,lins) = unzip [(f',LinDef f' xs (CommentedValue (render80 (punctuate "," (nub' ps))) v'))
                                 |(n,(ps,v'))<-zip [1..] (collectBySnd bs),
                                  let f'= funId f n]
--}
{--
    transLin (LinDef f xs v) =
       case runV (trans v) of
         [v'] -> ([],[LinDef f xs v'])
         vs   -> ([(f,fs)],lins)
           where
              (fs,lins) = unzip [(f',LinDef f' xs v')
                                 | (n,v')<-zip [1..] (nub' vs),
                                   let f'= funId f n]
--}
    funId (FunId f) n = FunId (f++"_"++show (n::Int))

    unqual (Qual _ n) = n
    unqual (Unqual n) = n

    ----------------------------------------------------------------------------

    transParams [] pvs = pvs
    transParams (eqn:eqns) pvs = combine pvs1++transParams eqns pvs2
      where
        (pvs1,pvs2) = partition inEqn pvs
        inEqn (Param (ParamId q) []) = unqual q `elem` eqn
        inEqn _ = False

        combine [] = []
        combine ps = [Param (mergedParam q eqn) []]
          where
            q:_ = [q|Param (ParamId q) []<-ps]

    ttrans vs = traverse trans vs
    t2trans vs = traverse2 trans vs

    trans :: LinValue -> V LinValue
    trans v =
      case v of
        ConcatValue v1 v2 -> ConcatValue <$> trans v1 <*> trans v2
        Projection lv l -> flip Projection l <$> trans lv
        Selection tv pv -> selection <$> trans tv <*> trans pv
        VariantValue vs -> VariantValue <$> ttrans vs
        ParamConstant pv -> ParamConstant <$> transP pv
        RecordValue rvs -> RecordValue <$> t2trans rvs
        PreValue alts def -> PreValue <$>t2trans alts <*> trans def
        TableValue ty trs -> transTable ty trs
        TupleValue lvs -> TupleValue <$> ttrans lvs
        CommentedValue "impossible" v -> trans v
        CommentedValue s v -> CommentedValue s <$> trans v
        LiteralValue _ -> pure v
        VarValue _ -> pure v
        PredefValue _ -> pure v
        ErrorValue _ -> pure v {-
        _ -> trace ("trans "++show v) $
             pure v-}

    transP (Param c []) = pure (Param (transP' c) [])
    transP (Param c vs) = Param c <$> ttrans vs

    transP' c@(ParamId q) =
      case [cs|cs<-[eqn],unqual q `elem` cs] of
        [] -> --trace ("transP' "++show c)
              c
        [cs] -> mergedParam q cs

    transTable ty trs =
        if n<=1
        then --[] -> TableValue ty . zipTR ps <$> tvs
             transposeTable ty ps (map trans vs)
        else do vs' <- transAll vs -- !!!
                snd (factorTable ty (zipTR ps vs'))
      where
        (ps,vs) = unzipTR trs
        (n,_) = factorTable ty trs -- !!!

    factorTable ty trs =
        (n,table ty (zip ps' (map (TableValue ty . zipTR ps) trrs)))
      where
        (ps,rss) = unzip $ collectByFst [(transPat p,(p,v))|TableRow p v<-trs]
        n = maximum (map length rss)
        pad = take n . cycle
        ps' : _ = [prunedPats (map fst rs) | rs<-rss,length rs==n]
        trrs = transpose (map (pad.map snd) rss)
        
    transAll vs =
        picks <$> t2trans (collectBySnd (zip [1..] vs))
      where
        picks nvs= map snd $ sortBy (compare `on` fst) [(n,v)|(ns,v)<-nvs,n<-ns]



    prunedPats = map prunedPat
    transPats  = map transPat
    
  --splitPats  = map splitPat 
  --splitPat p = (prunedPat,transPat p)

    prunedPat p =
      case p of
        ParamPattern pp -> prunedPP pp
        RecordPattern rps -> pWildP (RecordPattern . zipRow ls) ps
          where (ls,ps) = unzipRow rps
        TuplePattern ps -> pWildP TuplePattern ps
        WildPattern -> WildPattern

    transPat p =
      case p of
        ParamPattern pp -> transPP pp
        RecordPattern rps -> RecordPattern . zipRow ls $ transPats ps
          where (ls,ps) = unzipRow rps
        TuplePattern ps -> TuplePattern (transPats ps)
        WildPattern -> WildPattern

    prunedPP p@(Param c []) =
        if transP' c==c then WildPattern else ParamPattern p
    prunedPP (Param c ps) = pWildP (ParamPattern . Param c) ps

    transPP (Param c []) = ParamPattern (Param (transP' c) [])
    transPP (Param c ps) = ParamPattern $ Param c (transPats ps)


    pWildP f = wildP f . prunedPats

    wildP c ps = if all (==WildPattern) ps then WildPattern else c ps

transposeTable ty ps ts =
  curryTable . swapTable $ join (table ty (zip ps ts))

swapTable (Table (TupleType [ty1,ty2]) bs) =
    Table (TupleType [ty2,ty1]) (map swapRow bs)
  where
    swapRow (TuplePattern [p1,p2],v) = (TuplePattern [p2,p1],v)

curryTable (Table (TupleType [ty1,ty2]) bs) = table ty1 rows
  where
    rows = mapSnd table' $ curryRows (map conv bs)
    table' bs = tableValue ty2 (map (uncurry TableRow) bs)
    conv (TuplePattern [p1,p2],v) = ((p1,p2),v)

tableValue ty rows0 | ps == nub' ps = TableValue ty rows
                    | otherwise = error $ "duplicated patterns in table\n" ++
                                          unlines (map render80 ps)
  where
    rows = nub' rows0
    (ps,_) = unzipTR rows

--transposeRows bs = curryRows . swapRows . uncurryRows $ bs
--uncurryRows bs = [((p1,p2),v) | (p1,bs1) <- bs, (p2,v)<-bs1]
--swapRows bs = [((p2,p1),v) | ((p1,p2),v)<-bs]
curryRows bs = collectByFst [(p1,(p2,v))|((p1,p2),v)<-bs]
{-
allMatch ps1 ps2 = length ps1==length ps2 && and (zipWith match ps1 ps2)

match _ WildPattern = True
match (ParamPattern (Param c1 ps1)) (ParamPattern (Param c2 ps2)) =
  c1==c2 && allMatch ps1 ps2
match (RecordPattern rs1) (RecordPattern rs2) =
    ls1==ls2 && allMatch ps1 ps2 -- good enough for the purpose here
  where
    (ls1,ps1) = unzipRow rs1
    (ls2,ps2) = unzipRow rs2
match (TuplePattern ps1) (TuplePattern ps2) = allMatch ps1 ps2
match _ _ = False
-}
--------------------------------------------------------------------------------

unzipRow row = unzip [(l,v)|RecordRow l v<-row]
zipRow = zipWith RecordRow

unzipTR tr = unzip [(p,v)|TableRow p v<-tr]
zipTR = zipWith TableRow

requal (Unqual _) q = Unqual q
requal (Qual m _) q = Qual m q

mergedParam q cs = ParamId (requal q (intercalate "Or" cs))
