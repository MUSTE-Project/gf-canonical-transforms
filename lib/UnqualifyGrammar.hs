-- | Replace qualified names with unqualified names when possible without
-- introducing name clashes.
module UnqualifyGrammar where
import qualified Data.Map as M
import GF.Grammar.Canonical(QualId(..))
import Utils
import TQ

unqualifyGrammar g = mapQ unqual g
  where
    unqual (Unqual n) = Unqual n
    unqual q@(Qual m n) = if M.lookup n qs==Just [Just m]
                          then Unqual n
                          else q

    qs = fmap nub' $ M.fromListWith (++) [qual q|q<-nub' (listQ g)]

    qual (Qual m n) = (n,[Just m])
    qual (Unqual n) = (n,[Nothing])
