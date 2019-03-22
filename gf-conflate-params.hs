import qualified GF
import Data.List(partition)
import System.Environment(getArgs)
import GF.Support(noOptions)
import Utils(segments)
import ConflateParams
import UnqualifyGrammar
import WriteGrammar

main = transform . parseArgs =<< getArgs

parseArgs args = (eqns,files)
  where
    eqns = map (segments (/='=')) eqs
    (eqs,files) = partition ('=' `elem`) args

transform (eqns,files) =
  do (utc,(cncname,grammar)) <- GF.batchCompile noOptions files
     let absname = GF.srcAbsName grammar cncname
         canon = GF.grammar2canonical noOptions absname grammar
         canon' = conflateParams eqns (unqualifyGrammar canon)
     writeGrammar "transformed" canon'
