import qualified GF
import Data.List(partition)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.IO(hPutStrLn,stderr)
import GF.Support(noOptions)
import Utils(segments)
import ConflateParams
import UnqualifyGrammar
import WriteGrammar

main = transform =<< parseArgs =<< getArgs

data Args = Args {outdir::FilePath, eqns::[Eqn], files::[FilePath]}

transform (Args outdir eqns files@[_]) =
  do (utc,(cncname,grammar)) <- GF.batchCompile noOptions files
     let absname = GF.srcAbsName grammar cncname
         canon = GF.grammar2canonical noOptions absname grammar
         canon' = conflateParams eqns (unqualifyGrammar canon)
     writeGrammar outdir canon'
transform _ = abort usage

parseArgs = loop args0
  where
    args0 = Args {outdir="transformed", eqns=[], files=[]}

    loop args []                    = return args
    loop args ["-h"]                = abort usage
    loop args ["-d"]                = abort "Error: missing dirpath after -d"
    loop args ("-d":path:ws)        = loop args{outdir=path} ws
    loop args (w:ws) | '=' `elem` w = loop args{eqns=eqns args++[eqn]} ws
                     | otherwise    = loop args{files=files args++[w]} ws
        where eqn = segments (/='=') w

abort message =
  do hPutStrLn stderr message
     exitFailure

usage = "Usage: gf-conflate-params [-d dirpath] P=Q[=...] ... ConcreteGrammar.gf"
