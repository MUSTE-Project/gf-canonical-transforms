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

data Args = Args {outdir::FilePath, seps::Separators,
                  eqns::[Eqn], files::[FilePath]}

transform (Args outdir seps eqns files@[_]) =
  do (utc,(cncname,grammar)) <- GF.batchCompile noOptions files
     let absname = GF.srcAbsName grammar cncname
         canon = GF.grammar2canonical noOptions absname grammar
         canon' = conflateParams seps eqns (unqualifyGrammar canon)
     writeGrammar outdir canon'
transform _ = abort usage

parseArgs = loop args0
  where
    args0 = Args {outdir="transformed", seps=defaultSeparators,
                  eqns=[], files=[]}

    loop args []                    = return args
    loop args ["-h"]                = abort usage
    loop args ["-d"]                = abort "Error: missing dirpath after -d"
    loop args ("-d":path:ws)        = loop args{outdir=path} ws
    loop args ["--fn-sep"]          = abort "Error: missing str after --fn-sep"
    loop args ("--fn-sep":str:ws)   = loop args{seps=(seps args){fnSep=str}} ws
    loop args ["--or-sep"]          = abort "Error: missing str after --or-sep"
    loop args ("--or-sep":str:ws)   = loop args{seps=(seps args){orSep=str}} ws
    loop args (w:ws) | '=' `elem` w = loop args{eqns=eqns args++[eqn]} ws
                     | otherwise    = loop args{files=files args++[w]} ws
        where eqn = segments (/='=') w

abort message =
  do hPutStrLn stderr message
     exitFailure

usage = "Usage: gf-conflate-params [options] P=Q[=...] ... ConcreteGrammar.gf"
