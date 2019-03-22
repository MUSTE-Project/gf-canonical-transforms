module WriteGrammar where
import System.FilePath((</>),(<.>))
import System.Directory(createDirectoryIfMissing)
import GF.Support(writeUTF8File)
import GF.Text.Pretty(render80)
import GF.Grammar.Canonical(Grammar(..),ModId(..),abstrName,concName)

-- | Write a canonical grammar in GF source files that can be compiled with GF.
-- The 'FilePath' argument is the name of a subdirectory where the files will
-- be written.
writeGrammar prefix (Grammar abs cncs) =
  do createDirectoryIfMissing False prefix
     writeUTF8File (absPath abs) (render80 abs)
     sequence_ [writeUTF8File (cncPath cnc) (render80 cnc)|cnc<-cncs]
  where
    absPath = gfpath . abstrName 
    cncPath = gfpath . concName

    gfpath (ModId s) = prefix</>s<.>"gf"
