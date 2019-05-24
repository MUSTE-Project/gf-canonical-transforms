module RenameGrammar where
import Data.List(stripPrefix)
import GF.Grammar.Canonical

renameGrammar newAbs (Grammar abs cncs) =
    Grammar (renameAbs abs) (map renameCnc cncs)
  where
    renameAbs (Abstract _ fls cs fs) = Abstract (ModId newAbs) fls cs fs

    renameCnc (Concrete (ModId oldCnc) (ModId oldAbs) fls ps lcs ls) =
        Concrete (ModId newCnc) (ModId newAbs) fls ps lcs ls
      where
        newCnc = maybe oldCnc (newAbs++) (stripPrefix oldAbs oldCnc)

