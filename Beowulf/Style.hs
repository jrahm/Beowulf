{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Beowulf.Style where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Text.Printf
import Beowulf.Html
import Control.Monad.Writer.Lazy
import qualified Data.Foldable as Fold
import qualified Data.Map as Map

data ColorT = RGB Int Int Int | Hex Int | RGBA Int Int Int Float
data Link = URL String

instance HasRaw Link where
    raw = URL

data Unit = Px | Percent | Em
serializeUnit :: Unit -> ByteString
serializeUnit Px = "px"
serializeUnit Percent = "%"
serializeUnit Em = "em"

data Alignment = AlignRight | AlignLeft | AlignCenter
serialAlignment :: Alignment -> ByteString
serialAlignment AlignRight = "right"
serialAlignment AlignLeft = "left"
serialAlignment AlignCenter = "center"

data Style = Style StyleRoot
data StyleRoot = StyleRoot [StyleNode]
data StyleNode =
    RawStyleNode ByteString |
    Color ColorT            |
    BackgroundColor ColorT  |
    BackgroundImage Link    |
    Width Int Unit          |
    Height Int Unit         |
    TextAlign Alignment     

newStyle :: Style
newStyle = Style (StyleRoot [])

serialColor :: ColorT -> ByteString
serialColor (RGB r g b) = BC.pack $ printf "rgb(%d,%d,%d)" r g b
serialColor (Hex i) = BC.pack $ printf "#%06x" i
serialColor (RGBA r g b a') = BC.pack $ printf "rgba(%d,%d,%d,%f)" r g b a'

serialLink :: Link -> ByteString
serialLink (URL s) = BC.pack $ printf "url(\"%s\")" s

serializeStyle :: Style -> ByteString
serializeStyle (Style (StyleRoot nodes)) =
    snd . runWriter $ mapM_ (tell . flip BS.append (BC.singleton ';') . serialNode) (reverse nodes)
    where serialNode (RawStyleNode bs) = bs
          serialNode (Color c) = BC.append "color: " $ serialColor c
          serialNode (BackgroundColor c) = BC.append "background-color: " $ serialColor c
          serialNode (BackgroundImage link) = BC.append "background-image: " $ serialLink link
          serialNode (Width i w) =  Fold.fold ["width: ", BC.pack $ show i, serializeUnit w]
          serialNode (Height i w) =  Fold.fold ["height: ", BC.pack $ show i, serializeUnit w]
          serialNode (TextAlign a) =  Fold.fold ["text-align: ", serialAlignment a]

-- *->
data Rule = IfId String | IfClass String | IfTag String | ConsRule Rule Rule | EmptyRule deriving (Eq, Ord)
data StyleSheet = StyleSheet (Map.Map Rule Style)

serializeRule :: Rule -> ByteString
serializeRule (IfId s) = BS.append "#" $ BC.pack s
serializeRule (IfClass s) = BS.append "." $ BC.pack s
serializeRule (IfTag s) = BC.pack s
serializeRule (ConsRule r1 r2) = Fold.fold [serializeRule r1, " ", serializeRule r2]
serializeRule EmptyRule = ""

serializeStyleSheet :: StyleSheet -> ByteString
serializeStyleSheet (StyleSheet m) =
    Fold.fold $
        flip map (Map.toList m) $ \(rule, style') -> 
            Fold.fold [serializeRule rule, "{", serializeStyle style', "}\n"]

emptyStyleSheet :: StyleSheet
emptyStyleSheet = StyleSheet Map.empty

data StyleRule = StyleRule Rule Style

(~:) :: Rule -> Style -> StyleRule
(~:) = StyleRule
infixl 6 ~:

instance Attachable Rule Rule where
    attach = ConsRule
    empty = EmptyRule

instance HasChild StyleSheet StyleRule where
    attachChild (StyleSheet m) (StyleRule r s) = StyleSheet (Map.insert r s m)

instance Attachable Style StyleNode where
    attach (Style (StyleRoot lst)) nod = Style (StyleRoot (nod:lst))
    empty = newStyle

instance Attachable Style Style where
    attach (Style (StyleRoot lst1)) (Style (StyleRoot lst2)) = Style (StyleRoot (lst2 ++ lst1))
    empty = newStyle

instance HasRaw StyleNode where
    rawBS = RawStyleNode

instance HasRaw StyleRoot where
    rawBS = StyleRoot . return . RawStyleNode

instance IsAttr Style where
    attrName _ = BC.pack "style"
    attrVal = serializeStyle

style :: StyleSheet -> DocumentObject
style ss = DocumentObject (BC.pack "style") Map.empty [Left $ serializeStyleSheet ss]

foldStyle :: [StyleNode] -> Style
foldStyle = foldlA newStyle

