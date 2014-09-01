{-# LANGUAGE OverloadedStrings #-}

module Beowulf.Style.Ext where

import Beowulf.Style
import qualified Data.ByteString.Char8 as BC

import qualified Data.Foldable as Fold
import Text.Printf
import Data.ByteString (ByteString)
import Data.Char

quickRaw :: ByteString -> ByteString -> ByteString
quickRaw a1 a2 = Fold.fold [a1, ":", a2]

width :: Int -> Unit -> StyleNode
width = Width

height :: Int -> Unit -> StyleNode
height = Height

textAlign :: Alignment -> StyleNode
textAlign = TextAlign

backgroundColor :: ColorT -> StyleNode
backgroundColor = BackgroundColor

backgroundImage :: Link -> StyleNode
backgroundImage = BackgroundImage

boxShadow :: Int -> Int -> Int -> ColorT -> StyleNode
boxShadow offx offy spread color' = RawStyleNode $
            Fold.fold
                [ BC.pack (printf "box-shadow: %dpx %dpx %dpx " offx offy spread) `BC.append` serialColor color', ";",
                  BC.pack (printf "-webkit-box-shadow: %dpx %dpx %dpx " offx offy spread) `BC.append` serialColor color' ]

data Position = Fixed | Relative | Absolute deriving Show
position :: Position -> StyleNode
position pos = RawStyleNode $
    quickRaw "position" (BC.pack $ map toLower $ show pos)

zIndex :: Int -> StyleNode
zIndex i = RawStyleNode $
    quickRaw "z-index" (BC.pack $ show i)

data Direction = Top | Bottom | Left | Right | All
serializeDirection :: Direction -> ByteString
serializeDirection Top = "-top"
serializeDirection Bottom = "-bottom"
serializeDirection Beowulf.Style.Ext.Left = "-left"
serializeDirection Beowulf.Style.Ext.Right = "-right"
serializeDirection All = ""

auto :: Maybe a
auto = Nothing

margin :: Direction -> Maybe (Int, Unit) -> StyleNode 
margin dir val = RawStyleNode $ Fold.fold $
    [ "margin", serializeDirection dir, ": " ] ++
        maybe ["auto"] (\(i,u) -> [BC.pack $ show i, serializeUnit u]) val

padding :: Direction -> (Int, Unit) -> StyleNode 
padding dir (i,u) = RawStyleNode $ Fold.fold $
    [ "padding", serializeDirection dir, ": ", BC.pack $ show i, serializeUnit u]

color :: ColorT -> StyleNode
color col = RawStyleNode $ BC.append "color: " $ serialColor col
