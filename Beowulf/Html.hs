{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Beowulf.Html where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

import Data.Char

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Writer.Lazy
import Text.Printf

class IsLink a where
    toLink :: a -> String
    toLink = BC.unpack . toLinkBS

    toLinkBS :: a -> ByteString
    toLinkBS = BC.pack . toLink

class IsHtml a where
    toHtml :: a -> ByteString

class IsAttr a where
    attrName :: a -> ByteString
    attrVal :: a -> ByteString

data DocumentObject = DocumentObject {
    tag :: ByteString,
    attributes :: Map ByteString ByteString,
    children :: [Either ByteString DocumentObject]
}

emptyDocumentObject :: String -> DocumentObject
emptyDocumentObject tag' = DocumentObject (BC.pack tag') Map.empty []

paragraph :: HasChild DocumentObject a => a -> DocumentObject
paragraph p'  = DocumentObject (BC.pack "p") Map.empty [] *-> p'

article :: HasChild DocumentObject a => a -> DocumentObject
article a' = DocumentObject (BC.pack "a") Map.empty [] *-> a'

heading :: HasChild DocumentObject a => Int -> a -> DocumentObject
heading n s = DocumentObject (BC.pack $ "h"++show n) Map.empty [] *-> s

h1 :: HasChild DocumentObject a => a -> DocumentObject
h1 = heading 1

h2 :: HasChild DocumentObject a => a -> DocumentObject
h2 = heading 2

h3 :: HasChild DocumentObject a => a -> DocumentObject
h3 = heading 3

h4 :: HasChild DocumentObject a => a -> DocumentObject
h4 = heading 4

h5 :: HasChild DocumentObject a => a -> DocumentObject
h5 = heading 5

h6 :: HasChild DocumentObject a => a -> DocumentObject
h6 = heading 6

a :: HasChild DocumentObject a => a -> DocumentObject
a = article

p :: HasChild DocumentObject a => a -> DocumentObject
p = paragraph

title :: HasChild DocumentObject a => a -> DocumentObject
title = attachChild (DocumentObject (BC.pack "title") Map.empty [])

div :: DocumentObject
div = emptyDocumentObject "div"

panel :: DocumentObject
panel = Beowulf.Html.div

html :: DocumentObject
html = emptyDocumentObject "html"

head :: DocumentObject
head = emptyDocumentObject "head"

hr :: DocumentObject
hr = emptyDocumentObject "hr"

body :: DocumentObject -> DocumentObject
body d = emptyDocumentObject "body" *-> d

data Class = Class String
data RawAttr = RawAttr String String
data Id = Id String

instance IsAttr Id where
    attrName _ = BC.pack "id"
    attrVal (Id s) = BC.pack s

instance IsAttr RawAttr where
    attrName (RawAttr n _) = BC.pack n
    attrVal (RawAttr _ v) = BC.pack v

instance IsAttr Class where
    attrName _ = BC.pack "class"
    attrVal (Class bs) = BC.pack bs

attachAttr :: (IsAttr a) => DocumentObject -> a -> DocumentObject
attachAttr (DocumentObject tag' attrs children') attr =
    DocumentObject tag' (Map.insert (attrName attr) (attrVal attr) attrs) children'

-- (#) :: (IsAttr a) => DocumentObject -> a -> DocumentObject
-- (#) = attachAttr

appendAttr :: (IsAttr a) => DocumentObject -> a -> DocumentObject
appendAttr (DocumentObject tag' attrs children') attr =
    DocumentObject tag' (Map.insertWith (flip BS.append . BS.append (BC.singleton ' ')) (attrName attr) (attrVal attr) attrs) children'

(+#) :: (IsAttr a) => DocumentObject -> a -> DocumentObject
(+#) = appendAttr


class Attachable a b where
    attach :: a -> b -> a
    empty :: a

class HasChild a b where
    attachChild :: a -> b -> a

(#) :: (Attachable a b) => a -> b -> a
(#) = attach
(#$) :: (Attachable a b) => a -> b -> a
(#$) = attach

instance (IsAttr a) => Attachable DocumentObject a where
    attach = attachAttr
    empty = emptyDocumentObject ""

compileDOM :: DocumentObject -> ByteString
compileDOM = compile . Right

compile :: Either ByteString DocumentObject -> ByteString
compile = either id $ \(DocumentObject tag' attrs children') ->
    let serializeAttrs :: Map ByteString ByteString -> ByteString
        serializeAttrs attrs' = snd $ runWriter $
            forM_ (Map.toList attrs') $ \(key, val) ->
                mapM_ tell [bb " ", key, bb "=\"", val, bb "\""]
        compile' :: Writer ByteString ()
        compile' = do
            mapM_ tell [bb "<", tag', serializeAttrs attrs, bb ">"]
            mapM_ (tell.compile) (Prelude.reverse children')
            mapM_ tell [bb "</", tag', bb ">"] in snd $ runWriter compile'

    where bb = BC.pack

data Raw = Raw ByteString
data Escape = Escape ByteString

instance IsLink Raw where
    toLinkBS (Raw bs) = bs

class HasRaw a where
    raw :: String -> a
    raw = rawBS . BC.pack
    rawBS :: ByteString -> a
    rawBS = raw . BC.unpack

instance HasRaw Raw where
    rawBS = Raw

esc :: String -> Escape
esc = Escape. BC.pack

renderHTML :: DocumentObject -> DocumentObject -> ByteString
renderHTML h d = BS.append (BC.pack "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\">\n") $ compileDOM (html *-> h *-> body d) 

instance HasChild DocumentObject Escape where
    attachChild obj (Escape s) = attachChild obj $ Raw $ domEscape s
        where domEscape = BC.concatMap $ \ch ->
                case ch of
                    '&' -> BC.pack "&amp;"
                    '<' -> BC.pack "&lt;"
                    '>' -> BC.pack "&gt;"
                    '"' -> BC.pack "&quot;"
                    _ -> if isAscii ch then BC.singleton ch else
                            BC.pack $ printf "&#x%04x;" (ord ch)

instance HasChild DocumentObject DocumentObject where
    attachChild (DocumentObject a' b clds) ch = DocumentObject a' b (Right ch:clds)

instance HasChild DocumentObject ByteString where
    attachChild (DocumentObject a' b clds) ch = DocumentObject a' b (Left ch:clds)

instance HasChild DocumentObject Raw where
    attachChild (DocumentObject a' b clds) (Raw ch) = DocumentObject a' b (Left ch:clds)

foldlA :: (Attachable a b) => a -> [b] -> a 
foldlA = foldl (#)

(*->) :: (HasChild a b) => a -> b -> a
(*->) = attachChild
(**->) :: (HasChild a b) => a -> b -> a
(**->) = attachChild
(***->) :: (HasChild a b) => a -> b -> a
(***->) = attachChild
(****->) :: (HasChild a b) => a -> b -> a
(****->) = attachChild

(*->$) :: (HasChild a b) => a -> b -> a
(*->$) = attachChild

(!) :: DocumentObject -> String -> DocumentObject
(!) d s = d +# Class s

infixl 1 *->$
infixl 5 *->
infixl 6 **->
infixl 7 ***->
infixl 8 ****->

infixl 8 #
infixl 7 #$
