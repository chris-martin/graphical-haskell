#!/usr/bin/env stack
-- stack --resolver lts-14.15 runhaskell --package xeno --package temporary --package bytestring

import Control.Exception (throw)
import Data.Foldable (traverse_)
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempFile)
import System.Process (callProcess)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Xeno.DOM as X
import qualified System.IO as IO

main :: IO ()
main =
    getArgs >>= \[svgPath] ->
    BS.readFile svgPath >>= \bs ->
    either throw return (X.parse bs) >>= \svgRoot ->
    withSystemTempFile "graphical-haskell-program-.hs" $ \hsPath hsHandle ->
      (
        traverse_ (writeCode hsHandle) (nodeCodeBlocks svgRoot) >>
        IO.hClose hsHandle >>
        callProcess "cat" [hsPath] >>
        callProcess "runhaskell" [hsPath]
      )

-- Find all SVG text nodes and get the text from each.
nodeCodeBlocks :: X.Node -> [LBS.ByteString]
nodeCodeBlocks node
    | X.name node == BS8.pack "text" = [BSB.toLazyByteString (nodeText node)]
    | otherwise = foldMap nodeCodeBlocks (X.children node)

-- Extract all of the text from a node.
nodeText :: X.Node -> BSB.Builder
nodeText node =
    foldMap contentText (X.contents node) <>
    if nodeIsLine node then BSB.byteString (BS8.pack "\n") else mempty

nodeIsLine :: X.Node -> Bool
nodeIsLine node =
    lookup (BS8.pack "sodipodi:role") (X.attributes node) == Just (BS8.pack "line")

-- Extract all of the text from node content.
contentText :: X.Content -> BSB.Builder
contentText (X.Element node) = nodeText node
contentText (X.Text bs) = BSB.lazyByteString (unescapeEntitiesLBS (LBS.fromStrict bs))
contentText (X.CData bs) = BSB.byteString bs

unescapeEntitiesLBS :: LBS.ByteString -> LBS.ByteString
unescapeEntitiesLBS = LT.encodeUtf8 . unescapeEntitiesLT . LT.decodeUtf8

unescapeEntitiesLT :: LT.Text -> LT.Text
unescapeEntitiesLT = let r a b = LT.replace (LT.pack a) (LT.pack b) in
    r "&amp;" "&" . r "&lt;" "<" . r "&gt;" ">" . r "&quot;" "\"" . r "&apos;" "'"

-- Write a block of code to the handle.
writeCode :: IO.Handle -> LBS.ByteString -> IO ()
writeCode h code = LBS.hPut h code >> IO.hPutStrLn h ""
