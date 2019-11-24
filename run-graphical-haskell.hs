#!/usr/bin/env stack
-- stack --resolver lts-14.15 runhaskell --package xeno --package temporary --package bytestring --package text --package process

import Control.Exception (throw)
import Data.Foldable (traverse_)
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempFile)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Char as C
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Xeno.DOM as X
import qualified System.IO as IO
import qualified System.Process as Proc

main :: IO ()
main =
    getArgs >>= \(svgPath : svgArgs) ->
    BS.readFile svgPath >>= \bs ->
    either throw return (X.parse bs) >>= \svgRoot ->
    withSystemTempFile "pragmas-.hs" $ \pragmasPath pragmasHandle ->
    withSystemTempFile "imports-.hs" $ \importsPath importsHandle ->
    withSystemTempFile "declarations-.hs" $ \declsPath declsHandle ->
    withSystemTempFile "graphical-haskell-.hs" $ \hsPath hsHandle ->
      (
        IO.hClose hsHandle >>
        traverse_ (writeCode pragmasHandle importsHandle declsHandle) (nodeCodeBlocks svgRoot) >>
        IO.hClose pragmasHandle >>
        IO.hClose importsHandle >>
        IO.hClose declsHandle >>
        Proc.callCommand (unwords ["cat", importsPath, declsPath, ">", hsPath]) >>
        Proc.callProcess "runhaskell" (hsPath : svgArgs)
      )

-- Write a block of code to the appropriate handle.
writeCode :: IO.Handle -> IO.Handle -> IO.Handle -> LBS.ByteString -> IO ()
writeCode pragmasHandle importsHandle declsHandle code =
    let h = if LBS.isPrefixOf (LBS8.pack "{-# ") code && LBS.isPrefixOf (LBS8.pack "language ") (LBS8.map C.toLower code)
            then pragmasHandle else if LBS.isPrefixOf (LBS8.pack "import ") code
            then importsHandle else declsHandle
    in LBS.hPut h code >> IO.hPutStrLn h ""

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
