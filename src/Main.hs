{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- snowdrift-api - Server for Snowdrift API.
-- Copyright (C) 2015 Snowdrift.coop
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Affero General Public License for more details.
-- 
-- You should have received a copy of the GNU Affero General Public
-- License along with this program.  If not, see
-- <http://www.gnu.org/licenses/>.

-- | 
-- Module      : Main
-- Description : Runs the server software
-- Copyright   : Copyright (C) 2015 Snowdrift.coop
-- License     : AGPL-3
-- Maintainer  : peter@harpending.org
-- Stability   : experimental
-- Portability : POSIX

module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.Combinators as CC
import qualified Data.HashMap.Lazy as H
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Conduit
import Network.Wai.Handler.Warp
import Paths_snowdrift_api
import Safe
import qualified System.IO as IO
import System.Pager
import System.Posix.ByteString hiding (version)
import Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as M
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8
import Text.Markdown

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif

-- |Process some 
main :: IO ()
main =
  do cmdargs <- fmap (fmap TE.decodeUtf8) getArgs
     if |  or [elem "-h" cmdargs,elem "--help" cmdargs] -> printOrPage helpPage
        |  elem "--version" cmdargs ->
          putStrLn (showVersion version)
        |  elem "--license" cmdargs ->
          getDataFileName "LICENSE" >>=
          TIO.readFile >>=
          printOrPage
        |  otherwise ->
          runArgs (Args (case findIndex (\x ->
                                           or [(x == "--port"),(x == "-p")])
                                        cmdargs of
                           Nothing -> 8778
                           Just x ->
                             read (T.unpack (at cmdargs (x + 1))))
                        (or (fmap (`elem` cmdargs) ["--quiet","-q"])))

-- |The help page
helpPage :: Text
helpPage =
  T.unlines [(mappend "snowdrift-api v." (T.pack (showVersion version)))
            ,"Written by Peter Harpending."
            ,"Copyright (c) 2015 Snowdrift.coop. See `snowdrift-api --license` for the full license."
            ,mempty
            ,"OPTIONS"
            ,T.stripEnd
               (T.unlines (fmap (mappend (T.replicate 4 " "))
                                ["-h,--help                         Show this page."
                                ,"--license                         Print out the license (AGPLv3+)."
                                ,"--version                         Print out the version."
                                ,"-p,--port PORT                    Port on which to run the server (default: 8778)."
                                ,"-q,--quiet                        Don't output anything to stdout."]))]

-- |After taking care of the trivial stuff, the arguments are marshaled
-- into this type.
data Args =
  Args {port :: Int, quiet :: Bool}

-- |Take an 'Args' and run with it
runArgs :: Args -> IO ()
runArgs (Args port_ quiet_) =
  do unless quiet_
            (TIO.putStrLn (mappend "Starting up snowdrift-api on port " (T.pack (show port_))))
     run port_ app

-- |At the moment, this just sends back the request body
app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived 
app request responseHandler =
  do case parseMethod (requestMethod request) of
       Left bs ->
         responseHandler
           (responseLBS status500
                        mempty
                        (BL.fromStrict bs))
       Right req ->
         case req of
           GET ->
             do apiGuideFile <-
                  getDataFileName "api-guide.md"
                apiGuideText <-
                  runResourceT
                    (connect (CC.sourceFile apiGuideFile)
                             (CC.sinkLazy))
                let apiGuideHtml =
                      mkPageHtml (markdown def apiGuideText)
                responseHandler
                  (responseLBS status200
                               mempty
                               (renderHtml apiGuideHtml))
           POST ->
             runResourceT
               (do eitherJSONValue <-
                     connect (sourceRequestBody request)
                             (sinkParserEither json)
                   let response_ =
                         case eitherJSONValue of
                           Left parseError ->
                             responseLBS status500
                                         mempty
                                         (BLC.pack (show parseError))
                           Right jsonValue ->
                             responseLBS status200
                                         mempty
                                         (encode jsonValue)
                   lift (responseHandler response_))

mkPageHtml :: Html -> Html
mkPageHtml h =
  M.html (do M.docType
             M.head (M.title (M.toHtml (mappend "Snowdrift API v." (showVersion version))))
             M.body h)
