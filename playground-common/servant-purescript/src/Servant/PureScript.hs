{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.PureScript
  ( HasBridge,
    DefaultBridge,
    Settings (..),
    addGlobalHeader,
    addGlobalQueryParam,
    addSwitch,
    addTypes,
    apiModuleName,
    defaultBridge,
    defaultBridgeProxy,
    defaultSettings,
    generate,
    generateWithSettings,
    globalHeaders,
    globalQueryParams,
    jsonParseHeader,
    jsonParseUrlPiece,
    jsonToHeader,
    jsonToUrlPiece,
    languageBridge,
    psBridgeSwitches,
    psTypes,
    standardImports,
    toHeader,
    toPathSegment,
    toQueryValue,
  )
where

import Control.Lens
import Control.Monad (guard)
import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BS
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Language.PureScript.Bridge hiding (psTypes)
import Servant.Foreign hiding (Normal, toHeader)
import Servant.PureScript.CodeGen
import Servant.PureScript.Internal
import System.Directory
import System.FilePath
import System.IO (IOMode (..), withFile)
import Text.PrettyPrint.Mainland (hPutDocLn)

-- | Standard entry point - just create a purescript module with default settings
--   for accessing the servant API.
generate ::
  forall bridgeSelector api.
  ( HasForeign (PureScript bridgeSelector) PSType api,
    GenerateList PSType (Foreign PSType api),
    HasBridge bridgeSelector
  ) =>
  FilePath ->
  Proxy bridgeSelector ->
  Proxy api ->
  IO ()
generate = generateWithSettings defaultSettings

generateWithSettings ::
  forall bridgeSelector api.
  ( HasForeign (PureScript bridgeSelector) PSType api,
    GenerateList PSType (Foreign PSType api),
    HasBridge bridgeSelector
  ) =>
  Settings ->
  FilePath ->
  Proxy bridgeSelector ->
  Proxy api ->
  IO ()
generateWithSettings opts@Settings {..} root pBr pAPI = do
  T.putStrLn "\nCreating your PureScript Types..."
  writePSTypesWith _psBridgeSwitches root bridge
    $ interceptType . getSumTypeByTypeInfo
    <$> Set.toList _psTypes
  T.putStrLn "\nSuccessfully created your PureScript types!"
  T.putStrLn "\nCreating your API client module..."
  writeModule _apiModuleName
  T.putStrLn "\nSuccessfully created your client module!"
  T.putStrLn "Please make sure you have purescript-servant-support and purescript-bridge-json-helpers installed\n"
  where
    bridge = languageBridge pBr
    interceptType = interceptHeader . interceptQueryParam . interceptPathSegment
    interceptHeader sumType
      | Set.member (sumType ^. sumTypeInfo . to bridge) headerTypes = toHeader sumType
      | otherwise = sumType
    interceptQueryParam sumType
      | Set.member (sumType ^. sumTypeInfo . to bridge) queryTypes = toQueryValue sumType
      | otherwise = sumType
    interceptPathSegment sumType
      | Set.member (sumType ^. sumTypeInfo . to bridge) pathTypes = toPathSegment sumType
      | otherwise = sumType

    apiList = apiToList pAPI pBr
    flatArgTypes = argType . to flattenTypeInfo . traversed
    headerTypes = Set.fromList $ apiList ^.. traversed . reqHeaders . traversed . headerArg . flatArgTypes
    queryTypes = Set.fromList $ apiList ^.. traversed . reqUrl . queryStr . traversed . queryArgName . flatArgTypes
    pathTypes = Set.fromList $ apiList ^.. traversed . reqUrl . path . traversed . to unSegment . _Cap . flatArgTypes

    writeModule :: Text -> IO ()
    writeModule mName =
      let baseFileName = root </> joinPath (map T.unpack $ T.splitOn "." mName)
          pursModuleFile = baseFileName <> ".purs"
          pursModulePath = pursModuleFile
          mDir = takeDirectory baseFileName
          contents = genModule opts apiList
       in do
            unlessM (doesDirectoryExist mDir) $ createDirectoryIfMissing True mDir
            withFile pursModulePath WriteMode $ flip hPutDocLn contents

-- | Use this function for implementing 'parseUrlPiece' in your FromHttpApiData instances
--   in order to be compatible with the generated PS code.
--
-- >
-- > instance ToHttpApiData MyDataType where
-- >   toUrlPiece = jsonToUrlPiece
-- >   toHeader   = jsonToHeader
-- >
-- > instance FromHttpApiData MyDataType where
-- >   parseUrlPiece = jsonParseUrlPiece
-- >   parseHeader   = jsonParseHeader
-- >
jsonParseUrlPiece :: FromJSON a => Text -> Either Text a
jsonParseUrlPiece = jsonParseHeader . T.encodeUtf8

-- | Use this function for implementing 'toUrlPiece' in your ToHttpApiData instances
--   in order to be compatible with the generated PS code.
jsonToUrlPiece :: ToJSON a => a -> Text
jsonToUrlPiece = T.decodeUtf8 . jsonToHeader

-- | Use this function for implementing 'parseHeader' in your FromHttpApiData instances
--   in order to be compatible with the generated PS code.
jsonParseHeader :: FromJSON a => ByteString -> Either Text a
jsonParseHeader = first T.pack . eitherDecodeStrict

-- | Use this function for implementing 'toHeader' in your ToHttpApiData instances
--   in order to be compatible with the generated PS code.
jsonToHeader :: ToJSON a => a -> ByteString
jsonToHeader = BS.toStrict . encode

toHeader :: SumType lang -> SumType lang
toHeader = mkToURIData "Header"

toQueryValue :: SumType lang -> SumType lang
toQueryValue = mkToURIData "QueryValue"

toPathSegment :: SumType lang -> SumType lang
toPathSegment = mkToURIData "PathSegment"

mkToURIData :: Text -> SumType lang -> SumType lang
mkToURIData name (SumType t cs is) = SumType t cs $ mkInstance : is
  where
    mkInstance = Custom $ CustomInstance mkConstraints mkHead mkImplementation
    mkConstraints = []
    mkHead = TypeInfo "purescript-servant-support" "Servant.PureScript" ("To" <> name) [t]
    mkImplementation = fromMaybe useShow $
      (guard canDeriveNewtype $> DeriveNewtype)
        <|> (guard canUseEncodeJson $> useEncodeJson)
    canDeriveNewtype = Newtype `elem` is && isn'tRecord cs
    isn'tRecord [DataConstructor _ (Normal (ty :| []))] = ty ^. typeName . to isKnownCompatibleType
    isn'tRecord _                                       = False
    canUseEncodeJson = Json `elem` is
    isKnownCompatibleType "String" = True
    isKnownCompatibleType _        = True
    useShow = mkExplicit "show" Map.empty
    useEncodeJson = mkExplicit "encodeJson" $ importsFromList
      [ImportLine "Data.Argonaut" $ Set.fromList ["encodeJson"]
      ,ImportLine "Servant.PureScript" $ Set.fromList [methodName]
      ]
    methodName = "to" <> name
    mkExplicit body importLines =
      Explicit [InstanceMember methodName [] (methodName <> " <<< " <> body) [] importLines]

