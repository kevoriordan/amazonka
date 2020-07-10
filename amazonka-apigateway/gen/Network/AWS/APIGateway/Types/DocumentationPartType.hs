{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DocumentationPartType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.APIGateway.Types.DocumentationPartType (
  DocumentationPartType (
    ..
    , DPTAPI
    , DPTAuthorizer
    , DPTMethod
    , DPTModel
    , DPTPathParameter
    , DPTQueryParameter
    , DPTRequestBody
    , DPTRequestHeader
    , DPTResource
    , DPTResponse
    , DPTResponseBody
    , DPTResponseHeader
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DocumentationPartType = DocumentationPartType' (CI
                                                       Text)
                               deriving (Eq, Ord, Read, Show, Data, Typeable,
                                         Generic)

pattern DPTAPI :: DocumentationPartType
pattern DPTAPI = DocumentationPartType' "API"

pattern DPTAuthorizer :: DocumentationPartType
pattern DPTAuthorizer = DocumentationPartType' "AUTHORIZER"

pattern DPTMethod :: DocumentationPartType
pattern DPTMethod = DocumentationPartType' "METHOD"

pattern DPTModel :: DocumentationPartType
pattern DPTModel = DocumentationPartType' "MODEL"

pattern DPTPathParameter :: DocumentationPartType
pattern DPTPathParameter = DocumentationPartType' "PATH_PARAMETER"

pattern DPTQueryParameter :: DocumentationPartType
pattern DPTQueryParameter = DocumentationPartType' "QUERY_PARAMETER"

pattern DPTRequestBody :: DocumentationPartType
pattern DPTRequestBody = DocumentationPartType' "REQUEST_BODY"

pattern DPTRequestHeader :: DocumentationPartType
pattern DPTRequestHeader = DocumentationPartType' "REQUEST_HEADER"

pattern DPTResource :: DocumentationPartType
pattern DPTResource = DocumentationPartType' "RESOURCE"

pattern DPTResponse :: DocumentationPartType
pattern DPTResponse = DocumentationPartType' "RESPONSE"

pattern DPTResponseBody :: DocumentationPartType
pattern DPTResponseBody = DocumentationPartType' "RESPONSE_BODY"

pattern DPTResponseHeader :: DocumentationPartType
pattern DPTResponseHeader = DocumentationPartType' "RESPONSE_HEADER"

{-# COMPLETE
  DPTAPI,
  DPTAuthorizer,
  DPTMethod,
  DPTModel,
  DPTPathParameter,
  DPTQueryParameter,
  DPTRequestBody,
  DPTRequestHeader,
  DPTResource,
  DPTResponse,
  DPTResponseBody,
  DPTResponseHeader,
  DocumentationPartType' #-}

instance FromText DocumentationPartType where
    parser = (DocumentationPartType' . mk) <$> takeText

instance ToText DocumentationPartType where
    toText (DocumentationPartType' ci) = original ci

-- | Represents an enum of /known/ $DocumentationPartType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DocumentationPartType where
    toEnum i = case i of
        0 -> DPTAPI
        1 -> DPTAuthorizer
        2 -> DPTMethod
        3 -> DPTModel
        4 -> DPTPathParameter
        5 -> DPTQueryParameter
        6 -> DPTRequestBody
        7 -> DPTRequestHeader
        8 -> DPTResource
        9 -> DPTResponse
        10 -> DPTResponseBody
        11 -> DPTResponseHeader
        _ -> (error . showText) $ "Unknown index for DocumentationPartType: " <> toText i
    fromEnum x = case x of
        DPTAPI -> 0
        DPTAuthorizer -> 1
        DPTMethod -> 2
        DPTModel -> 3
        DPTPathParameter -> 4
        DPTQueryParameter -> 5
        DPTRequestBody -> 6
        DPTRequestHeader -> 7
        DPTResource -> 8
        DPTResponse -> 9
        DPTResponseBody -> 10
        DPTResponseHeader -> 11
        DocumentationPartType' name -> (error . showText) $ "Unknown DocumentationPartType: " <> original name

-- | Represents the bounds of /known/ $DocumentationPartType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DocumentationPartType where
    minBound = DPTAPI
    maxBound = DPTResponseHeader

instance Hashable     DocumentationPartType
instance NFData       DocumentationPartType
instance ToByteString DocumentationPartType
instance ToQuery      DocumentationPartType
instance ToHeader     DocumentationPartType

instance ToJSON DocumentationPartType where
    toJSON = toJSONText

instance FromJSON DocumentationPartType where
    parseJSON = parseJSONText "DocumentationPartType"
