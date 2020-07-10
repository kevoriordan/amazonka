{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.TextTransformation
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types.TextTransformation (
  TextTransformation (
    ..
    , CmdLine
    , CompressWhiteSpace
    , HTMLEntityDecode
    , Lowercase
    , None
    , URLDecode
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TextTransformation = TextTransformation' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern CmdLine :: TextTransformation
pattern CmdLine = TextTransformation' "CMD_LINE"

pattern CompressWhiteSpace :: TextTransformation
pattern CompressWhiteSpace = TextTransformation' "COMPRESS_WHITE_SPACE"

pattern HTMLEntityDecode :: TextTransformation
pattern HTMLEntityDecode = TextTransformation' "HTML_ENTITY_DECODE"

pattern Lowercase :: TextTransformation
pattern Lowercase = TextTransformation' "LOWERCASE"

pattern None :: TextTransformation
pattern None = TextTransformation' "NONE"

pattern URLDecode :: TextTransformation
pattern URLDecode = TextTransformation' "URL_DECODE"

{-# COMPLETE
  CmdLine,
  CompressWhiteSpace,
  HTMLEntityDecode,
  Lowercase,
  None,
  URLDecode,
  TextTransformation' #-}

instance FromText TextTransformation where
    parser = (TextTransformation' . mk) <$> takeText

instance ToText TextTransformation where
    toText (TextTransformation' ci) = original ci

-- | Represents an enum of /known/ $TextTransformation.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TextTransformation where
    toEnum i = case i of
        0 -> CmdLine
        1 -> CompressWhiteSpace
        2 -> HTMLEntityDecode
        3 -> Lowercase
        4 -> None
        5 -> URLDecode
        _ -> (error . showText) $ "Unknown index for TextTransformation: " <> toText i
    fromEnum x = case x of
        CmdLine -> 0
        CompressWhiteSpace -> 1
        HTMLEntityDecode -> 2
        Lowercase -> 3
        None -> 4
        URLDecode -> 5
        TextTransformation' name -> (error . showText) $ "Unknown TextTransformation: " <> original name

-- | Represents the bounds of /known/ $TextTransformation.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TextTransformation where
    minBound = CmdLine
    maxBound = URLDecode

instance Hashable     TextTransformation
instance NFData       TextTransformation
instance ToByteString TextTransformation
instance ToQuery      TextTransformation
instance ToHeader     TextTransformation

instance ToJSON TextTransformation where
    toJSON = toJSONText

instance FromJSON TextTransformation where
    parseJSON = parseJSONText "TextTransformation"
