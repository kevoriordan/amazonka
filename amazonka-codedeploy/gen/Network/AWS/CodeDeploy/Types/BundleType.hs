{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.BundleType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.BundleType (
  BundleType (
    ..
    , JSON
    , TAR
    , TGZ
    , Yaml
    , Zip
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data BundleType = BundleType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern JSON :: BundleType
pattern JSON = BundleType' "JSON"

pattern TAR :: BundleType
pattern TAR = BundleType' "tar"

pattern TGZ :: BundleType
pattern TGZ = BundleType' "tgz"

pattern Yaml :: BundleType
pattern Yaml = BundleType' "YAML"

pattern Zip :: BundleType
pattern Zip = BundleType' "zip"

{-# COMPLETE
  JSON,
  TAR,
  TGZ,
  Yaml,
  Zip,
  BundleType' #-}

instance FromText BundleType where
    parser = (BundleType' . mk) <$> takeText

instance ToText BundleType where
    toText (BundleType' ci) = original ci

-- | Represents an enum of /known/ $BundleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum BundleType where
    toEnum i = case i of
        0 -> JSON
        1 -> TAR
        2 -> TGZ
        3 -> Yaml
        4 -> Zip
        _ -> (error . showText) $ "Unknown index for BundleType: " <> toText i
    fromEnum x = case x of
        JSON -> 0
        TAR -> 1
        TGZ -> 2
        Yaml -> 3
        Zip -> 4
        BundleType' name -> (error . showText) $ "Unknown BundleType: " <> original name

-- | Represents the bounds of /known/ $BundleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded BundleType where
    minBound = JSON
    maxBound = Zip

instance Hashable     BundleType
instance NFData       BundleType
instance ToByteString BundleType
instance ToQuery      BundleType
instance ToHeader     BundleType

instance ToJSON BundleType where
    toJSON = toJSONText

instance FromJSON BundleType where
    parseJSON = parseJSONText "BundleType"
