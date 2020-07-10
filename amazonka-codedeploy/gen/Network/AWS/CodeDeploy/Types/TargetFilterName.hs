{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetFilterName
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeDeploy.Types.TargetFilterName (
  TargetFilterName (
    ..
    , ServerInstanceLabel
    , TargetStatus
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data TargetFilterName = TargetFilterName' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern ServerInstanceLabel :: TargetFilterName
pattern ServerInstanceLabel = TargetFilterName' "ServerInstanceLabel"

pattern TargetStatus :: TargetFilterName
pattern TargetStatus = TargetFilterName' "TargetStatus"

{-# COMPLETE
  ServerInstanceLabel,
  TargetStatus,
  TargetFilterName' #-}

instance FromText TargetFilterName where
    parser = (TargetFilterName' . mk) <$> takeText

instance ToText TargetFilterName where
    toText (TargetFilterName' ci) = original ci

-- | Represents an enum of /known/ $TargetFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TargetFilterName where
    toEnum i = case i of
        0 -> ServerInstanceLabel
        1 -> TargetStatus
        _ -> (error . showText) $ "Unknown index for TargetFilterName: " <> toText i
    fromEnum x = case x of
        ServerInstanceLabel -> 0
        TargetStatus -> 1
        TargetFilterName' name -> (error . showText) $ "Unknown TargetFilterName: " <> original name

-- | Represents the bounds of /known/ $TargetFilterName.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TargetFilterName where
    minBound = ServerInstanceLabel
    maxBound = TargetStatus

instance Hashable     TargetFilterName
instance NFData       TargetFilterName
instance ToByteString TargetFilterName
instance ToQuery      TargetFilterName
instance ToHeader     TargetFilterName

instance ToJSON TargetFilterName where
    toJSON = toJSONText
