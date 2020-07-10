{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ActionType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ActionType (
  ActionType (
    ..
    , ATRecommendNodeConfig
    , ATResizeCluster
    , ATRestoreCluster
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data ActionType = ActionType' (CI Text)
                    deriving (Eq, Ord, Read, Show, Data, Typeable,
                              Generic)

pattern ATRecommendNodeConfig :: ActionType
pattern ATRecommendNodeConfig = ActionType' "recommend-node-config"

pattern ATResizeCluster :: ActionType
pattern ATResizeCluster = ActionType' "resize-cluster"

pattern ATRestoreCluster :: ActionType
pattern ATRestoreCluster = ActionType' "restore-cluster"

{-# COMPLETE
  ATRecommendNodeConfig,
  ATResizeCluster,
  ATRestoreCluster,
  ActionType' #-}

instance FromText ActionType where
    parser = (ActionType' . mk) <$> takeText

instance ToText ActionType where
    toText (ActionType' ci) = original ci

-- | Represents an enum of /known/ $ActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ActionType where
    toEnum i = case i of
        0 -> ATRecommendNodeConfig
        1 -> ATResizeCluster
        2 -> ATRestoreCluster
        _ -> (error . showText) $ "Unknown index for ActionType: " <> toText i
    fromEnum x = case x of
        ATRecommendNodeConfig -> 0
        ATResizeCluster -> 1
        ATRestoreCluster -> 2
        ActionType' name -> (error . showText) $ "Unknown ActionType: " <> original name

-- | Represents the bounds of /known/ $ActionType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ActionType where
    minBound = ATRecommendNodeConfig
    maxBound = ATRestoreCluster

instance Hashable     ActionType
instance NFData       ActionType
instance ToByteString ActionType
instance ToQuery      ActionType
instance ToHeader     ActionType
