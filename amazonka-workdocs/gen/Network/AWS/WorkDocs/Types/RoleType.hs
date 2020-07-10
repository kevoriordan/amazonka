{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.RoleType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.RoleType (
  RoleType (
    ..
    , Contributor
    , Coowner
    , Owner
    , Viewer
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RoleType = RoleType' (CI Text)
                  deriving (Eq, Ord, Read, Show, Data, Typeable,
                            Generic)

pattern Contributor :: RoleType
pattern Contributor = RoleType' "CONTRIBUTOR"

pattern Coowner :: RoleType
pattern Coowner = RoleType' "COOWNER"

pattern Owner :: RoleType
pattern Owner = RoleType' "OWNER"

pattern Viewer :: RoleType
pattern Viewer = RoleType' "VIEWER"

{-# COMPLETE
  Contributor,
  Coowner,
  Owner,
  Viewer,
  RoleType' #-}

instance FromText RoleType where
    parser = (RoleType' . mk) <$> takeText

instance ToText RoleType where
    toText (RoleType' ci) = original ci

-- | Represents an enum of /known/ $RoleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RoleType where
    toEnum i = case i of
        0 -> Contributor
        1 -> Coowner
        2 -> Owner
        3 -> Viewer
        _ -> (error . showText) $ "Unknown index for RoleType: " <> toText i
    fromEnum x = case x of
        Contributor -> 0
        Coowner -> 1
        Owner -> 2
        Viewer -> 3
        RoleType' name -> (error . showText) $ "Unknown RoleType: " <> original name

-- | Represents the bounds of /known/ $RoleType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RoleType where
    minBound = Contributor
    maxBound = Viewer

instance Hashable     RoleType
instance NFData       RoleType
instance ToByteString RoleType
instance ToQuery      RoleType
instance ToHeader     RoleType

instance ToJSON RoleType where
    toJSON = toJSONText

instance FromJSON RoleType where
    parseJSON = parseJSONText "RoleType"
