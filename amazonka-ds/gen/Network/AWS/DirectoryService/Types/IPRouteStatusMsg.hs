{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.IPRouteStatusMsg
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.IPRouteStatusMsg (
  IPRouteStatusMsg (
    ..
    , AddFailed
    , Added
    , Adding
    , RemoveFailed
    , Removed
    , Removing
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data IPRouteStatusMsg = IPRouteStatusMsg' (CI Text)
                          deriving (Eq, Ord, Read, Show, Data, Typeable,
                                    Generic)

pattern AddFailed :: IPRouteStatusMsg
pattern AddFailed = IPRouteStatusMsg' "AddFailed"

pattern Added :: IPRouteStatusMsg
pattern Added = IPRouteStatusMsg' "Added"

pattern Adding :: IPRouteStatusMsg
pattern Adding = IPRouteStatusMsg' "Adding"

pattern RemoveFailed :: IPRouteStatusMsg
pattern RemoveFailed = IPRouteStatusMsg' "RemoveFailed"

pattern Removed :: IPRouteStatusMsg
pattern Removed = IPRouteStatusMsg' "Removed"

pattern Removing :: IPRouteStatusMsg
pattern Removing = IPRouteStatusMsg' "Removing"

{-# COMPLETE
  AddFailed,
  Added,
  Adding,
  RemoveFailed,
  Removed,
  Removing,
  IPRouteStatusMsg' #-}

instance FromText IPRouteStatusMsg where
    parser = (IPRouteStatusMsg' . mk) <$> takeText

instance ToText IPRouteStatusMsg where
    toText (IPRouteStatusMsg' ci) = original ci

-- | Represents an enum of /known/ $IPRouteStatusMsg.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum IPRouteStatusMsg where
    toEnum i = case i of
        0 -> AddFailed
        1 -> Added
        2 -> Adding
        3 -> RemoveFailed
        4 -> Removed
        5 -> Removing
        _ -> (error . showText) $ "Unknown index for IPRouteStatusMsg: " <> toText i
    fromEnum x = case x of
        AddFailed -> 0
        Added -> 1
        Adding -> 2
        RemoveFailed -> 3
        Removed -> 4
        Removing -> 5
        IPRouteStatusMsg' name -> (error . showText) $ "Unknown IPRouteStatusMsg: " <> original name

-- | Represents the bounds of /known/ $IPRouteStatusMsg.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded IPRouteStatusMsg where
    minBound = AddFailed
    maxBound = Removing

instance Hashable     IPRouteStatusMsg
instance NFData       IPRouteStatusMsg
instance ToByteString IPRouteStatusMsg
instance ToQuery      IPRouteStatusMsg
instance ToHeader     IPRouteStatusMsg

instance FromJSON IPRouteStatusMsg where
    parseJSON = parseJSONText "IPRouteStatusMsg"
