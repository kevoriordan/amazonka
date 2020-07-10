{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateBehavior
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.UpdateBehavior (
  UpdateBehavior (
    ..
    , UBLog
    , UBUpdateInDatabase
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data UpdateBehavior = UpdateBehavior' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern UBLog :: UpdateBehavior
pattern UBLog = UpdateBehavior' "LOG"

pattern UBUpdateInDatabase :: UpdateBehavior
pattern UBUpdateInDatabase = UpdateBehavior' "UPDATE_IN_DATABASE"

{-# COMPLETE
  UBLog,
  UBUpdateInDatabase,
  UpdateBehavior' #-}

instance FromText UpdateBehavior where
    parser = (UpdateBehavior' . mk) <$> takeText

instance ToText UpdateBehavior where
    toText (UpdateBehavior' ci) = original ci

-- | Represents an enum of /known/ $UpdateBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum UpdateBehavior where
    toEnum i = case i of
        0 -> UBLog
        1 -> UBUpdateInDatabase
        _ -> (error . showText) $ "Unknown index for UpdateBehavior: " <> toText i
    fromEnum x = case x of
        UBLog -> 0
        UBUpdateInDatabase -> 1
        UpdateBehavior' name -> (error . showText) $ "Unknown UpdateBehavior: " <> original name

-- | Represents the bounds of /known/ $UpdateBehavior.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded UpdateBehavior where
    minBound = UBLog
    maxBound = UBUpdateInDatabase

instance Hashable     UpdateBehavior
instance NFData       UpdateBehavior
instance ToByteString UpdateBehavior
instance ToQuery      UpdateBehavior
instance ToHeader     UpdateBehavior

instance ToJSON UpdateBehavior where
    toJSON = toJSONText

instance FromJSON UpdateBehavior where
    parseJSON = parseJSONText "UpdateBehavior"
