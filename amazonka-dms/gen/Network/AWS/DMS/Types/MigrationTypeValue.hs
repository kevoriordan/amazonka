{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MigrationTypeValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.MigrationTypeValue (
  MigrationTypeValue (
    ..
    , Cdc
    , FullLoad
    , FullLoadAndCdc
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data MigrationTypeValue = MigrationTypeValue' (CI
                                                 Text)
                            deriving (Eq, Ord, Read, Show, Data, Typeable,
                                      Generic)

pattern Cdc :: MigrationTypeValue
pattern Cdc = MigrationTypeValue' "cdc"

pattern FullLoad :: MigrationTypeValue
pattern FullLoad = MigrationTypeValue' "full-load"

pattern FullLoadAndCdc :: MigrationTypeValue
pattern FullLoadAndCdc = MigrationTypeValue' "full-load-and-cdc"

{-# COMPLETE
  Cdc,
  FullLoad,
  FullLoadAndCdc,
  MigrationTypeValue' #-}

instance FromText MigrationTypeValue where
    parser = (MigrationTypeValue' . mk) <$> takeText

instance ToText MigrationTypeValue where
    toText (MigrationTypeValue' ci) = original ci

-- | Represents an enum of /known/ $MigrationTypeValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum MigrationTypeValue where
    toEnum i = case i of
        0 -> Cdc
        1 -> FullLoad
        2 -> FullLoadAndCdc
        _ -> (error . showText) $ "Unknown index for MigrationTypeValue: " <> toText i
    fromEnum x = case x of
        Cdc -> 0
        FullLoad -> 1
        FullLoadAndCdc -> 2
        MigrationTypeValue' name -> (error . showText) $ "Unknown MigrationTypeValue: " <> original name

-- | Represents the bounds of /known/ $MigrationTypeValue.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded MigrationTypeValue where
    minBound = Cdc
    maxBound = FullLoadAndCdc

instance Hashable     MigrationTypeValue
instance NFData       MigrationTypeValue
instance ToByteString MigrationTypeValue
instance ToQuery      MigrationTypeValue
instance ToHeader     MigrationTypeValue

instance ToJSON MigrationTypeValue where
    toJSON = toJSONText

instance FromJSON MigrationTypeValue where
    parseJSON = parseJSONText "MigrationTypeValue"
