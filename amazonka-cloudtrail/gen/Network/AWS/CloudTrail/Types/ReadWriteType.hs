{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.ReadWriteType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudTrail.Types.ReadWriteType (
  ReadWriteType (
    ..
    , RWTAll
    , RWTReadOnly
    , RWTWriteOnly
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ReadWriteType = ReadWriteType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern RWTAll :: ReadWriteType
pattern RWTAll = ReadWriteType' "All"

pattern RWTReadOnly :: ReadWriteType
pattern RWTReadOnly = ReadWriteType' "ReadOnly"

pattern RWTWriteOnly :: ReadWriteType
pattern RWTWriteOnly = ReadWriteType' "WriteOnly"

{-# COMPLETE
  RWTAll,
  RWTReadOnly,
  RWTWriteOnly,
  ReadWriteType' #-}

instance FromText ReadWriteType where
    parser = (ReadWriteType' . mk) <$> takeText

instance ToText ReadWriteType where
    toText (ReadWriteType' ci) = original ci

-- | Represents an enum of /known/ $ReadWriteType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ReadWriteType where
    toEnum i = case i of
        0 -> RWTAll
        1 -> RWTReadOnly
        2 -> RWTWriteOnly
        _ -> (error . showText) $ "Unknown index for ReadWriteType: " <> toText i
    fromEnum x = case x of
        RWTAll -> 0
        RWTReadOnly -> 1
        RWTWriteOnly -> 2
        ReadWriteType' name -> (error . showText) $ "Unknown ReadWriteType: " <> original name

-- | Represents the bounds of /known/ $ReadWriteType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ReadWriteType where
    minBound = RWTAll
    maxBound = RWTWriteOnly

instance Hashable     ReadWriteType
instance NFData       ReadWriteType
instance ToByteString ReadWriteType
instance ToQuery      ReadWriteType
instance ToHeader     ReadWriteType

instance ToJSON ReadWriteType where
    toJSON = toJSONText

instance FromJSON ReadWriteType where
    parseJSON = parseJSONText "ReadWriteType"
