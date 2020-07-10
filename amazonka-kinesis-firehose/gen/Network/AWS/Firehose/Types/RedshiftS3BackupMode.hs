{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.RedshiftS3BackupMode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.RedshiftS3BackupMode (
  RedshiftS3BackupMode (
    ..
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data RedshiftS3BackupMode = RedshiftS3BackupMode' (CI
                                                     Text)
                              deriving (Eq, Ord, Read, Show, Data, Typeable,
                                        Generic)

pattern Disabled :: RedshiftS3BackupMode
pattern Disabled = RedshiftS3BackupMode' "Disabled"

pattern Enabled :: RedshiftS3BackupMode
pattern Enabled = RedshiftS3BackupMode' "Enabled"

{-# COMPLETE
  Disabled,
  Enabled,
  RedshiftS3BackupMode' #-}

instance FromText RedshiftS3BackupMode where
    parser = (RedshiftS3BackupMode' . mk) <$> takeText

instance ToText RedshiftS3BackupMode where
    toText (RedshiftS3BackupMode' ci) = original ci

-- | Represents an enum of /known/ $RedshiftS3BackupMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum RedshiftS3BackupMode where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        _ -> (error . showText) $ "Unknown index for RedshiftS3BackupMode: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        RedshiftS3BackupMode' name -> (error . showText) $ "Unknown RedshiftS3BackupMode: " <> original name

-- | Represents the bounds of /known/ $RedshiftS3BackupMode.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded RedshiftS3BackupMode where
    minBound = Disabled
    maxBound = Enabled

instance Hashable     RedshiftS3BackupMode
instance NFData       RedshiftS3BackupMode
instance ToByteString RedshiftS3BackupMode
instance ToQuery      RedshiftS3BackupMode
instance ToHeader     RedshiftS3BackupMode

instance ToJSON RedshiftS3BackupMode where
    toJSON = toJSONText

instance FromJSON RedshiftS3BackupMode where
    parseJSON = parseJSONText "RedshiftS3BackupMode"
