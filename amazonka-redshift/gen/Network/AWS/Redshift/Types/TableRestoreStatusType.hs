{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.TableRestoreStatusType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.TableRestoreStatusType (
  TableRestoreStatusType (
    ..
    , TRSTCanceled
    , TRSTFailed
    , TRSTInProgress
    , TRSTPending
    , TRSTSucceeded
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data TableRestoreStatusType = TableRestoreStatusType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern TRSTCanceled :: TableRestoreStatusType
pattern TRSTCanceled = TableRestoreStatusType' "CANCELED"

pattern TRSTFailed :: TableRestoreStatusType
pattern TRSTFailed = TableRestoreStatusType' "FAILED"

pattern TRSTInProgress :: TableRestoreStatusType
pattern TRSTInProgress = TableRestoreStatusType' "IN_PROGRESS"

pattern TRSTPending :: TableRestoreStatusType
pattern TRSTPending = TableRestoreStatusType' "PENDING"

pattern TRSTSucceeded :: TableRestoreStatusType
pattern TRSTSucceeded = TableRestoreStatusType' "SUCCEEDED"

{-# COMPLETE
  TRSTCanceled,
  TRSTFailed,
  TRSTInProgress,
  TRSTPending,
  TRSTSucceeded,
  TableRestoreStatusType' #-}

instance FromText TableRestoreStatusType where
    parser = (TableRestoreStatusType' . mk) <$> takeText

instance ToText TableRestoreStatusType where
    toText (TableRestoreStatusType' ci) = original ci

-- | Represents an enum of /known/ $TableRestoreStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum TableRestoreStatusType where
    toEnum i = case i of
        0 -> TRSTCanceled
        1 -> TRSTFailed
        2 -> TRSTInProgress
        3 -> TRSTPending
        4 -> TRSTSucceeded
        _ -> (error . showText) $ "Unknown index for TableRestoreStatusType: " <> toText i
    fromEnum x = case x of
        TRSTCanceled -> 0
        TRSTFailed -> 1
        TRSTInProgress -> 2
        TRSTPending -> 3
        TRSTSucceeded -> 4
        TableRestoreStatusType' name -> (error . showText) $ "Unknown TableRestoreStatusType: " <> original name

-- | Represents the bounds of /known/ $TableRestoreStatusType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded TableRestoreStatusType where
    minBound = TRSTCanceled
    maxBound = TRSTSucceeded

instance Hashable     TableRestoreStatusType
instance NFData       TableRestoreStatusType
instance ToByteString TableRestoreStatusType
instance ToQuery      TableRestoreStatusType
instance ToHeader     TableRestoreStatusType

instance FromXML TableRestoreStatusType where
    parseXML = parseXMLText "TableRestoreStatusType"
