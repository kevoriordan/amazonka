{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryStage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.DirectoryStage (
  DirectoryStage (
    ..
    , DSActive
    , DSCreated
    , DSCreating
    , DSDeleted
    , DSDeleting
    , DSFailed
    , DSImpaired
    , DSInoperable
    , DSRequested
    , DSRestoreFailed
    , DSRestoring
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data DirectoryStage = DirectoryStage' (CI Text)
                        deriving (Eq, Ord, Read, Show, Data, Typeable,
                                  Generic)

pattern DSActive :: DirectoryStage
pattern DSActive = DirectoryStage' "Active"

pattern DSCreated :: DirectoryStage
pattern DSCreated = DirectoryStage' "Created"

pattern DSCreating :: DirectoryStage
pattern DSCreating = DirectoryStage' "Creating"

pattern DSDeleted :: DirectoryStage
pattern DSDeleted = DirectoryStage' "Deleted"

pattern DSDeleting :: DirectoryStage
pattern DSDeleting = DirectoryStage' "Deleting"

pattern DSFailed :: DirectoryStage
pattern DSFailed = DirectoryStage' "Failed"

pattern DSImpaired :: DirectoryStage
pattern DSImpaired = DirectoryStage' "Impaired"

pattern DSInoperable :: DirectoryStage
pattern DSInoperable = DirectoryStage' "Inoperable"

pattern DSRequested :: DirectoryStage
pattern DSRequested = DirectoryStage' "Requested"

pattern DSRestoreFailed :: DirectoryStage
pattern DSRestoreFailed = DirectoryStage' "RestoreFailed"

pattern DSRestoring :: DirectoryStage
pattern DSRestoring = DirectoryStage' "Restoring"

{-# COMPLETE
  DSActive,
  DSCreated,
  DSCreating,
  DSDeleted,
  DSDeleting,
  DSFailed,
  DSImpaired,
  DSInoperable,
  DSRequested,
  DSRestoreFailed,
  DSRestoring,
  DirectoryStage' #-}

instance FromText DirectoryStage where
    parser = (DirectoryStage' . mk) <$> takeText

instance ToText DirectoryStage where
    toText (DirectoryStage' ci) = original ci

-- | Represents an enum of /known/ $DirectoryStage.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum DirectoryStage where
    toEnum i = case i of
        0 -> DSActive
        1 -> DSCreated
        2 -> DSCreating
        3 -> DSDeleted
        4 -> DSDeleting
        5 -> DSFailed
        6 -> DSImpaired
        7 -> DSInoperable
        8 -> DSRequested
        9 -> DSRestoreFailed
        10 -> DSRestoring
        _ -> (error . showText) $ "Unknown index for DirectoryStage: " <> toText i
    fromEnum x = case x of
        DSActive -> 0
        DSCreated -> 1
        DSCreating -> 2
        DSDeleted -> 3
        DSDeleting -> 4
        DSFailed -> 5
        DSImpaired -> 6
        DSInoperable -> 7
        DSRequested -> 8
        DSRestoreFailed -> 9
        DSRestoring -> 10
        DirectoryStage' name -> (error . showText) $ "Unknown DirectoryStage: " <> original name

-- | Represents the bounds of /known/ $DirectoryStage.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded DirectoryStage where
    minBound = DSActive
    maxBound = DSRestoring

instance Hashable     DirectoryStage
instance NFData       DirectoryStage
instance ToByteString DirectoryStage
instance ToQuery      DirectoryStage
instance ToHeader     DirectoryStage

instance FromJSON DirectoryStage where
    parseJSON = parseJSONText "DirectoryStage"
