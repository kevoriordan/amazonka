{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceTypeForTagging
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ResourceTypeForTagging (
  ResourceTypeForTagging (
    ..
    , RTFTDocument
    , RTFTMaintenanceWindow
    , RTFTManagedInstance
    , RTFTOpsItem
    , RTFTParameter
    , RTFTPatchBaseline
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ResourceTypeForTagging = ResourceTypeForTagging' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern RTFTDocument :: ResourceTypeForTagging
pattern RTFTDocument = ResourceTypeForTagging' "Document"

pattern RTFTMaintenanceWindow :: ResourceTypeForTagging
pattern RTFTMaintenanceWindow = ResourceTypeForTagging' "MaintenanceWindow"

pattern RTFTManagedInstance :: ResourceTypeForTagging
pattern RTFTManagedInstance = ResourceTypeForTagging' "ManagedInstance"

pattern RTFTOpsItem :: ResourceTypeForTagging
pattern RTFTOpsItem = ResourceTypeForTagging' "OpsItem"

pattern RTFTParameter :: ResourceTypeForTagging
pattern RTFTParameter = ResourceTypeForTagging' "Parameter"

pattern RTFTPatchBaseline :: ResourceTypeForTagging
pattern RTFTPatchBaseline = ResourceTypeForTagging' "PatchBaseline"

{-# COMPLETE
  RTFTDocument,
  RTFTMaintenanceWindow,
  RTFTManagedInstance,
  RTFTOpsItem,
  RTFTParameter,
  RTFTPatchBaseline,
  ResourceTypeForTagging' #-}

instance FromText ResourceTypeForTagging where
    parser = (ResourceTypeForTagging' . mk) <$> takeText

instance ToText ResourceTypeForTagging where
    toText (ResourceTypeForTagging' ci) = original ci

-- | Represents an enum of /known/ $ResourceTypeForTagging.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ResourceTypeForTagging where
    toEnum i = case i of
        0 -> RTFTDocument
        1 -> RTFTMaintenanceWindow
        2 -> RTFTManagedInstance
        3 -> RTFTOpsItem
        4 -> RTFTParameter
        5 -> RTFTPatchBaseline
        _ -> (error . showText) $ "Unknown index for ResourceTypeForTagging: " <> toText i
    fromEnum x = case x of
        RTFTDocument -> 0
        RTFTMaintenanceWindow -> 1
        RTFTManagedInstance -> 2
        RTFTOpsItem -> 3
        RTFTParameter -> 4
        RTFTPatchBaseline -> 5
        ResourceTypeForTagging' name -> (error . showText) $ "Unknown ResourceTypeForTagging: " <> original name

-- | Represents the bounds of /known/ $ResourceTypeForTagging.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ResourceTypeForTagging where
    minBound = RTFTDocument
    maxBound = RTFTPatchBaseline

instance Hashable     ResourceTypeForTagging
instance NFData       ResourceTypeForTagging
instance ToByteString ResourceTypeForTagging
instance ToQuery      ResourceTypeForTagging
instance ToHeader     ResourceTypeForTagging

instance ToJSON ResourceTypeForTagging where
    toJSON = toJSONText
