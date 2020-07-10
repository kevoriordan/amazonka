{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeSetType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.ChangeSetType (
  ChangeSetType (
    ..
    , CSTCreate
    , CSTImport
    , CSTUpdate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ChangeSetType = ChangeSetType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern CSTCreate :: ChangeSetType
pattern CSTCreate = ChangeSetType' "CREATE"

pattern CSTImport :: ChangeSetType
pattern CSTImport = ChangeSetType' "IMPORT"

pattern CSTUpdate :: ChangeSetType
pattern CSTUpdate = ChangeSetType' "UPDATE"

{-# COMPLETE
  CSTCreate,
  CSTImport,
  CSTUpdate,
  ChangeSetType' #-}

instance FromText ChangeSetType where
    parser = (ChangeSetType' . mk) <$> takeText

instance ToText ChangeSetType where
    toText (ChangeSetType' ci) = original ci

-- | Represents an enum of /known/ $ChangeSetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ChangeSetType where
    toEnum i = case i of
        0 -> CSTCreate
        1 -> CSTImport
        2 -> CSTUpdate
        _ -> (error . showText) $ "Unknown index for ChangeSetType: " <> toText i
    fromEnum x = case x of
        CSTCreate -> 0
        CSTImport -> 1
        CSTUpdate -> 2
        ChangeSetType' name -> (error . showText) $ "Unknown ChangeSetType: " <> original name

-- | Represents the bounds of /known/ $ChangeSetType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ChangeSetType where
    minBound = CSTCreate
    maxBound = CSTUpdate

instance Hashable     ChangeSetType
instance NFData       ChangeSetType
instance ToByteString ChangeSetType
instance ToQuery      ChangeSetType
instance ToHeader     ChangeSetType
