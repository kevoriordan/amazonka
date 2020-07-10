{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.FileShareType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.FileShareType (
  FileShareType (
    ..
    , Nfs
    , Smb
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | The type of the file share.
--
--
data FileShareType = FileShareType' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Nfs :: FileShareType
pattern Nfs = FileShareType' "NFS"

pattern Smb :: FileShareType
pattern Smb = FileShareType' "SMB"

{-# COMPLETE
  Nfs,
  Smb,
  FileShareType' #-}

instance FromText FileShareType where
    parser = (FileShareType' . mk) <$> takeText

instance ToText FileShareType where
    toText (FileShareType' ci) = original ci

-- | Represents an enum of /known/ $FileShareType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum FileShareType where
    toEnum i = case i of
        0 -> Nfs
        1 -> Smb
        _ -> (error . showText) $ "Unknown index for FileShareType: " <> toText i
    fromEnum x = case x of
        Nfs -> 0
        Smb -> 1
        FileShareType' name -> (error . showText) $ "Unknown FileShareType: " <> original name

-- | Represents the bounds of /known/ $FileShareType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded FileShareType where
    minBound = Nfs
    maxBound = Smb

instance Hashable     FileShareType
instance NFData       FileShareType
instance ToByteString FileShareType
instance ToQuery      FileShareType
instance ToHeader     FileShareType

instance FromJSON FileShareType where
    parseJSON = parseJSONText "FileShareType"
