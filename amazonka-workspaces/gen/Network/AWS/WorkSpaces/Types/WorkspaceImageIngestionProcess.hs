{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.WorkspaceImageIngestionProcess
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.WorkspaceImageIngestionProcess (
  WorkspaceImageIngestionProcess (
    ..
    , ByolGraphics
    , ByolGraphicspro
    , ByolRegular
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data WorkspaceImageIngestionProcess = WorkspaceImageIngestionProcess' (CI
                                                                         Text)
                                        deriving (Eq, Ord, Read, Show, Data,
                                                  Typeable, Generic)

pattern ByolGraphics :: WorkspaceImageIngestionProcess
pattern ByolGraphics = WorkspaceImageIngestionProcess' "BYOL_GRAPHICS"

pattern ByolGraphicspro :: WorkspaceImageIngestionProcess
pattern ByolGraphicspro = WorkspaceImageIngestionProcess' "BYOL_GRAPHICSPRO"

pattern ByolRegular :: WorkspaceImageIngestionProcess
pattern ByolRegular = WorkspaceImageIngestionProcess' "BYOL_REGULAR"

{-# COMPLETE
  ByolGraphics,
  ByolGraphicspro,
  ByolRegular,
  WorkspaceImageIngestionProcess' #-}

instance FromText WorkspaceImageIngestionProcess where
    parser = (WorkspaceImageIngestionProcess' . mk) <$> takeText

instance ToText WorkspaceImageIngestionProcess where
    toText (WorkspaceImageIngestionProcess' ci) = original ci

-- | Represents an enum of /known/ $WorkspaceImageIngestionProcess.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum WorkspaceImageIngestionProcess where
    toEnum i = case i of
        0 -> ByolGraphics
        1 -> ByolGraphicspro
        2 -> ByolRegular
        _ -> (error . showText) $ "Unknown index for WorkspaceImageIngestionProcess: " <> toText i
    fromEnum x = case x of
        ByolGraphics -> 0
        ByolGraphicspro -> 1
        ByolRegular -> 2
        WorkspaceImageIngestionProcess' name -> (error . showText) $ "Unknown WorkspaceImageIngestionProcess: " <> original name

-- | Represents the bounds of /known/ $WorkspaceImageIngestionProcess.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded WorkspaceImageIngestionProcess where
    minBound = ByolGraphics
    maxBound = ByolRegular

instance Hashable     WorkspaceImageIngestionProcess
instance NFData       WorkspaceImageIngestionProcess
instance ToByteString WorkspaceImageIngestionProcess
instance ToQuery      WorkspaceImageIngestionProcess
instance ToHeader     WorkspaceImageIngestionProcess

instance ToJSON WorkspaceImageIngestionProcess where
    toJSON = toJSONText
