{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SelectiveAuth
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.SelectiveAuth (
  SelectiveAuth (
    ..
    , Disabled
    , Enabled
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data SelectiveAuth = SelectiveAuth' (CI Text)
                       deriving (Eq, Ord, Read, Show, Data, Typeable,
                                 Generic)

pattern Disabled :: SelectiveAuth
pattern Disabled = SelectiveAuth' "Disabled"

pattern Enabled :: SelectiveAuth
pattern Enabled = SelectiveAuth' "Enabled"

{-# COMPLETE
  Disabled,
  Enabled,
  SelectiveAuth' #-}

instance FromText SelectiveAuth where
    parser = (SelectiveAuth' . mk) <$> takeText

instance ToText SelectiveAuth where
    toText (SelectiveAuth' ci) = original ci

-- | Represents an enum of /known/ $SelectiveAuth.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum SelectiveAuth where
    toEnum i = case i of
        0 -> Disabled
        1 -> Enabled
        _ -> (error . showText) $ "Unknown index for SelectiveAuth: " <> toText i
    fromEnum x = case x of
        Disabled -> 0
        Enabled -> 1
        SelectiveAuth' name -> (error . showText) $ "Unknown SelectiveAuth: " <> original name

-- | Represents the bounds of /known/ $SelectiveAuth.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded SelectiveAuth where
    minBound = Disabled
    maxBound = Enabled

instance Hashable     SelectiveAuth
instance NFData       SelectiveAuth
instance ToByteString SelectiveAuth
instance ToQuery      SelectiveAuth
instance ToHeader     SelectiveAuth

instance ToJSON SelectiveAuth where
    toJSON = toJSONText

instance FromJSON SelectiveAuth where
    parseJSON = parseJSONText "SelectiveAuth"
