{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CACertificateUpdateAction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.CACertificateUpdateAction (
  CACertificateUpdateAction (
    ..
    , Deactivate
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data CACertificateUpdateAction = CACertificateUpdateAction' (CI
                                                               Text)
                                   deriving (Eq, Ord, Read, Show, Data,
                                             Typeable, Generic)

pattern Deactivate :: CACertificateUpdateAction
pattern Deactivate = CACertificateUpdateAction' "DEACTIVATE"

{-# COMPLETE
  Deactivate,
  CACertificateUpdateAction' #-}

instance FromText CACertificateUpdateAction where
    parser = (CACertificateUpdateAction' . mk) <$> takeText

instance ToText CACertificateUpdateAction where
    toText (CACertificateUpdateAction' ci) = original ci

-- | Represents an enum of /known/ $CACertificateUpdateAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum CACertificateUpdateAction where
    toEnum i = case i of
        0 -> Deactivate
        _ -> (error . showText) $ "Unknown index for CACertificateUpdateAction: " <> toText i
    fromEnum x = case x of
        Deactivate -> 0
        CACertificateUpdateAction' name -> (error . showText) $ "Unknown CACertificateUpdateAction: " <> original name

-- | Represents the bounds of /known/ $CACertificateUpdateAction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded CACertificateUpdateAction where
    minBound = Deactivate
    maxBound = Deactivate

instance Hashable     CACertificateUpdateAction
instance NFData       CACertificateUpdateAction
instance ToByteString CACertificateUpdateAction
instance ToQuery      CACertificateUpdateAction
instance ToHeader     CACertificateUpdateAction

instance ToJSON CACertificateUpdateAction where
    toJSON = toJSONText

instance FromJSON CACertificateUpdateAction where
    parseJSON = parseJSONText "CACertificateUpdateAction"
