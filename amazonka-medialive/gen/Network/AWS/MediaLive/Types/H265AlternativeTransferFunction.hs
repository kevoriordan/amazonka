{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265AlternativeTransferFunction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H265AlternativeTransferFunction (
  H265AlternativeTransferFunction (
    ..
    , Insert
    , Omit
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

-- | H265 Alternative Transfer Function
data H265AlternativeTransferFunction = H265AlternativeTransferFunction' (CI
                                                                           Text)
                                         deriving (Eq, Ord, Read, Show, Data,
                                                   Typeable, Generic)

pattern Insert :: H265AlternativeTransferFunction
pattern Insert = H265AlternativeTransferFunction' "INSERT"

pattern Omit :: H265AlternativeTransferFunction
pattern Omit = H265AlternativeTransferFunction' "OMIT"

{-# COMPLETE
  Insert,
  Omit,
  H265AlternativeTransferFunction' #-}

instance FromText H265AlternativeTransferFunction where
    parser = (H265AlternativeTransferFunction' . mk) <$> takeText

instance ToText H265AlternativeTransferFunction where
    toText (H265AlternativeTransferFunction' ci) = original ci

-- | Represents an enum of /known/ $H265AlternativeTransferFunction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum H265AlternativeTransferFunction where
    toEnum i = case i of
        0 -> Insert
        1 -> Omit
        _ -> (error . showText) $ "Unknown index for H265AlternativeTransferFunction: " <> toText i
    fromEnum x = case x of
        Insert -> 0
        Omit -> 1
        H265AlternativeTransferFunction' name -> (error . showText) $ "Unknown H265AlternativeTransferFunction: " <> original name

-- | Represents the bounds of /known/ $H265AlternativeTransferFunction.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded H265AlternativeTransferFunction where
    minBound = Insert
    maxBound = Omit

instance Hashable     H265AlternativeTransferFunction
instance NFData       H265AlternativeTransferFunction
instance ToByteString H265AlternativeTransferFunction
instance ToQuery      H265AlternativeTransferFunction
instance ToHeader     H265AlternativeTransferFunction

instance ToJSON H265AlternativeTransferFunction where
    toJSON = toJSONText

instance FromJSON H265AlternativeTransferFunction where
    parseJSON = parseJSONText "H265AlternativeTransferFunction"
