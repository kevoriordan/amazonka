{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.ConferenceProviderType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.ConferenceProviderType (
  ConferenceProviderType (
    ..
    , Bluejeans
    , Chime
    , Custom
    , Fuze
    , GoogleHangouts
    , Polycom
    , Ringcentral
    , SkypeForBusiness
    , Webex
    , Zoom
    )
  ) where

import Data.CaseInsensitive
import Network.AWS.Prelude

data ConferenceProviderType = ConferenceProviderType' (CI
                                                         Text)
                                deriving (Eq, Ord, Read, Show, Data, Typeable,
                                          Generic)

pattern Bluejeans :: ConferenceProviderType
pattern Bluejeans = ConferenceProviderType' "BLUEJEANS"

pattern Chime :: ConferenceProviderType
pattern Chime = ConferenceProviderType' "CHIME"

pattern Custom :: ConferenceProviderType
pattern Custom = ConferenceProviderType' "CUSTOM"

pattern Fuze :: ConferenceProviderType
pattern Fuze = ConferenceProviderType' "FUZE"

pattern GoogleHangouts :: ConferenceProviderType
pattern GoogleHangouts = ConferenceProviderType' "GOOGLE_HANGOUTS"

pattern Polycom :: ConferenceProviderType
pattern Polycom = ConferenceProviderType' "POLYCOM"

pattern Ringcentral :: ConferenceProviderType
pattern Ringcentral = ConferenceProviderType' "RINGCENTRAL"

pattern SkypeForBusiness :: ConferenceProviderType
pattern SkypeForBusiness = ConferenceProviderType' "SKYPE_FOR_BUSINESS"

pattern Webex :: ConferenceProviderType
pattern Webex = ConferenceProviderType' "WEBEX"

pattern Zoom :: ConferenceProviderType
pattern Zoom = ConferenceProviderType' "ZOOM"

{-# COMPLETE
  Bluejeans,
  Chime,
  Custom,
  Fuze,
  GoogleHangouts,
  Polycom,
  Ringcentral,
  SkypeForBusiness,
  Webex,
  Zoom,
  ConferenceProviderType' #-}

instance FromText ConferenceProviderType where
    parser = (ConferenceProviderType' . mk) <$> takeText

instance ToText ConferenceProviderType where
    toText (ConferenceProviderType' ci) = original ci

-- | Represents an enum of /known/ $ConferenceProviderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
--   fromEnum is a partial function, and will error on values unknown at generation time.
instance Enum ConferenceProviderType where
    toEnum i = case i of
        0 -> Bluejeans
        1 -> Chime
        2 -> Custom
        3 -> Fuze
        4 -> GoogleHangouts
        5 -> Polycom
        6 -> Ringcentral
        7 -> SkypeForBusiness
        8 -> Webex
        9 -> Zoom
        _ -> (error . showText) $ "Unknown index for ConferenceProviderType: " <> toText i
    fromEnum x = case x of
        Bluejeans -> 0
        Chime -> 1
        Custom -> 2
        Fuze -> 3
        GoogleHangouts -> 4
        Polycom -> 5
        Ringcentral -> 6
        SkypeForBusiness -> 7
        Webex -> 8
        Zoom -> 9
        ConferenceProviderType' name -> (error . showText) $ "Unknown ConferenceProviderType: " <> original name

-- | Represents the bounds of /known/ $ConferenceProviderType.
--   AWS may have added more since the source was generated.
--   This instance exists only for backward compatibility.
instance Bounded ConferenceProviderType where
    minBound = Bluejeans
    maxBound = Zoom

instance Hashable     ConferenceProviderType
instance NFData       ConferenceProviderType
instance ToByteString ConferenceProviderType
instance ToQuery      ConferenceProviderType
instance ToHeader     ConferenceProviderType

instance ToJSON ConferenceProviderType where
    toJSON = toJSONText

instance FromJSON ConferenceProviderType where
    parseJSON = parseJSONText "ConferenceProviderType"
