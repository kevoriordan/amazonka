{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.EmergencyContact
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.EmergencyContact where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contact information that the DRT can use to contact you during a suspected attack.
--
--
--
-- /See:/ 'emergencyContact' smart constructor.
newtype EmergencyContact = EmergencyContact'{_ecEmailAddress
                                             :: Text}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EmergencyContact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecEmailAddress' - An email address that the DRT can use to contact you during a suspected attack.
emergencyContact
    :: Text -- ^ 'ecEmailAddress'
    -> EmergencyContact
emergencyContact pEmailAddress_
  = EmergencyContact'{_ecEmailAddress = pEmailAddress_}

-- | An email address that the DRT can use to contact you during a suspected attack.
ecEmailAddress :: Lens' EmergencyContact Text
ecEmailAddress = lens _ecEmailAddress (\ s a -> s{_ecEmailAddress = a})

instance FromJSON EmergencyContact where
        parseJSON
          = withObject "EmergencyContact"
              (\ x -> EmergencyContact' <$> (x .: "EmailAddress"))

instance Hashable EmergencyContact where

instance NFData EmergencyContact where

instance ToJSON EmergencyContact where
        toJSON EmergencyContact'{..}
          = object
              (catMaybes
                 [Just ("EmailAddress" .= _ecEmailAddress)])
