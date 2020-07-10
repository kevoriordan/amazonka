{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.KMSOptInRequired
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.KMSOptInRequired where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The AWS access key ID needs a subscription for the service.
--
--
--
-- /See:/ 'kmsOptInRequired' smart constructor.
newtype KMSOptInRequired = KMSOptInRequired'{_koirMessage
                                             :: Maybe Text}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KMSOptInRequired' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'koirMessage' - A message that provides information about the error.
kmsOptInRequired
    :: KMSOptInRequired
kmsOptInRequired
  = KMSOptInRequired'{_koirMessage = Nothing}

-- | A message that provides information about the error.
koirMessage :: Lens' KMSOptInRequired (Maybe Text)
koirMessage = lens _koirMessage (\ s a -> s{_koirMessage = a})

instance FromJSON KMSOptInRequired where
        parseJSON
          = withObject "KMSOptInRequired"
              (\ x -> KMSOptInRequired' <$> (x .:? "message"))

instance Hashable KMSOptInRequired where

instance NFData KMSOptInRequired where
