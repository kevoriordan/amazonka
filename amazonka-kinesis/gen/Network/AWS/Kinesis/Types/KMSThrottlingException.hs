{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.KMSThrottlingException
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.KMSThrottlingException where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The request was denied due to request throttling. For more information about throttling, see <http://docs.aws.amazon.com/kms/latest/developerguide/limits.html#requests-per-second Limits> in the /AWS Key Management Service Developer Guide/ .
--
--
--
-- /See:/ 'kmsThrottlingException' smart constructor.
newtype KMSThrottlingException = KMSThrottlingException'{_kteMessage
                                                         :: Maybe Text}
                                   deriving (Eq, Read, Show, Data, Typeable,
                                             Generic)

-- | Creates a value of 'KMSThrottlingException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kteMessage' - A message that provides information about the error.
kmsThrottlingException
    :: KMSThrottlingException
kmsThrottlingException
  = KMSThrottlingException'{_kteMessage = Nothing}

-- | A message that provides information about the error.
kteMessage :: Lens' KMSThrottlingException (Maybe Text)
kteMessage = lens _kteMessage (\ s a -> s{_kteMessage = a})

instance FromJSON KMSThrottlingException where
        parseJSON
          = withObject "KMSThrottlingException"
              (\ x ->
                 KMSThrottlingException' <$> (x .:? "message"))

instance Hashable KMSThrottlingException where

instance NFData KMSThrottlingException where
