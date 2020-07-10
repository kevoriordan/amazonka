{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.KMSDisabledException
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.KMSDisabledException where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The request was rejected because the specified customer master key (CMK) isn't enabled.
--
--
--
-- /See:/ 'kmsDisabledException' smart constructor.
newtype KMSDisabledException = KMSDisabledException'{_kdeMessage
                                                     :: Maybe Text}
                                 deriving (Eq, Read, Show, Data, Typeable,
                                           Generic)

-- | Creates a value of 'KMSDisabledException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kdeMessage' - A message that provides information about the error.
kmsDisabledException
    :: KMSDisabledException
kmsDisabledException
  = KMSDisabledException'{_kdeMessage = Nothing}

-- | A message that provides information about the error.
kdeMessage :: Lens' KMSDisabledException (Maybe Text)
kdeMessage = lens _kdeMessage (\ s a -> s{_kdeMessage = a})

instance FromJSON KMSDisabledException where
        parseJSON
          = withObject "KMSDisabledException"
              (\ x -> KMSDisabledException' <$> (x .:? "message"))

instance Hashable KMSDisabledException where

instance NFData KMSDisabledException where
