{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.KMSAccessDeniedException
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.KMSAccessDeniedException where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The ciphertext references a key that doesn't exist or that you don't have access to.
--
--
--
-- /See:/ 'kmsAccessDeniedException' smart constructor.
newtype KMSAccessDeniedException = KMSAccessDeniedException'{_kadeMessage
                                                             :: Maybe Text}
                                     deriving (Eq, Read, Show, Data, Typeable,
                                               Generic)

-- | Creates a value of 'KMSAccessDeniedException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kadeMessage' - A message that provides information about the error.
kmsAccessDeniedException
    :: KMSAccessDeniedException
kmsAccessDeniedException
  = KMSAccessDeniedException'{_kadeMessage = Nothing}

-- | A message that provides information about the error.
kadeMessage :: Lens' KMSAccessDeniedException (Maybe Text)
kadeMessage = lens _kadeMessage (\ s a -> s{_kadeMessage = a})

instance FromJSON KMSAccessDeniedException where
        parseJSON
          = withObject "KMSAccessDeniedException"
              (\ x ->
                 KMSAccessDeniedException' <$> (x .:? "message"))

instance Hashable KMSAccessDeniedException where

instance NFData KMSAccessDeniedException where
