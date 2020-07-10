{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Phase2EncryptionAlgorithmsRequestListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the encryption algorithm for the VPN tunnel for phase 2 IKE negotiations.
--
--
--
-- /See:/ 'phase2EncryptionAlgorithmsRequestListValue' smart constructor.
newtype Phase2EncryptionAlgorithmsRequestListValue = Phase2EncryptionAlgorithmsRequestListValue'{_phs2encryptnalgrthmsrqstlstvlValue
                                                                                                 ::
                                                                                                 Maybe
                                                                                                   Text}
                                                       deriving (Eq, Read, Show,
                                                                 Data, Typeable,
                                                                 Generic)

-- | Creates a value of 'Phase2EncryptionAlgorithmsRequestListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phs2encryptnalgrthmsrqstlstvlValue' - The encryption algorithm.
phase2EncryptionAlgorithmsRequestListValue
    :: Phase2EncryptionAlgorithmsRequestListValue
phase2EncryptionAlgorithmsRequestListValue
  = Phase2EncryptionAlgorithmsRequestListValue'{_phs2encryptnalgrthmsrqstlstvlValue
                                                  = Nothing}

-- | The encryption algorithm.
phs2encryptnalgrthmsrqstlstvlValue :: Lens' Phase2EncryptionAlgorithmsRequestListValue (Maybe Text)
phs2encryptnalgrthmsrqstlstvlValue = lens _phs2encryptnalgrthmsrqstlstvlValue (\ s a -> s{_phs2encryptnalgrthmsrqstlstvlValue = a})

instance Hashable
           Phase2EncryptionAlgorithmsRequestListValue
         where

instance NFData
           Phase2EncryptionAlgorithmsRequestListValue
         where

instance ToQuery
           Phase2EncryptionAlgorithmsRequestListValue
         where
        toQuery
          Phase2EncryptionAlgorithmsRequestListValue'{..}
          = mconcat
              ["Value" =: _phs2encryptnalgrthmsrqstlstvlValue]
