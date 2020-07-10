{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Phase1EncryptionAlgorithmsListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The encryption algorithm for phase 1 IKE negotiations.
--
--
--
-- /See:/ 'phase1EncryptionAlgorithmsListValue' smart constructor.
newtype Phase1EncryptionAlgorithmsListValue = Phase1EncryptionAlgorithmsListValue'{_phs1encryptnalgrthmslstvlValue
                                                                                   ::
                                                                                   Maybe
                                                                                     Text}
                                                deriving (Eq, Read, Show, Data,
                                                          Typeable, Generic)

-- | Creates a value of 'Phase1EncryptionAlgorithmsListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phs1encryptnalgrthmslstvlValue' - The value for the encryption algorithm.
phase1EncryptionAlgorithmsListValue
    :: Phase1EncryptionAlgorithmsListValue
phase1EncryptionAlgorithmsListValue
  = Phase1EncryptionAlgorithmsListValue'{_phs1encryptnalgrthmslstvlValue
                                           = Nothing}

-- | The value for the encryption algorithm.
phs1encryptnalgrthmslstvlValue :: Lens' Phase1EncryptionAlgorithmsListValue (Maybe Text)
phs1encryptnalgrthmslstvlValue = lens _phs1encryptnalgrthmslstvlValue (\ s a -> s{_phs1encryptnalgrthmslstvlValue = a})

instance FromXML Phase1EncryptionAlgorithmsListValue
         where
        parseXML x
          = Phase1EncryptionAlgorithmsListValue' <$>
              (x .@? "value")

instance Hashable Phase1EncryptionAlgorithmsListValue
         where

instance NFData Phase1EncryptionAlgorithmsListValue
         where
