{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the integrity algorithm for the VPN tunnel for phase 1 IKE negotiations.
--
--
--
-- /See:/ 'phase1IntegrityAlgorithmsRequestListValue' smart constructor.
newtype Phase1IntegrityAlgorithmsRequestListValue = Phase1IntegrityAlgorithmsRequestListValue'{_phs1intgrtyalgrthmsrqstlstvlValue
                                                                                               ::
                                                                                               Maybe
                                                                                                 Text}
                                                      deriving (Eq, Read, Show,
                                                                Data, Typeable,
                                                                Generic)

-- | Creates a value of 'Phase1IntegrityAlgorithmsRequestListValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'phs1intgrtyalgrthmsrqstlstvlValue' - The value for the integrity algorithm.
phase1IntegrityAlgorithmsRequestListValue
    :: Phase1IntegrityAlgorithmsRequestListValue
phase1IntegrityAlgorithmsRequestListValue
  = Phase1IntegrityAlgorithmsRequestListValue'{_phs1intgrtyalgrthmsrqstlstvlValue
                                                 = Nothing}

-- | The value for the integrity algorithm.
phs1intgrtyalgrthmsrqstlstvlValue :: Lens' Phase1IntegrityAlgorithmsRequestListValue (Maybe Text)
phs1intgrtyalgrthmsrqstlstvlValue = lens _phs1intgrtyalgrthmsrqstlstvlValue (\ s a -> s{_phs1intgrtyalgrthmsrqstlstvlValue = a})

instance Hashable
           Phase1IntegrityAlgorithmsRequestListValue
         where

instance NFData
           Phase1IntegrityAlgorithmsRequestListValue
         where

instance ToQuery
           Phase1IntegrityAlgorithmsRequestListValue
         where
        toQuery
          Phase1IntegrityAlgorithmsRequestListValue'{..}
          = mconcat
              ["Value" =: _phs1intgrtyalgrthmsrqstlstvlValue]
