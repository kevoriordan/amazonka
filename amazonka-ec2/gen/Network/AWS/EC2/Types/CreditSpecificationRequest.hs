{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreditSpecificationRequest
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CreditSpecificationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The credit option for CPU usage of a T2 or T3 instance.
--
--
--
-- /See:/ 'creditSpecificationRequest' smart constructor.
newtype CreditSpecificationRequest = CreditSpecificationRequest'{_csrCPUCredits
                                                                 :: Text}
                                       deriving (Eq, Read, Show, Data, Typeable,
                                                 Generic)

-- | Creates a value of 'CreditSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrCPUCredits' - The credit option for CPU usage of a T2 or T3 instance. Valid values are @standard@ and @unlimited@ .
creditSpecificationRequest
    :: Text -- ^ 'csrCPUCredits'
    -> CreditSpecificationRequest
creditSpecificationRequest pCPUCredits_
  = CreditSpecificationRequest'{_csrCPUCredits =
                                  pCPUCredits_}

-- | The credit option for CPU usage of a T2 or T3 instance. Valid values are @standard@ and @unlimited@ .
csrCPUCredits :: Lens' CreditSpecificationRequest Text
csrCPUCredits = lens _csrCPUCredits (\ s a -> s{_csrCPUCredits = a})

instance Hashable CreditSpecificationRequest where

instance NFData CreditSpecificationRequest where

instance ToQuery CreditSpecificationRequest where
        toQuery CreditSpecificationRequest'{..}
          = mconcat ["CpuCredits" =: _csrCPUCredits]
