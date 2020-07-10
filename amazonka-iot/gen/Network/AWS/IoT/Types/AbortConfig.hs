{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AbortConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AbortConfig where

import Network.AWS.IoT.Types.AbortCriteria
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of abort criteria to abort the job.
--
--
--
-- /See:/ 'abortConfig' smart constructor.
newtype AbortConfig = AbortConfig'{_acCriteriaList ::
                                   List1 AbortCriteria}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AbortConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acCriteriaList' - The list of abort criteria to define rules to abort the job.
abortConfig
    :: NonEmpty AbortCriteria -- ^ 'acCriteriaList'
    -> AbortConfig
abortConfig pCriteriaList_
  = AbortConfig'{_acCriteriaList =
                   _List1 # pCriteriaList_}

-- | The list of abort criteria to define rules to abort the job.
acCriteriaList :: Lens' AbortConfig (NonEmpty AbortCriteria)
acCriteriaList = lens _acCriteriaList (\ s a -> s{_acCriteriaList = a}) . _List1

instance FromJSON AbortConfig where
        parseJSON
          = withObject "AbortConfig"
              (\ x -> AbortConfig' <$> (x .: "criteriaList"))

instance Hashable AbortConfig where

instance NFData AbortConfig where

instance ToJSON AbortConfig where
        toJSON AbortConfig'{..}
          = object
              (catMaybes
                 [Just ("criteriaList" .= _acCriteriaList)])
