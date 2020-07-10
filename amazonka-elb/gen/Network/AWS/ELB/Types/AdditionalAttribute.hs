{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.AdditionalAttribute
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELB.Types.AdditionalAttribute where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is reserved.
--
--
--
-- /See:/ 'additionalAttribute' smart constructor.
data AdditionalAttribute = AdditionalAttribute'{_aaValue
                                                :: !(Maybe Text),
                                                _aaKey :: !(Maybe Text)}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdditionalAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaValue' - This parameter is reserved.
--
-- * 'aaKey' - This parameter is reserved.
additionalAttribute
    :: AdditionalAttribute
additionalAttribute
  = AdditionalAttribute'{_aaValue = Nothing,
                         _aaKey = Nothing}

-- | This parameter is reserved.
aaValue :: Lens' AdditionalAttribute (Maybe Text)
aaValue = lens _aaValue (\ s a -> s{_aaValue = a})

-- | This parameter is reserved.
aaKey :: Lens' AdditionalAttribute (Maybe Text)
aaKey = lens _aaKey (\ s a -> s{_aaKey = a})

instance FromXML AdditionalAttribute where
        parseXML x
          = AdditionalAttribute' <$>
              (x .@? "Value") <*> (x .@? "Key")

instance Hashable AdditionalAttribute where

instance NFData AdditionalAttribute where

instance ToQuery AdditionalAttribute where
        toQuery AdditionalAttribute'{..}
          = mconcat ["Value" =: _aaValue, "Key" =: _aaKey]
