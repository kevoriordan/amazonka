{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryValues
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.CostCategoryValues where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Cost Categories values used for filtering the costs.
--
--
--
-- /See:/ 'costCategoryValues' smart constructor.
data CostCategoryValues = CostCategoryValues'{_ccvValues
                                              :: !(Maybe [Text]),
                                              _ccvKey :: !(Maybe Text)}
                            deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CostCategoryValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccvValues' - The specific value of the Cost Category.
--
-- * 'ccvKey' - Undocumented member.
costCategoryValues
    :: CostCategoryValues
costCategoryValues
  = CostCategoryValues'{_ccvValues = Nothing,
                        _ccvKey = Nothing}

-- | The specific value of the Cost Category.
ccvValues :: Lens' CostCategoryValues [Text]
ccvValues = lens _ccvValues (\ s a -> s{_ccvValues = a}) . _Default . _Coerce

-- | Undocumented member.
ccvKey :: Lens' CostCategoryValues (Maybe Text)
ccvKey = lens _ccvKey (\ s a -> s{_ccvKey = a})

instance FromJSON CostCategoryValues where
        parseJSON
          = withObject "CostCategoryValues"
              (\ x ->
                 CostCategoryValues' <$>
                   (x .:? "Values" .!= mempty) <*> (x .:? "Key"))

instance Hashable CostCategoryValues where

instance NFData CostCategoryValues where

instance ToJSON CostCategoryValues where
        toJSON CostCategoryValues'{..}
          = object
              (catMaybes
                 [("Values" .=) <$> _ccvValues,
                  ("Key" .=) <$> _ccvKey])
