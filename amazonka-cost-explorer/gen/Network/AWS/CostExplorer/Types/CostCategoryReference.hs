{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryReference
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.CostCategoryReference where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A reference to a Cost Category containing only enough information to identify the Cost Category.
--
--
-- You can use this information to retrieve the full Cost Category information using @DescribeCostCategory@ .
--
--
-- /See:/ 'costCategoryReference' smart constructor.
data CostCategoryReference = CostCategoryReference'{_ccrEffectiveStart
                                                    :: !(Maybe Text),
                                                    _ccrCostCategoryARN ::
                                                    !(Maybe Text),
                                                    _ccrNumberOfRules ::
                                                    !(Maybe Nat),
                                                    _ccrName :: !(Maybe Text),
                                                    _ccrEffectiveEnd ::
                                                    !(Maybe Text)}
                               deriving (Eq, Read, Show, Data, Typeable,
                                         Generic)

-- | Creates a value of 'CostCategoryReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrEffectiveStart' - The Cost Category's effective start date.
--
-- * 'ccrCostCategoryARN' - The unique identifier for your Cost Category. 
--
-- * 'ccrNumberOfRules' - The number of rules associated with a specific Cost Category. 
--
-- * 'ccrName' - Undocumented member.
--
-- * 'ccrEffectiveEnd' - The Cost Category's effective end date.
costCategoryReference
    :: CostCategoryReference
costCategoryReference
  = CostCategoryReference'{_ccrEffectiveStart =
                             Nothing,
                           _ccrCostCategoryARN = Nothing,
                           _ccrNumberOfRules = Nothing, _ccrName = Nothing,
                           _ccrEffectiveEnd = Nothing}

-- | The Cost Category's effective start date.
ccrEffectiveStart :: Lens' CostCategoryReference (Maybe Text)
ccrEffectiveStart = lens _ccrEffectiveStart (\ s a -> s{_ccrEffectiveStart = a})

-- | The unique identifier for your Cost Category. 
ccrCostCategoryARN :: Lens' CostCategoryReference (Maybe Text)
ccrCostCategoryARN = lens _ccrCostCategoryARN (\ s a -> s{_ccrCostCategoryARN = a})

-- | The number of rules associated with a specific Cost Category. 
ccrNumberOfRules :: Lens' CostCategoryReference (Maybe Natural)
ccrNumberOfRules = lens _ccrNumberOfRules (\ s a -> s{_ccrNumberOfRules = a}) . mapping _Nat

-- | Undocumented member.
ccrName :: Lens' CostCategoryReference (Maybe Text)
ccrName = lens _ccrName (\ s a -> s{_ccrName = a})

-- | The Cost Category's effective end date.
ccrEffectiveEnd :: Lens' CostCategoryReference (Maybe Text)
ccrEffectiveEnd = lens _ccrEffectiveEnd (\ s a -> s{_ccrEffectiveEnd = a})

instance FromJSON CostCategoryReference where
        parseJSON
          = withObject "CostCategoryReference"
              (\ x ->
                 CostCategoryReference' <$>
                   (x .:? "EffectiveStart") <*>
                     (x .:? "CostCategoryArn")
                     <*> (x .:? "NumberOfRules")
                     <*> (x .:? "Name")
                     <*> (x .:? "EffectiveEnd"))

instance Hashable CostCategoryReference where

instance NFData CostCategoryReference where
