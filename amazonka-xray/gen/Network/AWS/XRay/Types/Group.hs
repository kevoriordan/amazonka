{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Group
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.Group where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details and metadata for a group.
--
--
--
-- /See:/ 'group'' smart constructor.
data Group = Group'{_gFilterExpression ::
                    !(Maybe Text),
                    _gGroupARN :: !(Maybe Text),
                    _gGroupName :: !(Maybe Text)}
               deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gFilterExpression' - The filter expression defining the parameters to include traces.
--
-- * 'gGroupARN' - The ARN of the group generated based on the GroupName.
--
-- * 'gGroupName' - The unique case-sensitive name of the group.
group'
    :: Group
group'
  = Group'{_gFilterExpression = Nothing,
           _gGroupARN = Nothing, _gGroupName = Nothing}

-- | The filter expression defining the parameters to include traces.
gFilterExpression :: Lens' Group (Maybe Text)
gFilterExpression = lens _gFilterExpression (\ s a -> s{_gFilterExpression = a})

-- | The ARN of the group generated based on the GroupName.
gGroupARN :: Lens' Group (Maybe Text)
gGroupARN = lens _gGroupARN (\ s a -> s{_gGroupARN = a})

-- | The unique case-sensitive name of the group.
gGroupName :: Lens' Group (Maybe Text)
gGroupName = lens _gGroupName (\ s a -> s{_gGroupName = a})

instance FromJSON Group where
        parseJSON
          = withObject "Group"
              (\ x ->
                 Group' <$>
                   (x .:? "FilterExpression") <*> (x .:? "GroupARN") <*>
                     (x .:? "GroupName"))

instance Hashable Group where

instance NFData Group where
