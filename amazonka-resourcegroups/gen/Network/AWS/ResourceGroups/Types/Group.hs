{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.Group
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ResourceGroups.Types.Group where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A resource group.
--
--
--
-- /See:/ 'group'' smart constructor.
data Group = Group'{_gDescription :: !(Maybe Text),
                    _gGroupARN :: !Text, _gName :: !Text}
               deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gDescription' - The description of the resource group.
--
-- * 'gGroupARN' - The ARN of a resource group.
--
-- * 'gName' - The name of a resource group.
group'
    :: Text -- ^ 'gGroupARN'
    -> Text -- ^ 'gName'
    -> Group
group' pGroupARN_ pName_
  = Group'{_gDescription = Nothing,
           _gGroupARN = pGroupARN_, _gName = pName_}

-- | The description of the resource group.
gDescription :: Lens' Group (Maybe Text)
gDescription = lens _gDescription (\ s a -> s{_gDescription = a})

-- | The ARN of a resource group.
gGroupARN :: Lens' Group Text
gGroupARN = lens _gGroupARN (\ s a -> s{_gGroupARN = a})

-- | The name of a resource group.
gName :: Lens' Group Text
gName = lens _gName (\ s a -> s{_gName = a})

instance FromJSON Group where
        parseJSON
          = withObject "Group"
              (\ x ->
                 Group' <$>
                   (x .:? "Description") <*> (x .: "GroupArn") <*>
                     (x .: "Name"))

instance Hashable Group where

instance NFData Group where
