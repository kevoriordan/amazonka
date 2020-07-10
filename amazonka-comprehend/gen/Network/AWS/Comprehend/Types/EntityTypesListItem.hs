{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityTypesListItem
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.EntityTypesListItem where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an individual item on a list of entity types.
--
--
--
-- /See:/ 'entityTypesListItem' smart constructor.
newtype EntityTypesListItem = EntityTypesListItem'{_etliType
                                                   :: Text}
                                deriving (Eq, Read, Show, Data, Typeable,
                                          Generic)

-- | Creates a value of 'EntityTypesListItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'etliType' - Entity type of an item on an entity type list.
entityTypesListItem
    :: Text -- ^ 'etliType'
    -> EntityTypesListItem
entityTypesListItem pType_
  = EntityTypesListItem'{_etliType = pType_}

-- | Entity type of an item on an entity type list.
etliType :: Lens' EntityTypesListItem Text
etliType = lens _etliType (\ s a -> s{_etliType = a})

instance FromJSON EntityTypesListItem where
        parseJSON
          = withObject "EntityTypesListItem"
              (\ x -> EntityTypesListItem' <$> (x .: "Type"))

instance Hashable EntityTypesListItem where

instance NFData EntityTypesListItem where

instance ToJSON EntityTypesListItem where
        toJSON EntityTypesListItem'{..}
          = object (catMaybes [Just ("Type" .= _etliType)])
