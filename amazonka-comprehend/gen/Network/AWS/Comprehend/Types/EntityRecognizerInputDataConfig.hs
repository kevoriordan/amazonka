{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig where

import Network.AWS.Comprehend.Types.EntityRecognizerAnnotations
import Network.AWS.Comprehend.Types.EntityRecognizerDocuments
import Network.AWS.Comprehend.Types.EntityRecognizerEntityList
import Network.AWS.Comprehend.Types.EntityTypesListItem
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the format and location of the input data.
--
--
--
-- /See:/ 'entityRecognizerInputDataConfig' smart constructor.
data EntityRecognizerInputDataConfig = EntityRecognizerInputDataConfig'{_eridcAnnotations
                                                                        ::
                                                                        !(Maybe
                                                                            EntityRecognizerAnnotations),
                                                                        _eridcEntityList
                                                                        ::
                                                                        !(Maybe
                                                                            EntityRecognizerEntityList),
                                                                        _eridcEntityTypes
                                                                        ::
                                                                        ![EntityTypesListItem],
                                                                        _eridcDocuments
                                                                        ::
                                                                        !EntityRecognizerDocuments}
                                         deriving (Eq, Read, Show, Data,
                                                   Typeable, Generic)

-- | Creates a value of 'EntityRecognizerInputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eridcAnnotations' - S3 location of the annotations file for an entity recognizer.
--
-- * 'eridcEntityList' - S3 location of the entity list for an entity recognizer.
--
-- * 'eridcEntityTypes' - The entity types in the input data for an entity recognizer. A maximum of 12 entity types can be used at one time to train an entity recognizer.
--
-- * 'eridcDocuments' - S3 location of the documents folder for an entity recognizer
entityRecognizerInputDataConfig
    :: EntityRecognizerDocuments -- ^ 'eridcDocuments'
    -> EntityRecognizerInputDataConfig
entityRecognizerInputDataConfig pDocuments_
  = EntityRecognizerInputDataConfig'{_eridcAnnotations
                                       = Nothing,
                                     _eridcEntityList = Nothing,
                                     _eridcEntityTypes = mempty,
                                     _eridcDocuments = pDocuments_}

-- | S3 location of the annotations file for an entity recognizer.
eridcAnnotations :: Lens' EntityRecognizerInputDataConfig (Maybe EntityRecognizerAnnotations)
eridcAnnotations = lens _eridcAnnotations (\ s a -> s{_eridcAnnotations = a})

-- | S3 location of the entity list for an entity recognizer.
eridcEntityList :: Lens' EntityRecognizerInputDataConfig (Maybe EntityRecognizerEntityList)
eridcEntityList = lens _eridcEntityList (\ s a -> s{_eridcEntityList = a})

-- | The entity types in the input data for an entity recognizer. A maximum of 12 entity types can be used at one time to train an entity recognizer.
eridcEntityTypes :: Lens' EntityRecognizerInputDataConfig [EntityTypesListItem]
eridcEntityTypes = lens _eridcEntityTypes (\ s a -> s{_eridcEntityTypes = a}) . _Coerce

-- | S3 location of the documents folder for an entity recognizer
eridcDocuments :: Lens' EntityRecognizerInputDataConfig EntityRecognizerDocuments
eridcDocuments = lens _eridcDocuments (\ s a -> s{_eridcDocuments = a})

instance FromJSON EntityRecognizerInputDataConfig
         where
        parseJSON
          = withObject "EntityRecognizerInputDataConfig"
              (\ x ->
                 EntityRecognizerInputDataConfig' <$>
                   (x .:? "Annotations") <*> (x .:? "EntityList") <*>
                     (x .:? "EntityTypes" .!= mempty)
                     <*> (x .: "Documents"))

instance Hashable EntityRecognizerInputDataConfig
         where

instance NFData EntityRecognizerInputDataConfig where

instance ToJSON EntityRecognizerInputDataConfig where
        toJSON EntityRecognizerInputDataConfig'{..}
          = object
              (catMaybes
                 [("Annotations" .=) <$> _eridcAnnotations,
                  ("EntityList" .=) <$> _eridcEntityList,
                  Just ("EntityTypes" .= _eridcEntityTypes),
                  Just ("Documents" .= _eridcDocuments)])
