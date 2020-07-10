{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Filter
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.Filter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies the name and value of a source filter object used to limit the number and type of records transferred from your source to your target.
--
--
--
-- /See:/ 'filter'' smart constructor.
data Filter = Filter'{_fName :: !Text,
                      _fValues :: ![Text]}
                deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fName' - The name of the filter.
--
-- * 'fValues' - The filter value.
filter'
    :: Text -- ^ 'fName'
    -> Filter
filter' pName_
  = Filter'{_fName = pName_, _fValues = mempty}

-- | The name of the filter.
fName :: Lens' Filter Text
fName = lens _fName (\ s a -> s{_fName = a})

-- | The filter value.
fValues :: Lens' Filter [Text]
fValues = lens _fValues (\ s a -> s{_fValues = a}) . _Coerce

instance Hashable Filter where

instance NFData Filter where

instance ToJSON Filter where
        toJSON Filter'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _fName),
                  Just ("Values" .= _fValues)])
