{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageDetails
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.PackageDetails where

import Network.AWS.ElasticSearch.Types.ErrorDetails
import Network.AWS.ElasticSearch.Types.PackageStatus
import Network.AWS.ElasticSearch.Types.PackageType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Basic information about a package.
--
--
--
-- /See:/ 'packageDetails' smart constructor.
data PackageDetails = PackageDetails'{_pdPackageId ::
                                      !(Maybe Text),
                                      _pdPackageType :: !(Maybe PackageType),
                                      _pdCreatedAt :: !(Maybe POSIX),
                                      _pdPackageName :: !(Maybe Text),
                                      _pdPackageStatus ::
                                      !(Maybe PackageStatus),
                                      _pdPackageDescription :: !(Maybe Text),
                                      _pdErrorDetails :: !(Maybe ErrorDetails)}
                        deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PackageDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdPackageId' - Internal ID of the package.
--
-- * 'pdPackageType' - Currently supports only TXT-DICTIONARY.
--
-- * 'pdCreatedAt' - Timestamp which tells creation date of the package.
--
-- * 'pdPackageName' - User specified name of the package.
--
-- * 'pdPackageStatus' - Current state of the package. Values are COPYING/COPY_FAILED/AVAILABLE/DELETING/DELETE_FAILED
--
-- * 'pdPackageDescription' - User-specified description of the package.
--
-- * 'pdErrorDetails' - Additional information if the package is in an error state. Null otherwise.
packageDetails
    :: PackageDetails
packageDetails
  = PackageDetails'{_pdPackageId = Nothing,
                    _pdPackageType = Nothing, _pdCreatedAt = Nothing,
                    _pdPackageName = Nothing, _pdPackageStatus = Nothing,
                    _pdPackageDescription = Nothing,
                    _pdErrorDetails = Nothing}

-- | Internal ID of the package.
pdPackageId :: Lens' PackageDetails (Maybe Text)
pdPackageId = lens _pdPackageId (\ s a -> s{_pdPackageId = a})

-- | Currently supports only TXT-DICTIONARY.
pdPackageType :: Lens' PackageDetails (Maybe PackageType)
pdPackageType = lens _pdPackageType (\ s a -> s{_pdPackageType = a})

-- | Timestamp which tells creation date of the package.
pdCreatedAt :: Lens' PackageDetails (Maybe UTCTime)
pdCreatedAt = lens _pdCreatedAt (\ s a -> s{_pdCreatedAt = a}) . mapping _Time

-- | User specified name of the package.
pdPackageName :: Lens' PackageDetails (Maybe Text)
pdPackageName = lens _pdPackageName (\ s a -> s{_pdPackageName = a})

-- | Current state of the package. Values are COPYING/COPY_FAILED/AVAILABLE/DELETING/DELETE_FAILED
pdPackageStatus :: Lens' PackageDetails (Maybe PackageStatus)
pdPackageStatus = lens _pdPackageStatus (\ s a -> s{_pdPackageStatus = a})

-- | User-specified description of the package.
pdPackageDescription :: Lens' PackageDetails (Maybe Text)
pdPackageDescription = lens _pdPackageDescription (\ s a -> s{_pdPackageDescription = a})

-- | Additional information if the package is in an error state. Null otherwise.
pdErrorDetails :: Lens' PackageDetails (Maybe ErrorDetails)
pdErrorDetails = lens _pdErrorDetails (\ s a -> s{_pdErrorDetails = a})

instance FromJSON PackageDetails where
        parseJSON
          = withObject "PackageDetails"
              (\ x ->
                 PackageDetails' <$>
                   (x .:? "PackageID") <*> (x .:? "PackageType") <*>
                     (x .:? "CreatedAt")
                     <*> (x .:? "PackageName")
                     <*> (x .:? "PackageStatus")
                     <*> (x .:? "PackageDescription")
                     <*> (x .:? "ErrorDetails"))

instance Hashable PackageDetails where

instance NFData PackageDetails where
