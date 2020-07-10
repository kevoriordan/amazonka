{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ContainerProperties
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.ContainerProperties where

import Network.AWS.Batch.Types.KeyValuePair
import Network.AWS.Batch.Types.LinuxParameters
import Network.AWS.Batch.Types.MountPoint
import Network.AWS.Batch.Types.ResourceRequirement
import Network.AWS.Batch.Types.Ulimit
import Network.AWS.Batch.Types.Volume
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Container properties are used in job definitions to describe the container that is launched as part of a job.
--
--
--
-- /See:/ 'containerProperties' smart constructor.
data ContainerProperties = ContainerProperties'{_cpImage
                                                :: !(Maybe Text),
                                                _cpCommand :: !(Maybe [Text]),
                                                _cpEnvironment ::
                                                !(Maybe [KeyValuePair]),
                                                _cpUlimits :: !(Maybe [Ulimit]),
                                                _cpPrivileged :: !(Maybe Bool),
                                                _cpJobRoleARN :: !(Maybe Text),
                                                _cpResourceRequirements ::
                                                !(Maybe [ResourceRequirement]),
                                                _cpInstanceType ::
                                                !(Maybe Text),
                                                _cpMemory :: !(Maybe Int),
                                                _cpUser :: !(Maybe Text),
                                                _cpLinuxParameters ::
                                                !(Maybe LinuxParameters),
                                                _cpMountPoints ::
                                                !(Maybe [MountPoint]),
                                                _cpVcpus :: !(Maybe Int),
                                                _cpReadonlyRootFilesystem ::
                                                !(Maybe Bool),
                                                _cpVolumes :: !(Maybe [Volume])}
                             deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpImage' - The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with @/repository-url/ //image/ :/tag/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .     * Images in Amazon ECR repositories use the full registry and repository URI (for example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@ ).     * Images in official repositories on Docker Hub use a single name (for example, @ubuntu@ or @mongo@ ).     * Images in other repositories on Docker Hub are qualified with an organization name (for example, @amazon/amazon-ecs-agent@ ).     * Images in other online repositories are qualified further by a domain name (for example, @quay.io/assemblyline/ubuntu@ ).
--
-- * 'cpCommand' - The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> .
--
-- * 'cpEnvironment' - The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/ docker run> . /Important:/ We do not recommend using plaintext environment variables for sensitive information, such as credential data.
--
-- * 'cpUlimits' - A list of @ulimits@ to set in the container. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cpPrivileged' - When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cpJobRoleARN' - The Amazon Resource Name (ARN) of the IAM role that the container can assume for AWS permissions.
--
-- * 'cpResourceRequirements' - The type and amount of a resource to assign to a container. Currently, the only supported resource is @GPU@ .
--
-- * 'cpInstanceType' - The instance type to use for a multi-node parallel job. Currently all node groups in a multi-node parallel job must use the same instance type. This parameter is not valid for single-node container jobs.
--
-- * 'cpMemory' - The hard limit (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/ docker run> . You must specify at least 4 MiB of memory for a job.
--
-- * 'cpUser' - The user name to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cpLinuxParameters' - Linux-specific modifications that are applied to the container, such as details for device mappings.
--
-- * 'cpMountPoints' - The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
--
-- * 'cpVcpus' - The number of vCPUs reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Each vCPU is equivalent to 1,024 CPU shares. You must specify at least one vCPU.
--
-- * 'cpReadonlyRootFilesystem' - When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--read-only@ option to @docker run@ .
--
-- * 'cpVolumes' - A list of data volumes used in a job.
containerProperties
    :: ContainerProperties
containerProperties
  = ContainerProperties'{_cpImage = Nothing,
                         _cpCommand = Nothing, _cpEnvironment = Nothing,
                         _cpUlimits = Nothing, _cpPrivileged = Nothing,
                         _cpJobRoleARN = Nothing,
                         _cpResourceRequirements = Nothing,
                         _cpInstanceType = Nothing, _cpMemory = Nothing,
                         _cpUser = Nothing, _cpLinuxParameters = Nothing,
                         _cpMountPoints = Nothing, _cpVcpus = Nothing,
                         _cpReadonlyRootFilesystem = Nothing,
                         _cpVolumes = Nothing}

-- | The image used to start a container. This string is passed directly to the Docker daemon. Images in the Docker Hub registry are available by default. Other repositories are specified with @/repository-url/ //image/ :/tag/ @ . Up to 255 letters (uppercase and lowercase), numbers, hyphens, underscores, colons, periods, forward slashes, and number signs are allowed. This parameter maps to @Image@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @IMAGE@ parameter of <https://docs.docker.com/engine/reference/run/ docker run> .     * Images in Amazon ECR repositories use the full registry and repository URI (for example, @012345678910.dkr.ecr.<region-name>.amazonaws.com/<repository-name>@ ).     * Images in official repositories on Docker Hub use a single name (for example, @ubuntu@ or @mongo@ ).     * Images in other repositories on Docker Hub are qualified with an organization name (for example, @amazon/amazon-ecs-agent@ ).     * Images in other online repositories are qualified further by a domain name (for example, @quay.io/assemblyline/ubuntu@ ).
cpImage :: Lens' ContainerProperties (Maybe Text)
cpImage = lens _cpImage (\ s a -> s{_cpImage = a})

-- | The command that is passed to the container. This parameter maps to @Cmd@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @COMMAND@ parameter to <https://docs.docker.com/engine/reference/run/ docker run> . For more information, see <https://docs.docker.com/engine/reference/builder/#cmd https://docs.docker.com/engine/reference/builder/#cmd> .
cpCommand :: Lens' ContainerProperties [Text]
cpCommand = lens _cpCommand (\ s a -> s{_cpCommand = a}) . _Default . _Coerce

-- | The environment variables to pass to a container. This parameter maps to @Env@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--env@ option to <https://docs.docker.com/engine/reference/run/ docker run> . /Important:/ We do not recommend using plaintext environment variables for sensitive information, such as credential data.
cpEnvironment :: Lens' ContainerProperties [KeyValuePair]
cpEnvironment = lens _cpEnvironment (\ s a -> s{_cpEnvironment = a}) . _Default . _Coerce

-- | A list of @ulimits@ to set in the container. This parameter maps to @Ulimits@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--ulimit@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cpUlimits :: Lens' ContainerProperties [Ulimit]
cpUlimits = lens _cpUlimits (\ s a -> s{_cpUlimits = a}) . _Default . _Coerce

-- | When this parameter is true, the container is given elevated privileges on the host container instance (similar to the @root@ user). This parameter maps to @Privileged@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--privileged@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cpPrivileged :: Lens' ContainerProperties (Maybe Bool)
cpPrivileged = lens _cpPrivileged (\ s a -> s{_cpPrivileged = a})

-- | The Amazon Resource Name (ARN) of the IAM role that the container can assume for AWS permissions.
cpJobRoleARN :: Lens' ContainerProperties (Maybe Text)
cpJobRoleARN = lens _cpJobRoleARN (\ s a -> s{_cpJobRoleARN = a})

-- | The type and amount of a resource to assign to a container. Currently, the only supported resource is @GPU@ .
cpResourceRequirements :: Lens' ContainerProperties [ResourceRequirement]
cpResourceRequirements = lens _cpResourceRequirements (\ s a -> s{_cpResourceRequirements = a}) . _Default . _Coerce

-- | The instance type to use for a multi-node parallel job. Currently all node groups in a multi-node parallel job must use the same instance type. This parameter is not valid for single-node container jobs.
cpInstanceType :: Lens' ContainerProperties (Maybe Text)
cpInstanceType = lens _cpInstanceType (\ s a -> s{_cpInstanceType = a})

-- | The hard limit (in MiB) of memory to present to the container. If your container attempts to exceed the memory specified here, the container is killed. This parameter maps to @Memory@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--memory@ option to <https://docs.docker.com/engine/reference/run/ docker run> . You must specify at least 4 MiB of memory for a job.
cpMemory :: Lens' ContainerProperties (Maybe Int)
cpMemory = lens _cpMemory (\ s a -> s{_cpMemory = a})

-- | The user name to use inside the container. This parameter maps to @User@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--user@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cpUser :: Lens' ContainerProperties (Maybe Text)
cpUser = lens _cpUser (\ s a -> s{_cpUser = a})

-- | Linux-specific modifications that are applied to the container, such as details for device mappings.
cpLinuxParameters :: Lens' ContainerProperties (Maybe LinuxParameters)
cpLinuxParameters = lens _cpLinuxParameters (\ s a -> s{_cpLinuxParameters = a})

-- | The mount points for data volumes in your container. This parameter maps to @Volumes@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--volume@ option to <https://docs.docker.com/engine/reference/run/ docker run> .
cpMountPoints :: Lens' ContainerProperties [MountPoint]
cpMountPoints = lens _cpMountPoints (\ s a -> s{_cpMountPoints = a}) . _Default . _Coerce

-- | The number of vCPUs reserved for the container. This parameter maps to @CpuShares@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--cpu-shares@ option to <https://docs.docker.com/engine/reference/run/ docker run> . Each vCPU is equivalent to 1,024 CPU shares. You must specify at least one vCPU.
cpVcpus :: Lens' ContainerProperties (Maybe Int)
cpVcpus = lens _cpVcpus (\ s a -> s{_cpVcpus = a})

-- | When this parameter is true, the container is given read-only access to its root file system. This parameter maps to @ReadonlyRootfs@ in the <https://docs.docker.com/engine/api/v1.23/#create-a-container Create a container> section of the <https://docs.docker.com/engine/api/v1.23/ Docker Remote API> and the @--read-only@ option to @docker run@ .
cpReadonlyRootFilesystem :: Lens' ContainerProperties (Maybe Bool)
cpReadonlyRootFilesystem = lens _cpReadonlyRootFilesystem (\ s a -> s{_cpReadonlyRootFilesystem = a})

-- | A list of data volumes used in a job.
cpVolumes :: Lens' ContainerProperties [Volume]
cpVolumes = lens _cpVolumes (\ s a -> s{_cpVolumes = a}) . _Default . _Coerce

instance FromJSON ContainerProperties where
        parseJSON
          = withObject "ContainerProperties"
              (\ x ->
                 ContainerProperties' <$>
                   (x .:? "image") <*> (x .:? "command" .!= mempty) <*>
                     (x .:? "environment" .!= mempty)
                     <*> (x .:? "ulimits" .!= mempty)
                     <*> (x .:? "privileged")
                     <*> (x .:? "jobRoleArn")
                     <*> (x .:? "resourceRequirements" .!= mempty)
                     <*> (x .:? "instanceType")
                     <*> (x .:? "memory")
                     <*> (x .:? "user")
                     <*> (x .:? "linuxParameters")
                     <*> (x .:? "mountPoints" .!= mempty)
                     <*> (x .:? "vcpus")
                     <*> (x .:? "readonlyRootFilesystem")
                     <*> (x .:? "volumes" .!= mempty))

instance Hashable ContainerProperties where

instance NFData ContainerProperties where

instance ToJSON ContainerProperties where
        toJSON ContainerProperties'{..}
          = object
              (catMaybes
                 [("image" .=) <$> _cpImage,
                  ("command" .=) <$> _cpCommand,
                  ("environment" .=) <$> _cpEnvironment,
                  ("ulimits" .=) <$> _cpUlimits,
                  ("privileged" .=) <$> _cpPrivileged,
                  ("jobRoleArn" .=) <$> _cpJobRoleARN,
                  ("resourceRequirements" .=) <$>
                    _cpResourceRequirements,
                  ("instanceType" .=) <$> _cpInstanceType,
                  ("memory" .=) <$> _cpMemory, ("user" .=) <$> _cpUser,
                  ("linuxParameters" .=) <$> _cpLinuxParameters,
                  ("mountPoints" .=) <$> _cpMountPoints,
                  ("vcpus" .=) <$> _cpVcpus,
                  ("readonlyRootFilesystem" .=) <$>
                    _cpReadonlyRootFilesystem,
                  ("volumes" .=) <$> _cpVolumes])
