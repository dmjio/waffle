{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeApplications           #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Waffle
-- Copyright   :  (C) 2018-2019 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <david@urbint.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Waffle
 ( -- * Waffle API
   -- $intro
   deleteColumn
 ) where

import           Data.Aeson
import           Data.Char
import           Data.Proxy
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Generics
import           Network.HTTP.Client hiding (Proxy)
import           Servant.API
import           Servant.Client

{-- $intro
The Waffle API is organized around REST. Our API has predictable, resource-oriented URLs, and uses HTTP response codes to indicate API errors. We use built-in HTTP features, like HTTP authentication and HTTP verbs, which are understood by off-the-shelf HTTP clients. We support cross-origin resource sharing, allowing you to interact securely with our API from a client-side web application (though you should never expose your secret API key in any public website's client-side code). JSON is returned by all API responses, including errors.
--}

-- | `User` endpoint
-- @
--  {
--    "_id": "53244220b728ed1d0009af87",
--    "account": "56254220b728ed1d0009af89",
--    "github": {
--      "login": "waffle-iron",
--      "email": "iron@waffle.io",
--      "name": "Waffle Iron",
--      "avatar_url": "https://avatars.githubusercontent.com/u/4775781?v=3"
--    }
--  }
-- @
data User
  = User
  { user_id :: T.Text
  -- ^ The unique identifier for this user.
  , userAccount :: T.Text
  -- ^ The id of this user's account.
  , userGithub :: Github
  -- ^ The GitHub profile information for the user.
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 4
  }

data Github
  = Github
  { githubLogin :: T.Text
  -- ^ Github login credentials
  , githubEmail :: T.Text
  -- ^ Github email address
  , githubName :: T.Text
  -- ^ Github user name
  , githubAvatarUrl :: T.Text
  -- ^ Github avatar url
  } deriving (Show, Eq, Generic)

instance FromJSON Github where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 6
  }

-- | Sending a GET to /user fetches the current user.
type UserAPI
  = "user"
  :> Get '[JSON] User

data Project
  = Project
  {
  } deriving (Show, Eq, Generic)

-- | Sending a GET to /user fetches the current user.
type UserProjectsAPI
  = "user"
  :> "projects"
  :> QueryParam "access_token" AccessToken
  :> Get '[JSON] [Project]

-- | Project Id
newtype ProjectId = ProjectId T.Text
  deriving (Show, Eq, FromJSON, ToJSON, ToHttpApiData)

-- | The GitHub repo owner
newtype Owner = Owner T.Text
  deriving (Show, Eq, FromJSON, ToJSON, ToHttpApiData)

-- | The name of the GitHub repo
newtype Repo = Repo T.Text
  deriving (Show, Eq, FromJSON, ToJSON, ToHttpApiData)

-- | Sending a GET to /user fetches the current user.
type ProjectsByIdAPI
  = "projects"
  :> Capture "id" ProjectId
  :> QueryParam "access_token" AccessToken
  :> Get '[JSON] [Project]

-- | Sending a GET to /user fetches the current user.
type ProjectsOwnerRepoAPI
  = "projects"
  :> Capture "owner" Owner
  :> Capture "repo" Repo
  :> QueryParam "access_token" AccessToken
  :> Get '[JSON] [Project]

data ProjectName
  = ProjectName
  { projectName :: T.Text
  -- ^ Send the name of the repo as json:
  } deriving (Show, Eq, Generic)

-- | Project name
instance ToJSON ProjectName where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7
  }

newtype AccessToken
  = AccessToken T.Text
  deriving (Show, Eq, FromJSON, ToJSON, ToHttpApiData)

-- | Create a new project from a repo in GitHub.
type CreateProjectsAPI
  = "projects"
  :> ReqBody '[JSON] ProjectName
  :> QueryParam "access_token" AccessToken
  :> Post '[JSON] NoContent

-- | Deletes a project using id. This will not delete any associated GitHub repositories or issues. You will however, lose any Waffle specific data like issue sizing and column configuration.
type DeleteProjectsAPI
  = "projects"
  :> Capture "id" ProjectId
  :> QueryParam "access_token" AccessToken
  :> Delete '[JSON] NoContent

-- | A source is a GitHub repository attached to a project (board). A POST will add a GitHub repo to your project.
data Sources
  = Sources
  { sourceProvider :: T.Text
  , sourcePrivate :: Bool
  , sourceRepoPath :: T.Text
  , sourceType :: T.Text
  } deriving (Show, Eq, Generic)

instance ToJSON Sources where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = \s ->
        case drop 7 s of
          x:xs -> toLower x : xs
  }

-- | Project sources API
type ProjectSourcesAPIp
  = "projects"
  :> Capture "id" ProjectId
  :> ReqBody '[JSON] Sources
  :> Capture "id" ProviderId
  :> QueryParam "access_token" AccessToken
  :> Post '[JSON] Value

-- | Project sources API
type EditSourceAPI
  = "projects"
  :> Capture "id" ProjectId
  :> "sources"
  :> Capture "id" ProviderId
  :> QueryParam "access_token" AccessToken
  :> Put '[JSON] Value

-- | provider field on a `Source`
newtype ProviderId
  = ProviderId T.Text
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Delete sources API
-- This is how you delete a source from a project. This will not delete any associated GitHub repositories or issues.
-- You're not able to delete the primary (first) source.
type DeleteSourceAPI
  = "projects"
  :> Capture "id" ProjectId
  :> "sources"
  :> Capture "id" ProviderId
  :> QueryParam "access_token" AccessToken
  :> Put '[JSON] Value

-------- Cards API

-- | A card object represents an issue on a Waffle board. Why do we call it a card? Well, it's a bit more than just the GitHub Issue. Waffle keeps track of the card's size, for example. The card keeps everything about the actual GitHub Issue in the githubMetadata property, but we tack on a few other things that's Waffle specific.
data Card
  = Card
  {
  } deriving (Show, Eq, Generic)

-- | This will show you all of the cards in the project. This will include cards from any secondary source, not just the primary source.
type CardsAPI
  = Capture "owner" Owner
  :> Capture "repo" Repo
  :> "cards"
  :> QueryParam "access_token" AccessToken
  :> Get '[JSON] [Card]

-- | This will show you all of the cards in the project. This will include cards from any secondary source, not just the primary source.
type ProjectCardAPI
  = "projects"
  :> Capture "id" ProjectId
  :> "cards"
  :> QueryParam "access_token" AccessToken
  :> Get '[JSON] Card

-- | This is how you can update information on a card. In this example, the title of the card was updated. You'll need to send these parameters as JSON:
data GithubMetadata
  = GithubMetadata
  {
  } deriving (Show, Eq)

-- | Card information update
type UpdateCardAPI
  = "projects"
  :> Capture "id" ProjectId
  :> "cards"
  :> QueryParam "access_token" AccessToken
  :> Put '[JSON] Card

data Archive
  = Archive
  { archiveId :: T.Text
  , archiveArchived :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON Archive where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = camelTo2 '_' . drop 7
  }

-- | Card information update
type ArchiveIssueAPI
  = "projects"
  :> Capture "id" ProjectId
  :> "cards"
  :> ReqBody '[JSON] [Archive]
  :> QueryParam "access_token" AccessToken
  :> Patch '[JSON] Card

-- | Create Card API
type CreateCardAPI
   = Capture "owner" Owner
  :> Capture "repo" Repo
  :> ReqBody '[JSON] GithubMetadata
  :> "cards"
  :> QueryParam "access_token" AccessToken
  :> Post '[JSON] Card

-------- Column --------

data Column
  = Column
  {
  } deriving (Show, Eq, Generic)

-- | Get Columns
type GetColumnsAPI
   = Capture "owner" Owner
  :> Capture "repo" Repo
  :> "columns"
  :> QueryParam "access_token" AccessToken
  :> Post '[JSON] [Column]

-- | Update columns
type UpdateColumnsAPI
   = "projects"
  :> Capture "id" ProjectId
  :> "columns"
  :> ReqBody '[JSON] [Column]
  :> QueryParam "access_token" AccessToken
  :> Put '[JSON] [Column]

-- | This is how you update information on a column.
type UpdateColumnAPI
   = Capture "owner" Owner
  :> Capture "repo" Repo
  :> "columns"
  :> Capture "id"
  :> ReqBody '[JSON] Object
  :> QueryParam "access_token" AccessToken
  :> Put '[JSON] Column

-- | Deletes a column for a project
type DeleteColumnAPI
   = Capture "owner" Owner
  :> Capture "repo" Repo
  :> "columns"
  :> Capture "id" ProjectId
  :> QueryParam "access_token" AccessToken
  :> Put '[JSON] NoContent

deleteColumn
  :: Owner
  -> Repo
  -> ProjectId
  -> AccessToken
  -> Manager
  -> IO (Either ServantError ())
deleteColumn
  owner
  repo
  projectId
  token
  mgr = do
    result <- runClientM deleteColumn (toClientEnv mgr)
    pure $ case result of
      Left e -> Left e
      Right NoContent -> Right ()
  where
    deleteColumn :: ClientM NoContent
    deleteColumn =
      client (Proxy @ DeleteColumnAPI) owner repo projectId (Just token)

toClientEnv
  :: Manager
  -> ClientEnv
toClientEnv mgr = ClientEnv mgr waffleUrl Nothing

waffleUrl :: BaseUrl
waffleUrl = BaseUrl Https "api.waffle.io" 443 mempty
