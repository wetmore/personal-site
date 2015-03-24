module Collections
    ( Collections (..)
    , buildCollections
    , collectionContext
    ) where

import           Hakyll
import           Control.Applicative (liftA2)
import           Data.Ord (comparing)
import           Data.List (elemIndex, insertBy)
import           Control.Monad (foldM)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mconcat)
import qualified Data.Map as M
import qualified Data.Set as S

data Collections = Collections
    { collMap        :: [(String, [Identifier])]
    , collMakeId     :: String -> Identifier
    , collDependency :: Dependency
    } deriving (Show)

------ Building the collections map

buildCollWith :: MonadMetadata m
              => (Identifier -> m (Maybe (Int, String)))
              -> Pattern
              -> (String -> Identifier)
              -> m Collections
buildCollWith f pattern makeId = do
    ids     <- getMatches pattern
    collMap <- foldM addColl M.empty ids
    let set'    = S.fromList ids
    let mapList = map (liftA2 (,) fst (map snd . snd)) $ M.toList collMap
    return $ Collections mapList makeId (PatternDependency pattern set')
  where
    addColl collMap id' = do
        maybeCollection <- f id'
        let add (pos, coll) = insertCollEntryInOrder coll (pos, id') collMap
        return $ maybe collMap add maybeCollection

insertCollEntryInOrder :: (Ord k, Ord a)
                       => k -> (a,b)
                       -> M.Map k [(a,b)] -> M.Map k [(a,b)]
insertCollEntryInOrder key val oldMap = M.insert key newVal oldMap
  where
    newVal = maybe [val] id $ do
      list <- M.lookup key oldMap
      return $ insertBy (comparing fst) val list

buildCollections :: MonadMetadata m
                 => Pattern
                 -> (String -> Identifier)
                 -> m Collections
buildCollections = buildCollWith getCollectionAndPos

-- swap order of pos and collection to fit name
getCollectionAndPos :: MonadMetadata m => Identifier -> m (Maybe (Int,String))
getCollectionAndPos identifier = do
    metadata <- getMetadata identifier
    return $ do
        coll <- M.lookup "collection" metadata
        pos  <- M.lookup "part" metadata
        return (read pos :: Int, coll)

--------------

offsetUrl :: (Int -> Int) -- offset function
          -> Collections
          -> Item a
          -> Compiler FilePath
offsetUrl f colls i = 
    (offsetInCollection colls (itemIdentifier i) f) >>= url
      where
        url Nothing    = fail $ "No next/prev page"
        url (Just id') = (getRoute id') >>= process
        
        process Nothing   = fail $ "No URL for that page"
        process (Just fp) = return $ toUrl fp

offsetInCollection :: MonadMetadata m
                 => Collections
                 -> Identifier
                 -> (Int -> Int)
                 -> m (Maybe Identifier)
offsetInCollection colls identifier f = do
    maybeCollection <- getCollectionAndPos identifier
    return $ do
      (pos, collection) <- maybeCollection
      list <- M.lookup collection $ M.fromList (collMap colls)
      list `indexList` (f $ pos - 1) -- Pos starts at 1

indexList :: [a] -> Int -> Maybe a
indexList xs n | n <  0         = Nothing
               | n >= length xs = Nothing
               | otherwise      = Just (xs !! n)

collectionContext :: Collections -> Context String
collectionContext colls = mconcat
  [ field "nextInCollection" $ offsetUrl (+1)    colls
  , field "prevInCollection" $ offsetUrl (+(-1)) colls
  ]
