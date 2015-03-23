module Collections
    ( Collections (..)
    , buildCollections
    , nextUrl
    , prevUrl
    ) where

import           Hakyll
import           Control.Applicative (liftA2)
import           Data.Ord (comparing)
import           Data.List (elemIndex, insertBy)
import           Control.Monad (foldM)
import qualified Data.Map as M
import qualified Data.Set as S

data Collections = Collections
    { collMap        :: [(String, [Identifier])]
    , collMakeId     :: String -> Identifier
    , collDependency :: Dependency
    } deriving (Show)

getCollection :: MonadMetadata m => Identifier -> m String
getCollection identifier = do
    metadata <- getMetadata identifier
    return $ maybe "" id $ M.lookup "collection" metadata

getCollectionAndPos :: MonadMetadata m => Identifier -> m (Maybe (Int,String))
getCollectionAndPos identifier = do
    metadata <- getMetadata identifier
    return $ do
        coll <- M.lookup "collection" metadata
        pos  <- M.lookup "part" metadata
        return (read pos :: Int, coll)

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


-- this can definitely be refactored
insertCollEntryInOrder :: (Ord k, Ord a)
                       => k -> (a,b)
                       -> M.Map k [(a,b)] -> M.Map k [(a,b)]
insertCollEntryInOrder key val oldMap = maybe (M.insert key [val] oldMap) id $ do
    list <- M.lookup key oldMap
    let newList = insertBy (comparing fst) val list
    return $ M.insert key newList oldMap

buildCollections :: MonadMetadata m
                 => Pattern
                 -> (String -> Identifier)
                 -> m Collections
buildCollections = buildCollWith getCollectionAndPos

offsetUrl :: MonadMetadata m => (Int -> Int) -> Collections -> Item a -> m FilePath
offsetUrl f colls i = do
    next <- offsetInCollection colls (itemIdentifier i) f
    return $ maybe "" toFilePath next

nextUrl, prevUrl :: MonadMetadata m => Collections -> Item a -> m FilePath
nextUrl = offsetUrl (+1)
prevUrl = offsetUrl (+(-1))

offsetInCollection :: MonadMetadata m
                 => Collections
                 -> Identifier
                 -> (Int -> Int)
                 -> m (Maybe Identifier)
offsetInCollection colls identifier f = do
    collection <- getCollection identifier
    let postsInCollection = M.lookup collection $ M.fromList (collMap colls)
    let next = do list  <- postsInCollection
                  index <- elemIndex identifier list
                  list `indexList` (f index)
    return next

indexList :: [a] -> Int -> Maybe a
indexList xs n | n <  0         = Nothing
               | n >= length xs = Nothing
               | otherwise      = Just (xs !! n)
