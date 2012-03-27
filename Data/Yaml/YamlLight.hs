-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Yaml.YamlLight
-- Copyright   :  Michael Ilseman (c) 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  michael <dot> ilseman <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- A light-weight wrapper with utility functions around HsSyck

{-# LANGUAGE OverloadedStrings #-}

module Data.Yaml.YamlLight
  ( -- * YamlLight data type
    YamlLight(..)
    -- * YamlLight versions of Syck functions
  , parseYaml, parseYamlFile, parseYamlBytes
    -- * YamlLight utility functions
  , fromYamlNode, lookupYL, lookupYLWith
  , combineSequencedMaps, combineMappedSequences, getTerminalsKeys
    -- ** Extractors
  , unSeq, unMap, unStr
  ,  (*!), (|!)
  ) where
  import Control.Applicative
--  import Data.Data
  import Data.List
  import Data.Maybe
  import Control.Arrow
  import qualified Data.Yaml.Syck as Syck
  import qualified Data.Map as Map
  import qualified Data.ByteString as ByteString

  {- | A light-weight, single ADT representation of a yaml document in contrast with what is provided by HsSyck.
       Note that the YMap is an actual Map from
       Data.Map, so behavior with respect to identical keys and ordering of entries will behave as Data.Map
       dictates. This behavior is also in compliance with the Yaml spec. If you currently rely on HsSyck's
       preservation of ordering, you can also consider representing
       such maps as sequences of single entry maps. See the examples of \"Ordered Mappings\" in the Yaml
       spec: <http://www.yaml.org/spec/1.2/spec.html>.
   -}
  data YamlLight = YMap (Map.Map YamlLight YamlLight)
                 | YSeq [YamlLight]
                 | YStr ByteString.ByteString
                 | YNil
    deriving (Show, Ord, Eq)

  convert :: (a -> Syck.YamlNode) -> (a -> YamlLight)
  convert f = fromYamlNode . f

  convertIO :: (a -> IO Syck.YamlNode) -> (a -> IO YamlLight)
  convertIO f yn = fromYamlNode <$> f yn

  -- | Parse a regular Haskell string
  parseYaml :: String -> IO YamlLight
  parseYaml = convertIO Syck.parseYaml

  -- | Given a file name, parse contents of file
  parseYamlFile :: String -> IO YamlLight
  parseYamlFile = convertIO Syck.parseYamlFile

  -- | Parse a ByteString buffer (this is faster)
  parseYamlBytes :: ByteString.ByteString -> IO YamlLight
  parseYamlBytes = convertIO Syck.parseYamlBytes

  -- | Convert a Syck YamlNode to a YamlLight
  fromYamlNode :: Syck.YamlNode -> YamlLight
  fromYamlNode = yamlElemToLight . Syck.n_elem

  yamlElemToLight :: Syck.YamlElem -> YamlLight
  yamlElemToLight (Syck.EMap ms)  = YMap . Map.fromList . map (\(a,b) -> (fromYamlNode a, fromYamlNode b)) $ ms
  yamlElemToLight (Syck.ESeq s)   = YSeq $ map fromYamlNode s
  yamlElemToLight (Syck.EStr buf) = YStr buf
  yamlElemToLight (Syck.ENil)     = YNil

  -- | Lookup the key's corresponding value in a Map. Returns Nothing if the YamlLight is not a map, or if
  -- the key is not found
  lookupYL :: YamlLight -> YamlLight -> Maybe YamlLight
  lookupYL key (YMap m) = Map.lookup key m
  lookupYL _ _          = Nothing

  -- | General form of lookup. Will return the first element that satisfies predicate p, otherwise Nothing
  lookupYLWith :: (YamlLight -> Bool) -> YamlLight -> Maybe YamlLight
  lookupYLWith p (YMap m) = snd <$> (find (p . fst) $ Map.toList m)
  lookupYLWith _ _        = Nothing

  {- | Combine a sequence of YMaps into a list of (key,value) pairs. The ordering of the result preserves the ordering
     of the sequence, but the ordering of the individual maps is as Data.Map handles it.

    Example:

    @
     - key1: val1
       key2: val2
     - key3: val3
    @

    Would become:

     @
     [(key1,val1),(key2,val2),(key3,val3)]
     @

     where key1 and key2 might be arranged differently as Data.Map would
     arrange them. This does not enforce uniqueness of keys across different maps.
     Any items of the sequence that are not maps will not be present in the output list.
     Returns Nothing if not called on a Sequence
   -}
  combineSequencedMaps :: YamlLight -> Maybe [(YamlLight, YamlLight)]
  combineSequencedMaps (YSeq ys) = Just . concatMap Map.assocs . catMaybes $ map unMap ys
  combineSequencedMaps _         = Nothing

  {- | Take a YamlLight that is a YMap of keys to YSeqs, and return a list of (key,elem) pairs, where elem is an element
       of the YSeq under key.

    Example:

    @
     key1: [val1, val2, val3]
     key2: [val4, val5]
    @

    Would become:

     @
     [(key1,val1),(key1,val2),(key1,val3),(key2,val4),(key2,val5)]
     @

     where the precise ordering of the key1 and key2 pairs depends on the ordering of Data.Map.
     Any values of keys that are not sequences will not appear in the output list.
     Returns Nothing if not called on a YMap.
   -}
  combineMappedSequences :: YamlLight -> Maybe [(YamlLight, YamlLight)]
  combineMappedSequences (YMap m) = Just . concatMap flattenTags . removeSndMaybes $ mapThenList unSeq m
  combineMappedSequences _        = Nothing

  mapThenList :: (b -> Maybe [c]) -> Map.Map a b -> [(a, Maybe [c])]
  mapThenList f m = Map.toList $ Map.map f m

  removeSndMaybes :: [(a,Maybe [b])] -> [(a,[b])]
  removeSndMaybes = map (second fromJust) . filter (isJust . snd)

  flattenTags :: (a,[b]) -> [(a,b)]
  flattenTags (a,bs) = map ((,) a) bs


  {- | Create a list of all the terminal YStrs in a YamlLight tree, and couple them with a list of
       all the keys above them.

       Example:

       @
        - key1:
            key1_1:
              - \"str1\"
              - \"str2\"
            key1_2:
              - \"str2\"
              - \"str3\"
        - key2:
            \"str4\"
        - \"str5\"
        @

        Would become:

        @
        [(\"str1\",[key1_1, key1]), (\"str2\", [key1_1, key1]), (\"str2\", [key1_2, key1]), (\"str3\",[key1_2, key1]), (\"str4\",[key2]), (\"str5\",[])
        @
   -}
  getTerminalsKeys :: YamlLight -> [(ByteString.ByteString,[YamlLight])]
  getTerminalsKeys = getTerminalsKeys' []

  getTerminalsKeys' :: [YamlLight] -> YamlLight -> [(ByteString.ByteString,[YamlLight])]
  getTerminalsKeys' hist (YStr s) = [(s,hist)]
  getTerminalsKeys' hist (YSeq s) = concatMap (getTerminalsKeys' hist) s
  getTerminalsKeys' hist (YMap m) = concat . Map.elems $ Map.mapWithKey (\k -> getTerminalsKeys' (k : hist)) m
  getTerminalsKeys' _ _ = []

  -- | Get the contents of a sequence
  unSeq :: YamlLight -> Maybe [YamlLight]
  unSeq (YSeq s) = Just s
  unSeq _        = Nothing

  -- | Get the contents of a map
  unMap :: YamlLight -> Maybe (Map.Map YamlLight YamlLight)
  unMap (YMap m) = Just m
  unMap _       = Nothing

  -- | Get the contents of a string
  unStr :: YamlLight -> Maybe ByteString.ByteString
  unStr (YStr s) = Just s
  unStr _       = Nothing


  -- | '(|!)' and '(*!)' are combinators that simplify accessing values within nested
  --   maps. Consider the following YAML document.
  --
  --   @
  --     foo:
  --       a:
  --        - 1
  --        - 2
  --        - 3
  --       b: barry
  --     bar:
  --       c:
  --         d: dale
  --         e: estelle
  --   @
  --
  --
  --   @y *! \"foo\" |! \"a\"  == Just (YSeq [YStr \"1\",YStr \"2\",YStr \"3\"])@
  --
  --   @y *! \"food\" |! \"a\" == Nothing@
  --
  --   @y *! \"bar\"         == Just (YMap (fromList [(YStr \"c\",YMap
  --                                      (fromList [(YStr \"d\",YStr \"dale\"),
  --                                          (YStr \"e\",YStr \"estelle\")]))]))@
  --
  --   @y *! \"bar\" |! \"c\" |! \"d\" == Just (YStr \"dale\")@
  --
  --   Use an application of '*!' followed by zero or more applications of '|!' to get the
  --   value you want.
  --
  --   For @y *! key@ will be returned if:
  --
  --     * @y@ not contructed with 'YMap'.
  --
  --     * @YStr key@ is not a key in the map.
  --
  --   Similarly for '|!'
  --
  (*!) :: YamlLight -> ByteString.ByteString -> Maybe YamlLight
  (*!) = (|!) . Just


  (|!) :: Maybe YamlLight -> ByteString.ByteString -> Maybe YamlLight
  my |! s = do -- maybe monad
   y <- my
   lookupYL (YStr s) y



  infixl 5 |!, *!

  -- tests

  performTest :: Show a => (YamlLight -> a) -> String -> IO ()
  performTest f s = parseYaml s >>= print . f

  cSeqMap1 = "[{key1: val1, key2: val2}, {key3: val3}]"
  cMapSeq1 = "{key1: [val1, val2, val3], key2: [val4, val5]}"
  gtKeys1  = " [{key1:                     \
             \   { key1_1: [str1, str2]    \
             \   , key1_2: [str2, str3] }} \
             \ , {key2: [str4]}            \
             \ , str5                      \
             \ ] "
  gtKeys2 = "[a, b, c]"
  gtKeys3 = "a: {b: [c, {d: [e, f]}]}"
  gtKeys4 = "[{a: {b: [c1, c2], d: [e1, e2]}, f: [g]}, h]"

  nestedMap = "foo:\n\
              \  a:\n\
              \    - 1\n\
              \    - 2\n\
              \    - 3\n\
              \  b: barry\n\
              \bar:\n\
              \  c:\n\
              \    d: dale\n\
              \    e: estelle"


  testCombineSequencedMaps1 = performTest combineSequencedMaps cSeqMap1

  testCombineMappedSequences1 = performTest combineMappedSequences cMapSeq1

  testGetTerminalsKeys1 = performTest getTerminalsKeys gtKeys1
  testGetTerminalsKeys2 = performTest getTerminalsKeys gtKeys2
  testGetTerminalsKeys3 = performTest getTerminalsKeys gtKeys3
  testGetTerminalsKeys4 = performTest getTerminalsKeys gtKeys4

  testNestedMap1 = performTest (\y -> y *! "foo" |! "a") nestedMap
  testNestedMap2 = performTest (\y -> y *! "food" |! "a") nestedMap
  testNestedMap3 = performTest (\y -> y *! "bar") nestedMap
  testNestedMap4 = performTest (\y -> y *! "bar" |! "c" |! "d") nestedMap