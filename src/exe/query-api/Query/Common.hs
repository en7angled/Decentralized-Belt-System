-- | Shared pagination helpers and ordering utilities used by both live
-- ('Query.Live') and projected ('Query.Projected') query modules.
module Query.Common where

import Data.Maybe (fromMaybe)
import DomainTypes.Core.BJJ (BJJBelt (..))
import DomainTypes.Transfer.OrderBy (SortOrder (Asc))

-- | True when the belt is Black or higher (can award promotions per protocol UX).
beltIsMasterCapable :: BJJBelt -> Bool
beltIsMasterCapable b = b >= Black

type Limit = Int

type Offset = Int

-- | Normalize optional limit/offset. Both absent → a bounded default page
-- (100, 0) rather than unbounded. Limit is clamped to [1, 500]; offset to >= 0.
normalizeLimitOffset :: Maybe Int -> Maybe Int -> Maybe (Int, Int)
normalizeLimitOffset limit offset =
  Just (clampLimit (fromMaybe 100 limit), max 0 (fromMaybe 0 offset))
  where
    clampLimit l = max 1 (min 500 l)

-- | Normalize optional order_by and sort_order; default sort order when only order_by given is Asc.
normalizeOrder :: Maybe a -> Maybe SortOrder -> Maybe (a, SortOrder)
normalizeOrder orderBy sortOrder = case (orderBy, sortOrder) of
  (Just ob, Just so) -> Just (ob, so)
  (Just ob, Nothing) -> Just (ob, Asc)
  _ -> Nothing

-- | Slice a list by the given limit and offset. 'Nothing' returns all items.
applyLimits :: Maybe (Limit, Offset) -> [a] -> [a]
applyLimits Nothing xs = xs
applyLimits (Just (limit, offset)) xs =
  let safeLimit = Prelude.max 0 limit
      safeOffset = Prelude.max 0 offset
   in Prelude.take safeLimit (Prelude.drop safeOffset xs)

-- | Apply optional filter, then optional ordering, then optional limit/offset.
-- Caller supplies the list and the two optional-argument list transformers.
applyFilterOrderLimit ::
  Maybe (Limit, Offset) ->
  Maybe f ->
  Maybe (ob, SortOrder) ->
  (Maybe f -> [a] -> [a]) ->
  (Maybe (ob, SortOrder) -> [a] -> [a]) ->
  [a] ->
  [a]
applyFilterOrderLimit maybeLimitOffset maybeFilter maybeOrder applyFilter applyOrder =
  applyLimits maybeLimitOffset . applyOrder maybeOrder . applyFilter maybeFilter
