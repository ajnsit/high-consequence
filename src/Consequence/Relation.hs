{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Consequence.Relation (Relation(..), PivotedRelation, heroQuery) where

import Data.Row
import qualified Data.Row.Internal as RInternal
import qualified Data.Row.Records as Rec
import Data.Text (Text)

import Consequence.RProxy (RProxy(RP))
import Util ((|>))

-- A Relation
data Relation (isPivot :: Bool) (r :: Row *) where

  -- Constructors ------------------------

  -- | Named DB Table
  DBBackedTable :: Text -> Relation 'False r

  -- | Relation from Raw Data
  RawDataRelation :: [Rec.Rec r] -> Relation 'False r

  -- | Empty Relation
  Empty :: Relation 'False r


  -- Transformations ---------------------

  -- | Select some columns from a relation
  Project
    :: Disjoint r rest
    => Relation b (r .+ rest)
    -> Relation 'False r

  -- | Remove some columns from a relation
  Except
    :: Disjoint r rest
    => Relation b (r .+ rest)
    -> Relation 'False rest

  -- | Rename a field in a relation
  RenameField
    :: (KnownSymbol old, KnownSymbol new)
    => Label old
    -> Label new
    -> Relation b r
    -> Relation b (Rec.Rename old new r)

  -- | Add a new field to a relation. The initial values for the column are computed per record.
  AddField
    :: (KnownSymbol l, Lacks l r)
    => Label l
    -> (Rec r -> a)
    -> Relation b r
    -> Relation 'False (Rec.Extend l a r)

  -- | Remove a single field from a relation
  RemoveField
    :: (KnownSymbol l, HasType l a r)
    => Label l
    -> Relation b r
    -> Relation 'False (r .- l)


  --- Row Ops --------------------------------

  Filter
    -- This function needs to be compiled to SQL
    -- It's perhaps more appropriate to have a small
    -- DSL here as well for Ops
    --   Op (Rec r) Bool
    :: (Rec r -> Bool)
    -> Relation b r
    -> Relation b r

  Limit
    :: Int
    -> Relation b r
    -> Relation b r


  -- Cartesian logic -------------------------

  -- Combine all the rows of a table
  -- Does NOT remove any duplicates in the process
  Union
    :: Relation b r
    -> Relation b r
    -> Relation b r


  -- Probably not a good idea
  -- Difference
  --   :: Relation b r
  --   -> Relation b r
  --   -> Relation b r


  -- Joins -----------------------------------

  -- NOTE: We can't choose the join fields
  -- Because then we don't know how to handle duplicate columns

  Join
    -- Bind `intersection` type var to allow passing it
    -- via TypeApplications at the time of usage
    -- So you can confirm the fields the join is on
    :: intersection ~ (r1 .\\ (r1 .\\ r2))
    => Relation b r1
    -> Relation b r2
    -> Relation 'False (r1 .\/ r2)


  -- Group -----------------------------------

  -- Not SQL "group by"
  -- Read - https://github.com/agentm/project-m36/blob/master/docs/introduction_to_the_relational_algebra.markdown#group

  -- Constructs nested relations
  -- This can be used to handle all rowspans and colspans

  -- After grouping, `r .\\ cols` will have unique rows

  -- TODO: Not sure why this doesn't work -
  -- Group
  --   :: (KnownSymbol l, Lacks l r, Disjoint cols r)
  --   => RProxy cols
  --   -> Label l
  --   -> Relation (r .+ cols)
  --   -> Relation (r .+ (l .== Relation cols))

  -- I don't like using `.\\` instead of `.+`, because
  -- it requires adding Internal.Subset constraint to prevent invalid usage
  Group
    :: (KnownSymbol l, Lacks l r, RInternal.Subset cols r)
    => RProxy cols
    -> Label l
    -> Relation b r
    -> Relation 'False (Rec.Extend l (Relation 'False cols) (r .\\ cols))

  -- Inverse of Group
  Ungroup
    :: (KnownSymbol l, HasType l (Relation c cols) r)
    => Label l
    -> Relation b r
    -> Relation 'False (r .- l .+ cols)


  --- Mapping --------------------------------

  -- Not a part of Relational Algebra

  -- | Map over a single column
  MapColumn
    :: (KnownSymbol l, HasType l a r)
    => Label l
    -> (a -> b)
    -> Relation p r
    -> Relation 'False (Rec.Extend l b (r .- l))

  -- | Map over entire records in the table
  MapAll
    :: (Rec.Rec r -> Rec.Rec s)
    -> Relation b r
    -> Relation 'False s


  -- Pivoting --------------------------------------

  -- Not a part of Relational Algebra

  -- You can pivot over a column
  -- This first groups over that column, which makes all values unique
  -- Then it makes all the values in that column as column headers,
  -- And then pivots the remaining relation values (which are themeselves relations)
  -- Example -

  -- | Initial Relation
  --  +-----+-----+-----+
  --  |Year |Car  |Sales|
  --  +-----+-----+-----+
  --  |2001 |A    |100  |
  --  |2001 |A    |200  |
  --  |2001 |B    |100  |
  --  |2002 |B    |100  |
  --  |2002 |A    |300  |
  --  |2002 |C    |300  |
  --  +-----+-----+-----+

  -- | Pivot 'Year
  --
  -- 1. Will effectively first Group over 'Year
  --  +-----+----------+
  --  |Year |DummyTitle|
  --  +-----+------+---+
  --  |2001 |Car|Sales |
  --  |     +---+------+
  --  |     |A  | 100  |
  --  |     |B  | 200  |
  --  |     |C  | 100  |
  --  +-----+---+------+
  --  |2002 |Car|Sales |
  --  |     +---+------+
  --  |     |A  | 300  |
  --  |     |B  | 100  |
  --  |     |C  | 300  |
  --  +-----+------+---+
  --
  -- 2. Then move year values to columns
  -- +----------+----------+
  -- | 2001     | 2002     |
  -- +-----+----+-----+----+
  -- |Car |Sales|Car |Sales|
  -- +----+-----+----+-----+
  -- | A  | 100 | A  | 300 |
  -- | A  | 200 | B  | 100 |
  -- | B  | 100 | C  | 300 |
  -- +---------------------+
  --
  -- At this point, it will always be a single row, and with only relational values
  --
  -- NOTE: We could also Pivot again on another column. We would need to map.
  -- For example -
  --
  -- mapAll $ Pivot `Car
  -- ._____________________________.
  -- | 2001      | 2002            |
  -- |-----------+-----------------|
  -- | A   | B   | A   | B   | C   |
  -- |-----+-----+-----+-----+-----|
  -- |Sale |Sale |Sale |Sale |Sale |
  -- |-----|-----|-----|-----|-----|
  -- | 100 | 100 | 300 | 300 | 300 |
  -- | 200 |_____|_____|_____|_____|
  -- '-----'     |     |     |     |
  -- '-----------------------------'
  --
  -- Here it should be clear that the 5 individual "Sale" relations
  --  in the cells are independent, and cannot be merged row wise
  --
  --
  -- RESTRICTION: After a PIVOT, the columns are unknown at compile time
  -- Then we lose the ability to map over a single column
  -- We can then only map over ALL columns
  -- As it's guaranteed that all the columns have the same known datatype
  -- Pivoting back however is possible, and leads to columns being known again

  -- | Pivot a Relation
  -- Pivoting is only possible for Text Columns
  Pivot
    :: (KnownSymbol l, HasType l Text r)
    => Label l
    -> Relation b r
    -> PivotedRelation (r .- l)

  -- | Undo a Pivot
  Unpivot
    :: (KnownSymbol l, HasType l Text r)
    => Label l
    -> PivotedRelation (r .- l)
    -> Relation 'False r

  MapPivot
    :: (Relation b r -> Relation c r')
    -> PivotedRelation r
    -> PivotedRelation r'

  -- Aggregations --------------------------------------
  -- TODO
  -- A relation can be aggregated to a single value
  -- It then ceases to be a relation anymore

  -- Continuing the previous example, we can aggregate -
  --
  -- mapAll $ mapAll $ Aggregate ('Sale) SumAgg
  -- ._____________________________.
  -- | 2001      | 2002            |
  -- |-----------+-----------------|
  -- | A   | B   | A   | B   | C   |
  -- |-----+-----+-----+-----+-----|
  -- | 300 | 100 | 300 | 300 | 300 |
  -- '-----------------------------'

  {-
  -- Aggregation
  -- TODO: Implement
  data Aggregation (from :: Row *) (to :: Row *) = Aggregation
  -}


  -- Drilldown -----------------------------------------
  -- A Drilldown table is just a table with multiple successive groupings. E.g.
  -- TODO: It doesn't need special support?

  --  +-----+----------+
  --  |Year |Dummy     |
  --  +-----+------+---+
  --  |2001 |Car|Dummy |
  --  |     +---+------+
  --  |     |A  | Sales|
  --  |     |   +------+
  --  |     |   |  100 |
  --  |     +---+------+
  --  |     |B  | Sales|
  --  |     |   +------+
  --  |     |   |  100 |
  --  |     |   |  200 |
  --  |     +---+------+
  --  |     |C  | Sales|
  --  |     |   +------+
  --  |     |   |  300 |
  --  |     |   |  600 |
  --  +-----+---+------+
  --  |2002 |Car|Dummy |
  --  |     +---+------+
  --  |     |A  | Sales|
  --  |     |   +------+
  --  |     |   |  100 |
  --  |     |   |  200 |
  --  |     +---+------+
  --  |     |C  | Sales|
  --  |     |   +------+
  --  |     |   |  100 |
  --  +-----+---+--+---+


-- Pivoted Tables --------------------------------

-- TODO: Are we counting on label l never being named these?
type INTERNAL_PIVOTED_FIELD = "INTERNAL_PIVOTED_FIELD"
type INTERNAL_PIVOT_COLUMNS = "INTERNAL_PIVOT_COLUMNS"

-- TODO: Allow the internal pivot columns to be pivoted again
-- This would require adding another isPivot param to PivotedRelation

-- | Internally a Pivoted relation is just a grouped relation
-- Where the grouping has been done on all columns but one
-- The remaining column must be Text, and is called the "pivoted field"
-- We forget its label, and don't have a user defined label for the newly created relation valued column either, which holds the rest of the table data
-- Unpivoting requires supplying the label of the pivoted field again
type PivotedRelation (r :: Row *) = Relation 'True (INTERNAL_PIVOTED_FIELD .== Text .+ INTERNAL_PIVOT_COLUMNS .== Relation 'False r)


------------------------------------------------------------
-- Sample Usage
personTable :: Relation 'False ("name" .== String .+ "age" .== Int)
personTable = DBBackedTable "persons"

-- Type signature is not needed here
randomSample :: Relation 'False ("name" .== String)
randomSample = personTable
  |> AddField #age1 (const (100::Int))
  |> AddField #dummy (const (123::Int))
  |> AddField #age2 (\r -> r .! #age + r .! #age1)
  |> Except @("age1" .== Int .+ "dummy" .== Int)
  |> RemoveField #age
  |> Join @("name" .== String) personTable
  |> RenameField #age #age1
  |> Project @("name" .== String)


-- GraphQL Star Wars examples
-- query Hero($episode: Episode, $withFriends: Boolean!) {
--   hero(episode: $episode) {
--     name
--     appearsIn
--     friends @include(if: $withFriends) {
--       name
--     }
--   }
-- }

-- Equivalent HC Query
-- Seems quite a bit bigger, but includes complete informatio on how to get data
-- Also, the GraphQL query seems unclear, does `appearsIn` contain movies apart from the $episode?
heroQuery :: String -> Bool -> Relation 'False OutputTableRow
heroQuery episode withFriends = heroMovieInformation |> addHeroFriendInformation
  where
    -- All hero ids which appear in supplied episode
    heroesThatAppearInThisEpisode = heroTable
      |> Join @("heroId" .== Int) movieAppearenceTable
      |> Join @("movieId" .== Int)
              (movieTable |> RenameField #name #appearsIn)
      |> Filter (\r -> r .! #appearsIn == episode)
      |> Project @("heroId" .== Int .+ "name" .== String)
    -- Get movie information for ids
    heroMovieInformation = heroesThatAppearInThisEpisode
      |> Join @("heroId" .== Int) movieAppearenceTable
      |> Join @("movieId" .== Int)
              (movieTable |> RenameField #name #appearsIn)
      |> Group (RP @("movieId" .== Int .+ "appearsIn" .== String)) #movieDetails
    -- Get friend information for ids
    addHeroFriendInformation h
      | withFriends = h
          |> Join @("heroId" .== Int) friendTable
          |> Join @("friendId" .== Int)
                  (heroTable |> RenameField #heroId #friendId
                            |> RenameField #name #friendName)
          |> Group (RP @("friendId" .== Int .+ "friendName" .== String)) #friendDetails
      | otherwise = h |> AddField #friendDetails (const Empty)

-- Types needed for this query follow
type HeroRow =
  (  "name" .== String
  .+ "heroId" .== Int
  )

type MovieRow =
  (  "movieId" .== Int
  .+ "name" .== String
  )

type MovieAppearenceRow =
  (  "heroId" .== Int
  .+ "movieId" .== Int
  )

type FriendRow =
  (  "heroId" .== Int
  .+ "friendId" .== Int
  )

heroTable :: Relation 'False HeroRow
heroTable = DBBackedTable "heroes"

movieTable :: Relation 'False MovieRow
movieTable = DBBackedTable "movies"

movieAppearenceTable :: Relation 'False MovieAppearenceRow
movieAppearenceTable = DBBackedTable "movieAppearences"

friendTable :: Relation 'False FriendRow
friendTable = DBBackedTable "friends"

type MovieDetailsRow = ("movieId" .== Int .+ "appearsIn" .== String)
type FriendDetailsRow = ("friendId" .== Int .+ "friendName" .== String)
type OutputTableRow = ("heroId" .== Int .+ "name" .== String .+ "friendDetails" .== Relation 'False FriendDetailsRow .+ "movieDetails" .== Relation 'False MovieDetailsRow)
