{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- NOTE! This module is automatically generated!
module Games.Pokemon.Pokeapi.Types where
import Data.Aeson
import Control.Lens
import Control.Monad (mzero)

data APIResourceList
  = APIResourceList {
      aPIResourceListCount :: Integer -- ^ The total number of resources available from this API
    , aPIResourceListNext :: String -- ^ The URL for the next page in the list
    , aPIResourceListPrevious :: Bool -- ^ The URL for the previous page in the list
    , aPIResourceListResults :: [APIResource] -- ^ A list of unnamed API resources
  } deriving (Eq, Ord, Show)

instance FromJSON APIResourceList where
  parseJSON (Object o) =
    APIResourceList
      <$> o .: "count"
      <*> o .: "next"
      <*> o .: "previous"
      <*> o .: "results"
  parseJSON _ = mzero

data NamedAPIResourceList
  = NamedAPIResourceList {
      namedAPIResourceListCount :: Integer -- ^ The total number of resources available from this API
    , namedAPIResourceListNext :: String -- ^ The URL for the next page in the list
    , namedAPIResourceListPrevious :: Bool -- ^ The URL for the previous page in the list
    , namedAPIResourceListResults :: [NamedAPIResource] -- ^ A list of named API resources
  } deriving (Eq, Ord, Show)

instance FromJSON NamedAPIResourceList where
  parseJSON (Object o) =
    NamedAPIResourceList
      <$> o .: "count"
      <*> o .: "next"
      <*> o .: "previous"
      <*> o .: "results"
  parseJSON _ = mzero

data Berry
  = Berry {
      berryId :: Integer -- ^ The identifier for this berry resource
    , berryName :: String -- ^ The name for this berry resource
    , berryGrowth_time :: Integer -- ^ Time it takes the tree to grow one stage, in hours. Berry trees go through four of these growth stages before they can be picked.
    , berryMax_harvest :: Integer -- ^ The maximum number of these berries that can grow on one tree in Generation IV
    , berryNatural_gift_power :: Integer -- ^ The power of the move "Natural Gift" when used with this Berry
    , berrySize :: Integer -- ^ The size of this Berry, in millimeters
    , berrySmoothness :: Integer -- ^ The smoothness of this Berry, used in making Pokéblocks or Poffins
    , berrySoil_dryness :: Integer -- ^ The speed at which this Berry dries out the soil as it grows.  A higher rate means the soil dries more quickly.
    , berryFirmness :: NamedAPIResource -- ^ The firmness of this berry, used in making Pokéblocks or Poffins
    , berryFlavors :: [BerryFlavorMap] -- ^ A list of references to each flavor a berry can have and the potency of each of those flavors in regard to this berry
    , berryItem :: NamedAPIResource -- ^ Berries are actually items. This is a reference to the item specific data for this berry.
    , berryNatural_gift_type :: NamedAPIResource -- ^ The Type the move "Natural Gift" has when used with this Berry
  } deriving (Eq, Ord, Show)

instance FromJSON Berry where
  parseJSON (Object o) =
    Berry
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "growth_time"
      <*> o .: "max_harvest"
      <*> o .: "natural_gift_power"
      <*> o .: "size"
      <*> o .: "smoothness"
      <*> o .: "soil_dryness"
      <*> o .: "firmness"
      <*> o .: "flavors"
      <*> o .: "item"
      <*> o .: "natural_gift_type"
  parseJSON _ = mzero

data BerryFlavorMap
  = BerryFlavorMap {
      berryFlavorMapPotency :: Integer -- ^ How powerful the referenced flavor is for this berry
    , berryFlavorMapFlavor :: NamedAPIResource -- ^ The referenced berry flavor
  } deriving (Eq, Ord, Show)

instance FromJSON BerryFlavorMap where
  parseJSON (Object o) =
    BerryFlavorMap
      <$> o .: "potency"
      <*> o .: "flavor"
  parseJSON _ = mzero

data BerryFirmness
  = BerryFirmness {
      berryFirmnessId :: Integer -- ^ The identifier for this berry firmness resource
    , berryFirmnessName :: String -- ^ The name for this berry firmness resource
    , berryFirmnessBerries :: [NamedAPIResource] -- ^ A list of the berries with this firmness
    , berryFirmnessNames :: [Name] -- ^ The name of this berry firmness listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON BerryFirmness where
  parseJSON (Object o) =
    BerryFirmness
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "berries"
      <*> o .: "names"
  parseJSON _ = mzero

data BerryFlavor
  = BerryFlavor {
      berryFlavorId :: Integer -- ^ The identifier for this berry flavor resource
    , berryFlavorName :: String -- ^ The name for this berry flavor resource
    , berryFlavorBerries :: [FlavorBerryMap] -- ^ A list of the berries with this flavor
    , berryFlavorContest_type :: NamedAPIResource -- ^ The contest type that correlates with this berry flavor
    , berryFlavorNames :: [Name] -- ^ The name of this berry flavor listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON BerryFlavor where
  parseJSON (Object o) =
    BerryFlavor
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "berries"
      <*> o .: "contest_type"
      <*> o .: "names"
  parseJSON _ = mzero

data FlavorBerryMap
  = FlavorBerryMap {
      flavorBerryMapPotency :: Integer -- ^ How powerful the referenced flavor is for this berry
    , flavorBerryMapBerry :: NamedAPIResource -- ^ The berry with the referenced flavor
  } deriving (Eq, Ord, Show)

instance FromJSON FlavorBerryMap where
  parseJSON (Object o) =
    FlavorBerryMap
      <$> o .: "potency"
      <*> o .: "berry"
  parseJSON _ = mzero

data ContestType
  = ContestType {
      contestTypeId :: Integer -- ^ The identifier for this contest type resource
    , contestTypeName :: String -- ^ The name for this contest type resource
    , contestTypeBerry_flavor :: NamedAPIResource -- ^ The berry flavor that correlates with this contest type
    , contestTypeNames :: [ContestName] -- ^ The name of this contest type listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON ContestType where
  parseJSON (Object o) =
    ContestType
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "berry_flavor"
      <*> o .: "names"
  parseJSON _ = mzero

data ContestName
  = ContestName {
      contestNameName :: String -- ^ The name for this contest
    , contestNameColor :: String -- ^ The color associated with this contest's name
    , contestNameLanguage :: NamedAPIResource -- ^ The language that this name is in
  } deriving (Eq, Ord, Show)

instance FromJSON ContestName where
  parseJSON (Object o) =
    ContestName
      <$> o .: "name"
      <*> o .: "color"
      <*> o .: "language"
  parseJSON _ = mzero

data ContestEffect
  = ContestEffect {
      contestEffectId :: Integer -- ^ The identifier for this contest type resource
    , contestEffectAppeal :: Integer -- ^ The base number of hearts the user of this move gets
    , contestEffectJam :: Integer -- ^ The base number of hearts the user's opponent loses
    , contestEffectEffect_entries :: [Effect] -- ^ The result of this contest effect listed in different languages
    , contestEffectFlavor_text_entries :: [FlavorText] -- ^ The flavor text of this contest effect listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON ContestEffect where
  parseJSON (Object o) =
    ContestEffect
      <$> o .: "id"
      <*> o .: "appeal"
      <*> o .: "jam"
      <*> o .: "effect_entries"
      <*> o .: "flavor_text_entries"
  parseJSON _ = mzero

data SuperContestEffect
  = SuperContestEffect {
      superContestEffectId :: Integer -- ^ The identifier for this super contest effect resource
    , superContestEffectAppeal :: Integer -- ^ The level of appeal this super contest effect has
    , superContestEffectFlavor_text_entries :: [FlavorText] -- ^ The flavor text of this super contest effect listed in different languages
    , superContestEffectMoves :: [NamedAPIResource] -- ^ A list of moves that have the effect when used in super contests
  } deriving (Eq, Ord, Show)

instance FromJSON SuperContestEffect where
  parseJSON (Object o) =
    SuperContestEffect
      <$> o .: "id"
      <*> o .: "appeal"
      <*> o .: "flavor_text_entries"
      <*> o .: "moves"
  parseJSON _ = mzero

data EncounterMethod
  = EncounterMethod {
      encounterMethodId :: Integer -- ^ The identifier for this encounter method resource
    , encounterMethodName :: String -- ^ The name for this encounter method resource
    , encounterMethodOrder :: Integer -- ^ A good value for sorting
    , encounterMethodNames :: [Name] -- ^ The name of this encounter method listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON EncounterMethod where
  parseJSON (Object o) =
    EncounterMethod
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "order"
      <*> o .: "names"
  parseJSON _ = mzero

data EncounterCondition
  = EncounterCondition {
      encounterConditionId :: Integer -- ^ The identifier for this encounter condition resource
    , encounterConditionName :: String -- ^ The name for this encounter condition resource
    , encounterConditionNames :: [Name] -- ^ The name of this encounter method listed in different languages
    , encounterConditionValues :: [NamedAPIResource] -- ^ A list of possible values for this encounter condition
  } deriving (Eq, Ord, Show)

instance FromJSON EncounterCondition where
  parseJSON (Object o) =
    EncounterCondition
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "names"
      <*> o .: "values"
  parseJSON _ = mzero

data EncounterConditionValue
  = EncounterConditionValue {
      encounterConditionValueId :: Integer -- ^ The identifier for this encounter condition value resource
    , encounterConditionValueName :: String -- ^ The name for this encounter condition value resource
    , encounterConditionValueCondition :: [NamedAPIResource] -- ^ The condition this encounter condition value pertains to
    , encounterConditionValueNames :: [Name] -- ^ The name of this encounter condition value listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON EncounterConditionValue where
  parseJSON (Object o) =
    EncounterConditionValue
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "condition"
      <*> o .: "names"
  parseJSON _ = mzero

data EvolutionChain
  = EvolutionChain {
      evolutionChainId :: Integer -- ^ The identifier for this evolution chain resource
    , evolutionChainBaby_trigger_item :: NamedAPIResource -- ^ The item that a Pokémon would be holding when mating that would trigger the egg hatching a baby Pokémon rather than a basic Pokémon
    , evolutionChainChain :: ChainLink -- ^ The base chain link object. Each link contains evolution details for a Pokémon in the chain. Each link references the next Pokémon in the natural evolution order.
  } deriving (Eq, Ord, Show)

instance FromJSON EvolutionChain where
  parseJSON (Object o) =
    EvolutionChain
      <$> o .: "id"
      <*> o .: "baby_trigger_item"
      <*> o .: "chain"
  parseJSON _ = mzero

data ChainLink
  = ChainLink {
      chainLinkIs_baby :: Bool -- ^ Whether or not this link is for a baby Pokémon. This would only ever be true on the base link.
    , chainLinkSpecies :: NamedAPIResource -- ^ The Pokémon species at this point in the evolution chain
    , chainLinkEvolution_details :: [EvolutionDetail] -- ^ All details regarding the specific details of the referenced Pokémon species evolution
    , chainLinkEvolves_to :: [ChainLink] -- ^ A List of chain objects.
  } deriving (Eq, Ord, Show)

instance FromJSON ChainLink where
  parseJSON (Object o) =
    ChainLink
      <$> o .: "is_baby"
      <*> o .: "species"
      <*> o .: "evolution_details"
      <*> o .: "evolves_to"
  parseJSON _ = mzero

data EvolutionDetail
  = EvolutionDetail {
      evolutionDetailItem :: NamedAPIResource -- ^ The item required to cause evolution this into Pokémon species
    , evolutionDetailTrigger :: NamedAPIResource -- ^ The type of event that triggers evolution into this Pokémon species
    , evolutionDetailGender :: Integer -- ^ The id of the gender of the evolving Pokémon species must be in order to evolve into this Pokémon species
    , evolutionDetailHeld_item :: NamedAPIResource -- ^ The item the evolving Pokémon species must be holding during the evolution trigger event to evolve into this Pokémon species
    , evolutionDetailKnown_move :: NamedAPIResource -- ^ The move that must be known by the evolving Pokémon species during the evolution trigger event in order to evolve into this Pokémon species
    , evolutionDetailKnown_move_type :: NamedAPIResource -- ^ The evolving Pokémon species must know a move with this type during the evolution trigger event in order to evolve into this Pokémon species
    , evolutionDetailLocation :: NamedAPIResource -- ^ The location the evolution must be triggered at.
    , evolutionDetailMin_level :: Integer -- ^ The minimum required level of the evolving Pokémon species to evolve into this Pokémon species
    , evolutionDetailMin_happiness :: Integer -- ^ The minimum required level of happiness the evolving Pokémon species to evolve into this Pokémon species
    , evolutionDetailMin_beauty :: Integer -- ^ The minimum required level of beauty the evolving Pokémon species to evolve into this Pokémon species
    , evolutionDetailMin_affection :: Integer -- ^ The minimum required level of affection the evolving Pokémon species to evolve into this Pokémon species
    , evolutionDetailNeeds_overworld_rain :: Bool -- ^ Whether or not it must be raining in the overworld to cause evolution this Pokémon species
    , evolutionDetailParty_species :: NamedAPIResource -- ^ The Pokémon species that must be in the players party in order for the evolving Pokémon species to evolve into this Pokémon species
    , evolutionDetailParty_type :: NamedAPIResource -- ^ The player must have a Pokémon of this type in their party during the evolution trigger event in order for the evolving Pokémon species to evolve into this Pokémon species
    , evolutionDetailRelative_physical_stats :: Integer -- ^ The required relation between the Pokémon's Attack and Defense stats. 1 means Attack > Defense. 0 means Attack = Defense. -1 means Attack < Defense.
    , evolutionDetailTime_of_day :: String -- ^ The required time of day. Day or night.
    , evolutionDetailTrade_species :: NamedAPIResource -- ^ Pokémon species for which this one must be traded.
    , evolutionDetailTurn_upside_down :: Bool -- ^ Whether or not the 3DS needs to be turned upside-down as this Pokémon levels up.
  } deriving (Eq, Ord, Show)

instance FromJSON EvolutionDetail where
  parseJSON (Object o) =
    EvolutionDetail
      <$> o .: "item"
      <*> o .: "trigger"
      <*> o .: "gender"
      <*> o .: "held_item"
      <*> o .: "known_move"
      <*> o .: "known_move_type"
      <*> o .: "location"
      <*> o .: "min_level"
      <*> o .: "min_happiness"
      <*> o .: "min_beauty"
      <*> o .: "min_affection"
      <*> o .: "needs_overworld_rain"
      <*> o .: "party_species"
      <*> o .: "party_type"
      <*> o .: "relative_physical_stats"
      <*> o .: "time_of_day"
      <*> o .: "trade_species"
      <*> o .: "turn_upside_down"
  parseJSON _ = mzero

data EvolutionTrigger
  = EvolutionTrigger {
      evolutionTriggerId :: Integer -- ^ The identifier for this evolution trigger resource
    , evolutionTriggerName :: String -- ^ The name for this evolution trigger resource
    , evolutionTriggerNames :: [Name] -- ^ The name of this evolution trigger listed in different languages
    , evolutionTriggerPokemon_species :: [NamedAPIResource] -- ^ A list of pokemon species that result from this evolution trigger
  } deriving (Eq, Ord, Show)

instance FromJSON EvolutionTrigger where
  parseJSON (Object o) =
    EvolutionTrigger
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "names"
      <*> o .: "pokemon_species"
  parseJSON _ = mzero

data Generation
  = Generation {
      generationId :: Integer -- ^ The identifier for this generation resource
    , generationName :: String -- ^ The name for this generation resource
    , generationAbilities :: [NamedAPIResource] -- ^ A list of abilities that were introduced in this generation
    , generationNames :: [Name] -- ^ The name of this generation listed in different languages
    , generationMain_region :: NamedAPIResource -- ^ The main region travelled in this generation
    , generationMoves :: [NamedAPIResource] -- ^ A list of moves that were introduced in this generation
    , generationPokemon_species :: [NamedAPIResource] -- ^ A list of Pokémon species that were introduced in this generation
    , generationTypes :: [NamedAPIResource] -- ^ A list of types that were introduced in this generation
    , generationVersion_groups :: [NamedAPIResource] -- ^ A list of version groups that were introduced in this generation
  } deriving (Eq, Ord, Show)

instance FromJSON Generation where
  parseJSON (Object o) =
    Generation
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "abilities"
      <*> o .: "names"
      <*> o .: "main_region"
      <*> o .: "moves"
      <*> o .: "pokemon_species"
      <*> o .: "types"
      <*> o .: "version_groups"
  parseJSON _ = mzero

data Pokedex
  = Pokedex {
      pokedexId :: Integer -- ^ The identifier for this Pokédex resource
    , pokedexName :: String -- ^ The name for this Pokédex resource
    , pokedexIs_main_series :: Bool -- ^ Whether or not this Pokédex originated in the main series of the video games
    , pokedexDescriptions :: [Description] -- ^ The description of this Pokédex listed in different languages
    , pokedexNames :: [Name] -- ^ The name of this Pokédex listed in different languages
    , pokedexPokemon_entries :: [PokemonEntry] -- ^ A list of Pokémon catalogued in this Pokédex and their indexes
    , pokedexRegion :: NamedAPIResource -- ^ The region this Pokédex catalogues Pokémon for
    , pokedexVersion_groups :: [NamedAPIResource] -- ^ A list of version groups this Pokédex is relevant to
  } deriving (Eq, Ord, Show)

instance FromJSON Pokedex where
  parseJSON (Object o) =
    Pokedex
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "is_main_series"
      <*> o .: "descriptions"
      <*> o .: "names"
      <*> o .: "pokemon_entries"
      <*> o .: "region"
      <*> o .: "version_groups"
  parseJSON _ = mzero

data PokemonEntry
  = PokemonEntry {
      pokemonEntryEntry_number :: Integer -- ^ The index of this Pokémon species entry within the Pokédex
    , pokemonEntryPokemon_species :: NamedAPIResource -- ^ The Pokémon species being encountered
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonEntry where
  parseJSON (Object o) =
    PokemonEntry
      <$> o .: "entry_number"
      <*> o .: "pokemon_species"
  parseJSON _ = mzero

data Version
  = Version {
      versionId :: Integer -- ^ The identifier for this version resource
    , versionName :: String -- ^ The name for this version resource
    , versionNames :: [Name] -- ^ The name of this version listed in different languages
    , versionVersion_group :: NamedAPIResource -- ^ The version group this version belongs to
  } deriving (Eq, Ord, Show)

instance FromJSON Version where
  parseJSON (Object o) =
    Version
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "names"
      <*> o .: "version_group"
  parseJSON _ = mzero

data VersionGroup
  = VersionGroup {
      versionGroupId :: Integer -- ^ The identifier for this version group resource
    , versionGroupName :: String -- ^ The name for this version group resource
    , versionGroupOrder :: Integer -- ^ Order for sorting. Almost by date of release, except similar versions are grouped together.
    , versionGroupGeneration :: NamedAPIResource -- ^ The generation this version was introduced in
    , versionGroupMove_learn_methods :: [NamedAPIResource] -- ^ A list of methods in which Pokémon can learn moves in this version group
    , versionGroupPokedexes :: [NamedAPIResource] -- ^ A list of Pokédexes introduces in this version group
    , versionGroupRegions :: [NamedAPIResource] -- ^ A list of regions that can be visited in this version group
    , versionGroupVersions :: [NamedAPIResource] -- ^ The versions this version group owns
  } deriving (Eq, Ord, Show)

instance FromJSON VersionGroup where
  parseJSON (Object o) =
    VersionGroup
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "order"
      <*> o .: "generation"
      <*> o .: "move_learn_methods"
      <*> o .: "pokedexes"
      <*> o .: "regions"
      <*> o .: "versions"
  parseJSON _ = mzero

data Item
  = Item {
      itemId :: Integer -- ^ The identifier for this item resource
    , itemName :: String -- ^ The name for this item resource
    , itemCost :: Integer -- ^ The price of this item in stores
    , itemFling_power :: Integer -- ^ The power of the move Fling when used with this item.
    , itemFling_effect :: NamedAPIResource -- ^ The effect of the move Fling when used with this item
    , itemAttributes :: [NamedAPIResource] -- ^ A list of attributes this item has
    , itemCategory :: ItemCategory -- ^ The category of items this item falls into
    , itemEffect_entries :: [VerboseEffect] -- ^ The effect of this ability listed in different languages
    , itemFlavor_text_entries :: [VersionGroupFlavorText] -- ^ The flavor text of this ability listed in different languages
    , itemGame_indices :: [GenerationGameIndex] -- ^ A list of game indices relevent to this item by generation
    , itemNames :: [Name] -- ^ The name of this item listed in different languages
    , itemSprites :: ItemSprites -- ^ A set of sprites used to depict this item in the game
    , itemHeld_by_pokemon :: [ItemHolderPokemon] -- ^ A list of Pokémon that might be found in the wild holding this item
    , itemBaby_trigger_for :: APIResource -- ^ An evolution chain this item requires to produce a bay during mating
    , itemMachines :: [MachineVersionDetail] -- ^ A list of the machines related to this item
  } deriving (Eq, Ord, Show)

instance FromJSON Item where
  parseJSON (Object o) =
    Item
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "cost"
      <*> o .: "fling_power"
      <*> o .: "fling_effect"
      <*> o .: "attributes"
      <*> o .: "category"
      <*> o .: "effect_entries"
      <*> o .: "flavor_text_entries"
      <*> o .: "game_indices"
      <*> o .: "names"
      <*> o .: "sprites"
      <*> o .: "held_by_pokemon"
      <*> o .: "baby_trigger_for"
      <*> o .: "machines"
  parseJSON _ = mzero

data ItemSprites
  = ItemSprites {
      itemSpritesDefault' :: String -- ^ The default depiction of this item
  } deriving (Eq, Ord, Show)

instance FromJSON ItemSprites where
  parseJSON (Object o) =
    ItemSprites
      <$> o .: "default"
  parseJSON _ = mzero

data ItemHolderPokemon
  = ItemHolderPokemon {
      itemHolderPokemonPokemon :: String -- ^ The Pokémon that holds this item
    , itemHolderPokemonVersion_details :: [ItemHolderPokemonVersionDetail] -- ^ The details for the version that this item is held in by the Pokémon
  } deriving (Eq, Ord, Show)

instance FromJSON ItemHolderPokemon where
  parseJSON (Object o) =
    ItemHolderPokemon
      <$> o .: "pokemon"
      <*> o .: "version_details"
  parseJSON _ = mzero

data ItemHolderPokemonVersionDetail
  = ItemHolderPokemonVersionDetail {
      itemHolderPokemonVersionDetailRarity :: String -- ^ How often this Pokémon holds this item in this version
    , itemHolderPokemonVersionDetailVersion :: NamedAPIResource -- ^ The version that this item is held in by the Pokémon
  } deriving (Eq, Ord, Show)

instance FromJSON ItemHolderPokemonVersionDetail where
  parseJSON (Object o) =
    ItemHolderPokemonVersionDetail
      <$> o .: "rarity"
      <*> o .: "version"
  parseJSON _ = mzero

data ItemAttribute
  = ItemAttribute {
      itemAttributeId :: Integer -- ^ The identifier for this item attribute resource
    , itemAttributeName :: String -- ^ The name for this item attribute resource
    , itemAttributeItems :: [NamedAPIResource] -- ^ A list of items that have this attribute
    , itemAttributeNames :: [Name] -- ^ The name of this item attribute listed in different languages
    , itemAttributeDescriptions :: [Description] -- ^ The description of this item attribute listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON ItemAttribute where
  parseJSON (Object o) =
    ItemAttribute
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "items"
      <*> o .: "names"
      <*> o .: "descriptions"
  parseJSON _ = mzero

data ItemCategory
  = ItemCategory {
      itemCategoryId :: Integer -- ^ The identifier for this item category resource
    , itemCategoryName :: String -- ^ The name for this item category resource
    , itemCategoryItems :: [NamedAPIResource] -- ^ A list of items that are a part of this category
    , itemCategoryNames :: [Name] -- ^ The name of this item category listed in different languages
    , itemCategoryPocket :: NamedAPIResource -- ^ The pocket items in this category would be put in
  } deriving (Eq, Ord, Show)

instance FromJSON ItemCategory where
  parseJSON (Object o) =
    ItemCategory
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "items"
      <*> o .: "names"
      <*> o .: "pocket"
  parseJSON _ = mzero

data ItemFlingEffect
  = ItemFlingEffect {
      itemFlingEffectId :: Integer -- ^ The identifier for this fling effect resource
    , itemFlingEffectName :: String -- ^ The name for this fling effect resource
    , itemFlingEffectEffect_entries :: [Effect] -- ^ The result of this fling effect listed in different languages
    , itemFlingEffectItems :: [NamedAPIResource] -- ^ A list of items that have this fling effect
  } deriving (Eq, Ord, Show)

instance FromJSON ItemFlingEffect where
  parseJSON (Object o) =
    ItemFlingEffect
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "effect_entries"
      <*> o .: "items"
  parseJSON _ = mzero

data ItemPocket
  = ItemPocket {
      itemPocketId :: Integer -- ^ The identifier for this item pocket resource
    , itemPocketName :: String -- ^ The name for this item pocket resource
    , itemPocketCategories :: [NamedAPIResource] -- ^ A list of item categories that are relevant to this item pocket
    , itemPocketNames :: [Name] -- ^ The name of this item pocket listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON ItemPocket where
  parseJSON (Object o) =
    ItemPocket
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "categories"
      <*> o .: "names"
  parseJSON _ = mzero

data Move
  = Move {
      moveId :: Integer -- ^ The identifier for this move resource
    , moveName :: String -- ^ The name for this move resource
    , moveAccuracy :: Integer -- ^ The percent value of how likely this move is to be successful
    , moveEffect_chance :: Integer -- ^ The percent value of how likely it is this moves effect will happen
    , movePp :: Integer -- ^ Power points. The number of times this move can be used
    , movePriority :: Integer -- ^ A value between -8 and 8. Sets the order in which moves are executed during battle. See [Bulbapedia](http://bulbapedia.bulbagarden.net/wiki/Priority) for greater detail.
    , movePower :: Integer -- ^ The base power of this move with a value of 0 if it does not have a base power
    , moveContest_combos :: ContestComboSets -- ^ A detail of normal and super contest combos that require this move
    , moveContest_type :: NamedAPIResource -- ^ The type of appeal this move gives a Pokémon when used in a contest
    , moveContest_effect :: APIResource -- ^ The effect the move has when used in a contest
    , moveDamage_class :: NamedAPIResource -- ^ The type of damage the move inflicts on the target, e.g. physical
    , moveEffect_entries :: [VerboseEffect] -- ^ The effect of this move listed in different languages
    , moveEffect_changes :: [AbilityEffectChange] -- ^ The list of previous effects this move has had across version groups of the games
    , moveFlavor_text_entries :: Move -- ^ The flavor text of this move listed in different languages
    , moveGeneration :: NamedAPIResource -- ^ The generation in which this move was introduced
    , moveMachines :: [MachineVersionDetail] -- ^ A list of the machines that teach this move
    , moveMeta :: MoveMetaData -- ^ Metadata about this move
    , moveNames :: [Name] -- ^ The name of this move listed in different languages
    , movePast_values :: [PastMoveStatValues] -- ^ A list of move resource value changes across version groups of the game
    , moveStat_changes :: [MoveStatChange] -- ^ A list of stats this moves effects and how much it effects them
    , moveSuper_contest_effect :: APIResource -- ^ The effect the move has when used in a super contest
    , moveTarget :: NamedAPIResource -- ^ The type of target that will receive the effects of the attack
    , moveType' :: NamedAPIResource -- ^ The elemental type of this move
  } deriving (Eq, Ord, Show)

instance FromJSON Move where
  parseJSON (Object o) =
    Move
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "accuracy"
      <*> o .: "effect_chance"
      <*> o .: "pp"
      <*> o .: "priority"
      <*> o .: "power"
      <*> o .: "contest_combos"
      <*> o .: "contest_type"
      <*> o .: "contest_effect"
      <*> o .: "damage_class"
      <*> o .: "effect_entries"
      <*> o .: "effect_changes"
      <*> o .: "flavor_text_entries"
      <*> o .: "generation"
      <*> o .: "machines"
      <*> o .: "meta"
      <*> o .: "names"
      <*> o .: "past_values"
      <*> o .: "stat_changes"
      <*> o .: "super_contest_effect"
      <*> o .: "target"
      <*> o .: "type"
  parseJSON _ = mzero

data ContestComboSets
  = ContestComboSets {
      contestComboSetsNormal :: ContestComboDetail -- ^ A detail of moves this move can be used before or after, granting additional appeal points in contests
    , contestComboSetsSuper :: ContestComboDetail -- ^ A detail of moves this move can be used before or after, granting additional appeal points in super contests
  } deriving (Eq, Ord, Show)

instance FromJSON ContestComboSets where
  parseJSON (Object o) =
    ContestComboSets
      <$> o .: "normal"
      <*> o .: "super"
  parseJSON _ = mzero

data ContestComboDetail
  = ContestComboDetail {
      contestComboDetailUse_before :: [NamedAPIResource] -- ^ A list of moves to use before this move
    , contestComboDetailUse_after :: [NamedAPIResource] -- ^ A list of moves to use after this move
  } deriving (Eq, Ord, Show)

instance FromJSON ContestComboDetail where
  parseJSON (Object o) =
    ContestComboDetail
      <$> o .: "use_before"
      <*> o .: "use_after"
  parseJSON _ = mzero

data MoveFlavorText
  = MoveFlavorText {
      moveFlavorTextFlavor_text :: String -- ^ The localized flavor text for an api resource in a specific language
    , moveFlavorTextLanguage :: NamedAPIResource -- ^ The language this name is in
    , moveFlavorTextVersion_group :: NamedAPIResource -- ^ The version group that uses this flavor text
  } deriving (Eq, Ord, Show)

instance FromJSON MoveFlavorText where
  parseJSON (Object o) =
    MoveFlavorText
      <$> o .: "flavor_text"
      <*> o .: "language"
      <*> o .: "version_group"
  parseJSON _ = mzero

data MoveMetaData
  = MoveMetaData {
      moveMetaDataAilment :: NamedAPIResource -- ^ The status ailment this move inflicts on its target
    , moveMetaDataCategory :: NamedAPIResource -- ^ The category of move this move falls under, e.g. damage or ailment
    , moveMetaDataMin_hits :: Integer -- ^ The minimum number of times this move hits. Null if it always only hits once.
    , moveMetaDataMax_hits :: Integer -- ^ The maximum number of times this move hits. Null if it always only hits once.
    , moveMetaDataMin_turns :: Integer -- ^ The minimum number of turns this move continues to take effect. Null if it always only lasts one turn.
    , moveMetaDataMax_turns :: Integer -- ^ The maximum number of turns this move continues to take effect. Null if it always only lasts one turn.
    , moveMetaDataDrain :: Integer -- ^ HP drain (if positive) or Recoil damage (if negative), in percent of damage done
    , moveMetaDataHealing :: Integer -- ^ The amount of hp gained by the attacking Pokemon, in percent of it's maximum HP
    , moveMetaDataCrit_rate :: Integer -- ^ Critical hit rate bonus
    , moveMetaDataAilment_chance :: Integer -- ^ The likelihood this attack will cause an ailment
    , moveMetaDataFlinch_chance :: Integer -- ^ The likelihood this attack will cause the target Pokémon to flinch
    , moveMetaDataStat_chance :: Integer -- ^ The likelihood this attack will cause a stat change in the target Pokémon
  } deriving (Eq, Ord, Show)

instance FromJSON MoveMetaData where
  parseJSON (Object o) =
    MoveMetaData
      <$> o .: "ailment"
      <*> o .: "category"
      <*> o .: "min_hits"
      <*> o .: "max_hits"
      <*> o .: "min_turns"
      <*> o .: "max_turns"
      <*> o .: "drain"
      <*> o .: "healing"
      <*> o .: "crit_rate"
      <*> o .: "ailment_chance"
      <*> o .: "flinch_chance"
      <*> o .: "stat_chance"
  parseJSON _ = mzero

data MoveStatChange
  = MoveStatChange {
      moveStatChangeChange :: Integer -- ^ The amount of change
    , moveStatChangeStat :: NamedAPIResource -- ^ The stat being affected
  } deriving (Eq, Ord, Show)

instance FromJSON MoveStatChange where
  parseJSON (Object o) =
    MoveStatChange
      <$> o .: "change"
      <*> o .: "stat"
  parseJSON _ = mzero

data PastMoveStatValues
  = PastMoveStatValues {
      pastMoveStatValuesAccuracy :: Integer -- ^ The percent value of how likely this move is to be successful
    , pastMoveStatValuesEffect_chance :: Integer -- ^ The percent value of how likely it is this moves effect will take effect
    , pastMoveStatValuesPower :: Integer -- ^ The base power of this move with a value of 0 if it does not have a base power
    , pastMoveStatValuesPp :: Integer -- ^ Power points. The number of times this move can be used
    , pastMoveStatValuesEffect_entries :: [VerboseEffect] -- ^ The effect of this move listed in different languages
    , pastMoveStatValuesType' :: NamedAPIResource -- ^ The elemental type of this move
    , pastMoveStatValuesVersion_group :: NamedAPIResource -- ^ The version group in which these move stat values were in effect
  } deriving (Eq, Ord, Show)

instance FromJSON PastMoveStatValues where
  parseJSON (Object o) =
    PastMoveStatValues
      <$> o .: "accuracy"
      <*> o .: "effect_chance"
      <*> o .: "power"
      <*> o .: "pp"
      <*> o .: "effect_entries"
      <*> o .: "type"
      <*> o .: "version_group"
  parseJSON _ = mzero

data MoveAilment
  = MoveAilment {
      moveAilmentId :: Integer -- ^ The identifier for this move ailment resource
    , moveAilmentName :: String -- ^ The name for this move ailment resource
    , moveAilmentMoves :: [NamedAPIResource] -- ^ A list of moves that cause this ailment
    , moveAilmentNames :: [Name] -- ^ The name of this move ailment listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON MoveAilment where
  parseJSON (Object o) =
    MoveAilment
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "moves"
      <*> o .: "names"
  parseJSON _ = mzero

data MoveBattleStyle
  = MoveBattleStyle {
      moveBattleStyleId :: Integer -- ^ The identifier for this move battle style resource
    , moveBattleStyleName :: String -- ^ The name for this move battle style resource
    , moveBattleStyleNames :: [Name] -- ^ The name of this move battle style listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON MoveBattleStyle where
  parseJSON (Object o) =
    MoveBattleStyle
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "names"
  parseJSON _ = mzero

data MoveCategory
  = MoveCategory {
      moveCategoryId :: Integer -- ^ The identifier for this move category resource
    , moveCategoryName :: String -- ^ The name for this move category resource
    , moveCategoryMoves :: [NamedAPIResource] -- ^ A list of moves that fall into this category
    , moveCategoryDescriptions :: [Description] -- ^ The description of this move ailment listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON MoveCategory where
  parseJSON (Object o) =
    MoveCategory
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "moves"
      <*> o .: "descriptions"
  parseJSON _ = mzero

data MoveDamageClass
  = MoveDamageClass {
      moveDamageClassId :: Integer -- ^ The identifier for this move damage class resource
    , moveDamageClassName :: String -- ^ The name for this move damage class resource
    , moveDamageClassDescriptions :: [Description] -- ^ The description of this move damage class listed in different languages
    , moveDamageClassMoves :: [NamedAPIResource] -- ^ A list of moves that fall into this damage class
    , moveDamageClassNames :: [Name] -- ^ The name of this move damage class listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON MoveDamageClass where
  parseJSON (Object o) =
    MoveDamageClass
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "descriptions"
      <*> o .: "moves"
      <*> o .: "names"
  parseJSON _ = mzero

data MoveLearnMethod
  = MoveLearnMethod {
      moveLearnMethodId :: Integer -- ^ The identifier for this move learn method resource
    , moveLearnMethodName :: String -- ^ The name for this move learn method resource
    , moveLearnMethodDescriptions :: [Description] -- ^ The description of this move learn method listed in different languages
    , moveLearnMethodNames :: [Name] -- ^ The name of this move learn method listed in different languages
    , moveLearnMethodVersion_groups :: [NamedAPIResource] -- ^ A list of version groups where moves can be learned through this method
  } deriving (Eq, Ord, Show)

instance FromJSON MoveLearnMethod where
  parseJSON (Object o) =
    MoveLearnMethod
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "descriptions"
      <*> o .: "names"
      <*> o .: "version_groups"
  parseJSON _ = mzero

data MoveTarget
  = MoveTarget {
      moveTargetId :: Integer -- ^ The identifier for this move target resource
    , moveTargetName :: String -- ^ The name for this move target resource
    , moveTargetDescriptions :: [Description] -- ^ The description of this move target listed in different languages
    , moveTargetMoves :: [NamedAPIResource] -- ^ A list of moves that that are directed at this target
    , moveTargetNames :: [Name] -- ^ The name of this move target listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON MoveTarget where
  parseJSON (Object o) =
    MoveTarget
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "descriptions"
      <*> o .: "moves"
      <*> o .: "names"
  parseJSON _ = mzero

data Location
  = Location {
      locationId :: Integer -- ^ The identifier for this location resource
    , locationName :: String -- ^ The name for this location resource
    , locationRegion :: NamedAPIResource -- ^ The region this location can be found in
    , locationNames :: [Name] -- ^ The name of this language listed in different languages
    , locationGame_indices :: [GenerationGameIndex] -- ^ A list of game indices relevent to this location by generation
    , locationAreas :: [NamedAPIResource] -- ^ Areas that can be found within this location
  } deriving (Eq, Ord, Show)

instance FromJSON Location where
  parseJSON (Object o) =
    Location
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "region"
      <*> o .: "names"
      <*> o .: "game_indices"
      <*> o .: "areas"
  parseJSON _ = mzero

data LocationArea
  = LocationArea {
      locationAreaId :: Integer -- ^ The identifier for this location resource
    , locationAreaName :: String -- ^ The name for this location resource
    , locationAreaGame_index :: Integer -- ^ The internal id of an API resource within game data
    , locationAreaEncounter_method_rates :: [EncounterMethodRate] -- ^ A list of methods in which Pokémon may be encountered in this area and how likely the method will occur depending on the version of the game
    , locationAreaLocation :: NamedAPIResource -- ^ The region this location can be found in
    , locationAreaNames :: [Name] -- ^ The name of this location area listed in different languages
    , locationAreaPokemon_encounters :: [PokemonEncounter] -- ^ A list of Pokémon that can be encountered in this area along with version specific details about the encounter
  } deriving (Eq, Ord, Show)

instance FromJSON LocationArea where
  parseJSON (Object o) =
    LocationArea
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "game_index"
      <*> o .: "encounter_method_rates"
      <*> o .: "location"
      <*> o .: "names"
      <*> o .: "pokemon_encounters"
  parseJSON _ = mzero

data EncounterMethodRate
  = EncounterMethodRate {
      encounterMethodRateEncounter_method :: NamedAPIResource -- ^ The method in which Pokémon may be encountered in an area.
    , encounterMethodRateVersion_details :: [EncounterVersionDetails] -- ^ The chance of the encounter to occur on a version of the game.
  } deriving (Eq, Ord, Show)

instance FromJSON EncounterMethodRate where
  parseJSON (Object o) =
    EncounterMethodRate
      <$> o .: "encounter_method"
      <*> o .: "version_details"
  parseJSON _ = mzero

data EncounterVersionDetails
  = EncounterVersionDetails {
      encounterVersionDetailsRate :: Integer -- ^ The chance of an encounter to occur.
    , encounterVersionDetailsVersion :: NamedAPIResource -- ^ The version of the game in which the encounter can occur with the given chance.
  } deriving (Eq, Ord, Show)

instance FromJSON EncounterVersionDetails where
  parseJSON (Object o) =
    EncounterVersionDetails
      <$> o .: "rate"
      <*> o .: "version"
  parseJSON _ = mzero

data PokemonEncounter
  = PokemonEncounter {
      pokemonEncounterPokemon :: NamedAPIResource -- ^ The Pokémon being encountered
    , pokemonEncounterVersion_details :: [VersionEncounterDetail] -- ^ A list of versions and encounters with Pokémon that might happen in the referenced location area
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonEncounter where
  parseJSON (Object o) =
    PokemonEncounter
      <$> o .: "pokemon"
      <*> o .: "version_details"
  parseJSON _ = mzero

data PalParkArea
  = PalParkArea {
      palParkAreaId :: Integer -- ^ The identifier for this pal park area resource
    , palParkAreaName :: String -- ^ The name for this pal park area resource
    , palParkAreaNames :: [Name] -- ^ The name of this pal park area listed in different languages
    , palParkAreaPokemon_encounters :: [PalParkEncounterSpecies] -- ^ A list of Pokémon encountered in thi pal park area along with details
  } deriving (Eq, Ord, Show)

instance FromJSON PalParkArea where
  parseJSON (Object o) =
    PalParkArea
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "names"
      <*> o .: "pokemon_encounters"
  parseJSON _ = mzero

data PalParkEncounterSpecies
  = PalParkEncounterSpecies {
      palParkEncounterSpeciesBase_score :: Integer -- ^ The base score given to the player when this Pokémon is caught during a pal park run
    , palParkEncounterSpeciesRate :: Integer -- ^ The base rate for encountering this Pokémon in this pal park area
    , palParkEncounterSpeciesPokemon_species :: NamedAPIResource -- ^ The Pokémon species being encountered
  } deriving (Eq, Ord, Show)

instance FromJSON PalParkEncounterSpecies where
  parseJSON (Object o) =
    PalParkEncounterSpecies
      <$> o .: "base_score"
      <*> o .: "rate"
      <*> o .: "pokemon_species"
  parseJSON _ = mzero

data Region
  = Region {
      regionId :: Integer -- ^ The identifier for this region resource
    , regionName :: String -- ^ The name for this region resource
    , regionLocations :: [NamedAPIResource] -- ^ A list of locations that can be found in this region
    , regionMain_generation :: NamedAPIResource -- ^ The generation this region was introduced in
    , regionNames :: [Name] -- ^ The name of this region listed in different languages
    , regionPokedexes :: [NamedAPIResource] -- ^ A list of pokédexes that catalogue Pokémon in this region
    , regionVersion_groups :: [NamedAPIResource] -- ^ A list of version groups where this region can be visited
  } deriving (Eq, Ord, Show)

instance FromJSON Region where
  parseJSON (Object o) =
    Region
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "locations"
      <*> o .: "main_generation"
      <*> o .: "names"
      <*> o .: "pokedexes"
      <*> o .: "version_groups"
  parseJSON _ = mzero

data Ability
  = Ability {
      abilityId :: Integer -- ^ The identifier for this ability resource
    , abilityName :: String -- ^ The name for this ability resource
    , abilityIs_main_series :: Bool -- ^ Whether or not this ability originated in the main series of the video games
    , abilityGeneration :: NamedAPIResource -- ^ The generation this ability originated in
    , abilityNames :: [Name] -- ^ The name of this ability listed in different languages
    , abilityEffect_entries :: [VerboseEffect] -- ^ The effect of this ability listed in different languages
    , abilityEffect_changes :: [AbilityEffectChange] -- ^ The list of previous effects this ability has had across version groups
    , abilityFlavor_text_entries :: [AbilityFlavorText] -- ^ The flavor text of this ability listed in different languages
    , abilityPokemon :: [AbilityPokemon] -- ^ A list of Pokémon that could potentially have this ability
  } deriving (Eq, Ord, Show)

instance FromJSON Ability where
  parseJSON (Object o) =
    Ability
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "is_main_series"
      <*> o .: "generation"
      <*> o .: "names"
      <*> o .: "effect_entries"
      <*> o .: "effect_changes"
      <*> o .: "flavor_text_entries"
      <*> o .: "pokemon"
  parseJSON _ = mzero

data AbilityEffectChange
  = AbilityEffectChange {
      abilityEffectChangeEffect_entries :: [Effect] -- ^ The previous effect of this ability listed in different languages
    , abilityEffectChangeVersion_group :: NamedAPIResource -- ^ The version group in which the previous effect of this ability originated
  } deriving (Eq, Ord, Show)

instance FromJSON AbilityEffectChange where
  parseJSON (Object o) =
    AbilityEffectChange
      <$> o .: "effect_entries"
      <*> o .: "version_group"
  parseJSON _ = mzero

data AbilityFlavorText
  = AbilityFlavorText {
      abilityFlavorTextFlavor_text :: String -- ^ The localized name for an API resource in a specific language
    , abilityFlavorTextLanguage :: NamedAPIResource -- ^ The language this name is in
    , abilityFlavorTextVersion_group :: NamedAPIResource -- ^ The version group that uses this flavor text
  } deriving (Eq, Ord, Show)

instance FromJSON AbilityFlavorText where
  parseJSON (Object o) =
    AbilityFlavorText
      <$> o .: "flavor_text"
      <*> o .: "language"
      <*> o .: "version_group"
  parseJSON _ = mzero

data AbilityPokemon
  = AbilityPokemon {
      abilityPokemonIs_hidden :: Bool -- ^ Whether or not this a hidden ability for the referenced Pokémon
    , abilityPokemonSlot :: Integer -- ^ Pokémon have 3 ability 'slots' which hold references to possible abilities they could have. This is the slot of this ability for the referenced pokemon.
    , abilityPokemonPokemon :: NamedAPIResource -- ^ The Pokémon this ability could belong to
  } deriving (Eq, Ord, Show)

instance FromJSON AbilityPokemon where
  parseJSON (Object o) =
    AbilityPokemon
      <$> o .: "is_hidden"
      <*> o .: "slot"
      <*> o .: "pokemon"
  parseJSON _ = mzero

data Characteristic
  = Characteristic {
      characteristicId :: Integer -- ^ The identifier for this characteristic resource
    , characteristicGene_modulo :: Integer -- ^ The remainder of the highest stat/IV divided by 5
    , characteristicPossible_values :: [Integer] -- ^ The possible values of the highest stat that would result in a Pokémon recieving this characteristic when divided by 5
    , characteristicDescriptions :: [Description] -- ^ The descriptions of this characteristic listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON Characteristic where
  parseJSON (Object o) =
    Characteristic
      <$> o .: "id"
      <*> o .: "gene_modulo"
      <*> o .: "possible_values"
      <*> o .: "descriptions"
  parseJSON _ = mzero

data EggGroup
  = EggGroup {
      eggGroupId :: Integer -- ^ The identifier for this egg group resource
    , eggGroupName :: String -- ^ The name for this egg group resource
    , eggGroupNames :: [Name] -- ^ The name of this egg group listed in different languages
    , eggGroupPokemon_species :: [NamedAPIResource] -- ^ A list of all Pokémon species that are members of this egg group
  } deriving (Eq, Ord, Show)

instance FromJSON EggGroup where
  parseJSON (Object o) =
    EggGroup
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "names"
      <*> o .: "pokemon_species"
  parseJSON _ = mzero

data Gender
  = Gender {
      genderId :: Integer -- ^ The identifier for this gender resource
    , genderName :: String -- ^ The name for this gender resource
    , genderPokemon_species_details :: [PokemonSpeciesGender] -- ^ A list of Pokémon species that can be this gender and how likely it is that they will be
    , genderRequired_for_evolution :: [NamedAPIResource] -- ^ A list of Pokémon species that required this gender in order for a Pokémon to evolve into them
  } deriving (Eq, Ord, Show)

instance FromJSON Gender where
  parseJSON (Object o) =
    Gender
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "pokemon_species_details"
      <*> o .: "required_for_evolution"
  parseJSON _ = mzero

data PokemonSpeciesGender
  = PokemonSpeciesGender {
      pokemonSpeciesGenderRate :: Integer -- ^ The chance of this Pokémon being female, in eighths; or -1 for genderless
    , pokemonSpeciesGenderPokemon_species :: NamedAPIResource -- ^ A Pokémon species that can be the referenced gender
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonSpeciesGender where
  parseJSON (Object o) =
    PokemonSpeciesGender
      <$> o .: "rate"
      <*> o .: "pokemon_species"
  parseJSON _ = mzero

data GrowthRate
  = GrowthRate {
      growthRateId :: Integer -- ^ The identifier for this gender resource
    , growthRateName :: String -- ^ The name for this gender resource
    , growthRateFormula :: String -- ^ The formula used to calculate the rate at which the Pokémon species gains level
    , growthRateDescriptions :: [Description] -- ^ The descriptions of this characteristic listed in different languages
    , growthRateLevels :: [GrowthRateExperienceLevel] -- ^ A list of levels and the amount of experienced needed to atain them based on this growth rate
    , growthRatePokemon_species :: [NamedAPIResource] -- ^ A list of Pokémon species that gain levels at this growth rate
  } deriving (Eq, Ord, Show)

instance FromJSON GrowthRate where
  parseJSON (Object o) =
    GrowthRate
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "formula"
      <*> o .: "descriptions"
      <*> o .: "levels"
      <*> o .: "pokemon_species"
  parseJSON _ = mzero

data GrowthRateExperienceLevel
  = GrowthRateExperienceLevel {
      growthRateExperienceLevelLevel :: Integer -- ^ The level gained
    , growthRateExperienceLevelExperience :: Integer -- ^ The amount of experience required to reach the referenced level
  } deriving (Eq, Ord, Show)

instance FromJSON GrowthRateExperienceLevel where
  parseJSON (Object o) =
    GrowthRateExperienceLevel
      <$> o .: "level"
      <*> o .: "experience"
  parseJSON _ = mzero

data Nature
  = Nature {
      natureId :: Integer -- ^ The identifier for this nature resource
    , natureName :: String -- ^ The name for this nature resource
    , natureDecreased_stat :: NamedAPIResource -- ^ The stat decreased by 10% in Pokémon with this nature
    , natureIncreased_stat :: NamedAPIResource -- ^ The stat increased by 10% in Pokémon with this nature
    , natureHates_flavor :: NamedAPIResource -- ^ The flavor hated by Pokémon with this nature
    , natureLikes_flavor :: NamedAPIResource -- ^ The flavor liked by Pokémon with this nature
    , naturePokeathlon_stat_changes :: [NatureStatChange] -- ^ A list of Pokéathlon stats this nature effects and how much it effects them
    , natureMove_battle_style_preferences :: [MoveBattleStylePreference] -- ^ A list of battle styles and how likely a Pokémon with this nature is to use them in the Battle Palace or Battle Tent.
    , natureNames :: [Name] -- ^ The name of this nature listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON Nature where
  parseJSON (Object o) =
    Nature
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "decreased_stat"
      <*> o .: "increased_stat"
      <*> o .: "hates_flavor"
      <*> o .: "likes_flavor"
      <*> o .: "pokeathlon_stat_changes"
      <*> o .: "move_battle_style_preferences"
      <*> o .: "names"
  parseJSON _ = mzero

data NatureStatChange
  = NatureStatChange {
      natureStatChangeMax_change :: Integer -- ^ The amount of change
    , natureStatChangePokeathlon_stat :: NamedAPIResource -- ^ The stat being affected
  } deriving (Eq, Ord, Show)

instance FromJSON NatureStatChange where
  parseJSON (Object o) =
    NatureStatChange
      <$> o .: "max_change"
      <*> o .: "pokeathlon_stat"
  parseJSON _ = mzero

data MoveBattleStylePreference
  = MoveBattleStylePreference {
      moveBattleStylePreferenceLow_hp_preference :: Integer -- ^ Chance of using the move, in percent, if HP is under one half
    , moveBattleStylePreferenceHigh_hp_preference :: Integer -- ^ Chance of using the move, in percent, if HP is over one half
    , moveBattleStylePreferenceMove_battle_style :: NamedAPIResource -- ^ The move battle style
  } deriving (Eq, Ord, Show)

instance FromJSON MoveBattleStylePreference where
  parseJSON (Object o) =
    MoveBattleStylePreference
      <$> o .: "low_hp_preference"
      <*> o .: "high_hp_preference"
      <*> o .: "move_battle_style"
  parseJSON _ = mzero

data PokeathlonStat
  = PokeathlonStat {
      pokeathlonStatId :: Integer -- ^ The identifier for this Pokéathlon stat resource
    , pokeathlonStatName :: String -- ^ The name for this Pokéathlon stat resource
    , pokeathlonStatNames :: [Name] -- ^ The name of this Pokéathlon stat listed in different languages
    , pokeathlonStatAffecting_natures :: NaturePokeathlonStatAffectSets -- ^ A detail of natures which affect this Pokéathlon stat positively or negatively
  } deriving (Eq, Ord, Show)

instance FromJSON PokeathlonStat where
  parseJSON (Object o) =
    PokeathlonStat
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "names"
      <*> o .: "affecting_natures"
  parseJSON _ = mzero

data NaturePokeathlonStatAffectSets
  = NaturePokeathlonStatAffectSets {
      naturePokeathlonStatAffectSetsIncrease :: [NaturePokeathlonStatAffect] -- ^ A list of natures and how they change the referenced Pokéathlon stat
    , naturePokeathlonStatAffectSetsDecrease :: [NaturePokeathlonStatAffect] -- ^ A list of natures and how they change the referenced Pokéathlon stat
  } deriving (Eq, Ord, Show)

instance FromJSON NaturePokeathlonStatAffectSets where
  parseJSON (Object o) =
    NaturePokeathlonStatAffectSets
      <$> o .: "increase"
      <*> o .: "decrease"
  parseJSON _ = mzero

data NaturePokeathlonStatAffect
  = NaturePokeathlonStatAffect {
      naturePokeathlonStatAffectMax_change :: Integer -- ^ The maximum amount of change to the referenced Pokéathlon stat
    , naturePokeathlonStatAffectNature :: NamedAPIResource -- ^ The nature causing the change
  } deriving (Eq, Ord, Show)

instance FromJSON NaturePokeathlonStatAffect where
  parseJSON (Object o) =
    NaturePokeathlonStatAffect
      <$> o .: "max_change"
      <*> o .: "nature"
  parseJSON _ = mzero

data Pokemon
  = Pokemon {
      pokemonId :: Integer -- ^ The identifier for this Pokémon resource
    , pokemonName :: String -- ^ The name for this Pokémon resource
    , pokemonBase_experience :: Integer -- ^ The base experience gained for defeating this Pokémon
    , pokemonHeight :: Integer -- ^ The height of this Pokémon
    , pokemonIs_default :: Bool -- ^ Set for exactly one Pokémon used as the default for each species
    , pokemonOrder :: Integer -- ^ Order for sorting. Almost national order, except families are grouped together.
    , pokemonWeight :: Integer -- ^ The weight of this Pokémon
    , pokemonAbilities :: [PokemonAbility] -- ^ A list of abilities this Pokémon could potentially have
    , pokemonForms :: [NamedAPIResource] -- ^ A list of forms this Pokémon can take on
    , pokemonGame_indices :: [VersionGameIndex] -- ^ A list of game indices relevent to Pokémon item by generation
    , pokemonHeld_items :: [PokemonHeldItem] -- ^ A list of items this Pokémon may be holding when encountered
    , pokemonLocation_area_encounters :: String -- ^ A link to a list of location areas as well as encounter details pertaining to specific versions
    , pokemonMoves :: [PokemonMove] -- ^ A list of moves along with learn methods and level details pertaining to specific version groups
    , pokemonSprites :: PokemonSprites -- ^ A set of sprites used to depict this Pokémon in the game
    , pokemonSpecies :: NamedAPIResource -- ^ The species this Pokémon belongs to
    , pokemonStats :: [PokemonStat] -- ^ A list of base stat values for this Pokémon
    , pokemonTypes :: [PokemonType] -- ^ A list of details showing types this Pokémon has
  } deriving (Eq, Ord, Show)

instance FromJSON Pokemon where
  parseJSON (Object o) =
    Pokemon
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "base_experience"
      <*> o .: "height"
      <*> o .: "is_default"
      <*> o .: "order"
      <*> o .: "weight"
      <*> o .: "abilities"
      <*> o .: "forms"
      <*> o .: "game_indices"
      <*> o .: "held_items"
      <*> o .: "location_area_encounters"
      <*> o .: "moves"
      <*> o .: "sprites"
      <*> o .: "species"
      <*> o .: "stats"
      <*> o .: "types"
  parseJSON _ = mzero

data PokemonAbility
  = PokemonAbility {
      pokemonAbilityIs_hidden :: Bool -- ^ Whether or not this is a hidden ability
    , pokemonAbilitySlot :: Integer -- ^ The slot this ability occupies in this Pokémon species
    , pokemonAbilityAbility :: NamedAPIResource -- ^ The ability the Pokémon may have
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonAbility where
  parseJSON (Object o) =
    PokemonAbility
      <$> o .: "is_hidden"
      <*> o .: "slot"
      <*> o .: "ability"
  parseJSON _ = mzero

data PokemonType
  = PokemonType {
      pokemonTypeSlot :: Integer -- ^ The order the Pokémon's types are listed in
    , pokemonTypeType' :: NamedAPIResource -- ^ The type the referenced Pokémon has
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonType where
  parseJSON (Object o) =
    PokemonType
      <$> o .: "slot"
      <*> o .: "type"
  parseJSON _ = mzero

data PokemonHeldItem
  = PokemonHeldItem {
      pokemonHeldItemItem :: NamedAPIResource -- ^ The item the referenced Pokémon holds
    , pokemonHeldItemVersion_details :: [PokemonHeldItemVersion] -- ^ The details of the different versions in which the item is held
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonHeldItem where
  parseJSON (Object o) =
    PokemonHeldItem
      <$> o .: "item"
      <*> o .: "version_details"
  parseJSON _ = mzero

data PokemonHeldItemVersion
  = PokemonHeldItemVersion {
      pokemonHeldItemVersionVersion :: NamedAPIResource -- ^ The version in which the item is held
    , pokemonHeldItemVersionRarity :: Integer -- ^ How often the item is held
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonHeldItemVersion where
  parseJSON (Object o) =
    PokemonHeldItemVersion
      <$> o .: "version"
      <*> o .: "rarity"
  parseJSON _ = mzero

data PokemonMove
  = PokemonMove {
      pokemonMoveMove :: NamedAPIResource -- ^ The move the Pokémon can learn
    , pokemonMoveVersion_group_details :: [PokemonMoveVersion] -- ^ The details of the version in which the Pokémon can learn the move
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonMove where
  parseJSON (Object o) =
    PokemonMove
      <$> o .: "move"
      <*> o .: "version_group_details"
  parseJSON _ = mzero

data PokemonMoveVersion
  = PokemonMoveVersion {
      pokemonMoveVersionMove_learn_method :: NamedAPIResource -- ^ The method by which the move is learned
    , pokemonMoveVersionVersion_group :: NamedAPIResource -- ^ The version group in which the move is learned
    , pokemonMoveVersionLevel_learned_at :: Integer -- ^ The minimum level to learn the move
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonMoveVersion where
  parseJSON (Object o) =
    PokemonMoveVersion
      <$> o .: "move_learn_method"
      <*> o .: "version_group"
      <*> o .: "level_learned_at"
  parseJSON _ = mzero

data PokemonStat
  = PokemonStat {
      pokemonStatStat :: NamedAPIResource -- ^ The stat the Pokémon has
    , pokemonStatEffort :: Integer -- ^ The effort points (EV) the Pokémon has in the stat
    , pokemonStatBase_stat :: Integer -- ^ The base value of the stst
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonStat where
  parseJSON (Object o) =
    PokemonStat
      <$> o .: "stat"
      <*> o .: "effort"
      <*> o .: "base_stat"
  parseJSON _ = mzero

data PokemonSprites
  = PokemonSprites {
      pokemonSpritesFront_default :: String -- ^ The default depiction of this Pokémon from the front in battle
    , pokemonSpritesFront_shiny :: String -- ^ The shiny depiction of this Pokémon from the front in battle
    , pokemonSpritesFront_female :: Maybe String -- ^ The female depiction of this Pokémon from the front in battle
    , pokemonSpritesFront_shiny_female :: Maybe String -- ^ The shiny female depiction of this Pokémon from the front in battle
    , pokemonSpritesBack_default :: String -- ^ The default depiction of this Pokémon from the back in battle
    , pokemonSpritesBack_shiny :: String -- ^ The shiny depiction of this Pokémon from the back in battle
    , pokemonSpritesBack_female :: Maybe String -- ^ The female depiction of this Pokémon from the back in battle
    , pokemonSpritesBack_shiny_female :: Maybe String -- ^ The shiny female depiction of this Pokémon from the back in battle
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonSprites where
  parseJSON (Object o) =
    PokemonSprites
      <$> o .: "front_default"
      <*> o .: "front_shiny"
      <*> o .: "front_female"
      <*> o .: "front_shiny_female"
      <*> o .: "back_default"
      <*> o .: "back_shiny"
      <*> o .: "back_female"
      <*> o .: "back_shiny_female"
  parseJSON _ = mzero

data LocationAreaEncounter
  = LocationAreaEncounter {
      locationAreaEncounterLocation_area :: NamedAPIResource -- ^ The location area the referenced Pokémon can be encountered in
    , locationAreaEncounterVersion_details :: [VersionEncounterDetail] -- ^ A list of versions and encounters with the referenced Pokémon that might happen
  } deriving (Eq, Ord, Show)

instance FromJSON LocationAreaEncounter where
  parseJSON (Object o) =
    LocationAreaEncounter
      <$> o .: "location_area"
      <*> o .: "version_details"
  parseJSON _ = mzero

data PokemonColor
  = PokemonColor {
      pokemonColorId :: Integer -- ^ The identifier for this Pokémon color resource
    , pokemonColorName :: String -- ^ The name for this Pokémon color resource
    , pokemonColorNames :: [Name] -- ^ The name of this Pokémon color listed in different languages
    , pokemonColorPokemon_species :: [NamedAPIResource] -- ^ A list of the Pokémon species that have this color
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonColor where
  parseJSON (Object o) =
    PokemonColor
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "names"
      <*> o .: "pokemon_species"
  parseJSON _ = mzero

data PokemonForm
  = PokemonForm {
      pokemonFormId :: Integer -- ^ The identifier for this Pokémon form resource
    , pokemonFormName :: String -- ^ The name for this Pokémon form resource
    , pokemonFormOrder :: Integer -- ^ The order in which forms should be sorted within all forms. Multiple forms may have equal order, in which case they should fall back on sorting by name.
    , pokemonFormForm_order :: Integer -- ^ The order in which forms should be sorted within a species' forms
    , pokemonFormIs_default :: Bool -- ^ True for exactly one form used as the default for each Pokémon
    , pokemonFormIs_battle_only :: Bool -- ^ Whether or not this form can only happen during battle
    , pokemonFormIs_mega :: Bool -- ^ Whether or not this form requires mega evolution
    , pokemonFormForm_name :: String -- ^ The name of this form
    , pokemonFormPokemon :: NamedAPIResource -- ^ The Pokémon that can take on this form
    , pokemonFormSprites :: PokemonFormSprites -- ^ A set of sprites used to depict this Pokémon form in the game
    , pokemonFormVersion_group :: NamedAPIResource -- ^ The version group this Pokémon form was introduced in
    , pokemonFormNames :: [Name] -- ^ The form specific full name of this Pokémon form, or empty if the form does not have a specific name
    , pokemonFormForm_names :: [Name] -- ^ The form specific form name of this Pokémon form, or empty if the form does not have a specific name
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonForm where
  parseJSON (Object o) =
    PokemonForm
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "order"
      <*> o .: "form_order"
      <*> o .: "is_default"
      <*> o .: "is_battle_only"
      <*> o .: "is_mega"
      <*> o .: "form_name"
      <*> o .: "pokemon"
      <*> o .: "sprites"
      <*> o .: "version_group"
      <*> o .: "names"
      <*> o .: "form_names"
  parseJSON _ = mzero

data PokemonFormSprites
  = PokemonFormSprites {
      pokemonFormSpritesFront_default :: String -- ^ The default depiction of this Pokémon form from the front in battle
    , pokemonFormSpritesFront_shiny :: String -- ^ The shiny depiction of this Pokémon form from the front in battle
    , pokemonFormSpritesBack_default :: String -- ^ The default depiction of this Pokémon form from the back in battle
    , pokemonFormSpritesBack_shiny :: String -- ^ The shiny depiction of this Pokémon form from the back in battle
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonFormSprites where
  parseJSON (Object o) =
    PokemonFormSprites
      <$> o .: "front_default"
      <*> o .: "front_shiny"
      <*> o .: "back_default"
      <*> o .: "back_shiny"
  parseJSON _ = mzero

data PokemonHabitat
  = PokemonHabitat {
      pokemonHabitatId :: Integer -- ^ The identifier for this Pokémon habitat resource
    , pokemonHabitatName :: String -- ^ The name for this Pokémon habitat resource
    , pokemonHabitatNames :: [Name] -- ^ The name of this Pokémon habitat listed in different languages
    , pokemonHabitatPokemon_species :: [NamedAPIResource] -- ^ A list of the Pokémon species that can be found in this habitat
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonHabitat where
  parseJSON (Object o) =
    PokemonHabitat
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "names"
      <*> o .: "pokemon_species"
  parseJSON _ = mzero

data PokemonShape
  = PokemonShape {
      pokemonShapeId :: Integer -- ^ The identifier for this Pokémon shape resource
    , pokemonShapeName :: String -- ^ The name for this Pokémon shape resource
    , pokemonShapeAwesome_names :: [AwesomeName] -- ^ The "scientific" name of this Pokémon shape listed in different languages
    , pokemonShapeNames :: [Name] -- ^ The name of this Pokémon shape listed in different languages
    , pokemonShapePokemon_species :: [NamedAPIResource] -- ^ A list of the Pokémon species that have this shape
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonShape where
  parseJSON (Object o) =
    PokemonShape
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "awesome_names"
      <*> o .: "names"
      <*> o .: "pokemon_species"
  parseJSON _ = mzero

data AwesomeName
  = AwesomeName {
      awesomeNameAwesome_name :: String -- ^ The localized "scientific" name for an API resource in a specific language
    , awesomeNameLanguage :: NamedAPIResource -- ^ The language this "scientific" name is in
  } deriving (Eq, Ord, Show)

instance FromJSON AwesomeName where
  parseJSON (Object o) =
    AwesomeName
      <$> o .: "awesome_name"
      <*> o .: "language"
  parseJSON _ = mzero

data PokemonSpecies
  = PokemonSpecies {
      pokemonSpeciesId :: Integer -- ^ The identifier for this Pokémon species resource
    , pokemonSpeciesName :: String -- ^ The name for this Pokémon species resource
    , pokemonSpeciesOrder :: Integer -- ^ The order in which species should be sorted.  Based on National Dex order, except families are grouped together and sorted by stage.
    , pokemonSpeciesGender_rate :: Integer -- ^ The chance of this Pokémon being female, in eighths; or -1 for genderless
    , pokemonSpeciesCapture_rate :: Integer -- ^ The base capture rate; up to 255. The higher the number, the easier the catch.
    , pokemonSpeciesBase_happiness :: Integer -- ^ The happiness when caught by a normal Pokéball; up to 255. The higher the number, the happier the Pokémon.
    , pokemonSpeciesIs_baby :: Bool -- ^ Whether or not this is a baby Pokémon
    , pokemonSpeciesHatch_counter :: Integer -- ^ Initial hatch counter: one must walk 255 × (hatch_counter + 1) steps before this Pokémon's egg hatches, unless utilizing bonuses like Flame Body's
    , pokemonSpeciesHas_gender_differences :: Bool -- ^ Whether or not this Pokémon has visual gender differences
    , pokemonSpeciesForms_switchable :: Bool -- ^ Whether or not this Pokémon has multiple forms and can switch between them
    , pokemonSpeciesGrowth_rate :: NamedAPIResource -- ^ The rate at which this Pokémon species gains levels
    , pokemonSpeciesPokedex_numbers :: [PokemonSpeciesDexEntry] -- ^ A list of Pokedexes and the indexes reserved within them for this Pokémon species
    , pokemonSpeciesEgg_groups :: [NamedAPIResource] -- ^ A list of egg groups this Pokémon species is a member of
    , pokemonSpeciesColor :: NamedAPIResource -- ^ The color of this Pokémon for gimmicky Pokédex search
    , pokemonSpeciesShape :: NamedAPIResource -- ^ The shape of this Pokémon for gimmicky Pokédex search
    , pokemonSpeciesEvolves_from_species :: NamedAPIResource -- ^ The Pokémon species that evolves into this Pokemon_species
    , pokemonSpeciesEvolution_chain :: APIResource -- ^ The evolution chain this Pokémon species is a member of
    , pokemonSpeciesHabitat :: NamedAPIResource -- ^ The habitat this Pokémon species can be encountered in
    , pokemonSpeciesGeneration :: NamedAPIResource -- ^ The generation this Pokémon species was introduced in
    , pokemonSpeciesNames :: [Name] -- ^ The name of this Pokémon species listed in different languages
    , pokemonSpeciesPal_park_encounters :: [PalParkEncounterArea] -- ^ A list of encounters that can be had with this Pokémon species in pal park
    , pokemonSpeciesFlavor_text_entries :: [FlavorText] -- ^ A list of flavor text entries for this Pokémon species
    , pokemonSpeciesForm_descriptions :: [Description] -- ^ Descriptions of different forms Pokémon take on within the Pokémon species
    , pokemonSpeciesGenera :: [Genus] -- ^ The genus of this Pokémon species listed in multiple languages
    , pokemonSpeciesVarieties :: [PokemonSpeciesVariety] -- ^ A list of the Pokémon that exist within this Pokémon species
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonSpecies where
  parseJSON (Object o) =
    PokemonSpecies
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "order"
      <*> o .: "gender_rate"
      <*> o .: "capture_rate"
      <*> o .: "base_happiness"
      <*> o .: "is_baby"
      <*> o .: "hatch_counter"
      <*> o .: "has_gender_differences"
      <*> o .: "forms_switchable"
      <*> o .: "growth_rate"
      <*> o .: "pokedex_numbers"
      <*> o .: "egg_groups"
      <*> o .: "color"
      <*> o .: "shape"
      <*> o .: "evolves_from_species"
      <*> o .: "evolution_chain"
      <*> o .: "habitat"
      <*> o .: "generation"
      <*> o .: "names"
      <*> o .: "pal_park_encounters"
      <*> o .: "flavor_text_entries"
      <*> o .: "form_descriptions"
      <*> o .: "genera"
      <*> o .: "varieties"
  parseJSON _ = mzero

data Genus
  = Genus {
      genusGenus :: String -- ^ The localized genus for the referenced Pokémon species
    , genusLanguage :: NamedAPIResource -- ^ The language this genus is in
  } deriving (Eq, Ord, Show)

instance FromJSON Genus where
  parseJSON (Object o) =
    Genus
      <$> o .: "genus"
      <*> o .: "language"
  parseJSON _ = mzero

data PokemonSpeciesDexEntry
  = PokemonSpeciesDexEntry {
      pokemonSpeciesDexEntryEntry_number :: Integer -- ^ The index number within the Pokédex
    , pokemonSpeciesDexEntryPokedex :: NamedAPIResource -- ^ The Pokédex the referenced Pokémon species can be found in
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonSpeciesDexEntry where
  parseJSON (Object o) =
    PokemonSpeciesDexEntry
      <$> o .: "entry_number"
      <*> o .: "pokedex"
  parseJSON _ = mzero

data PalParkEncounterArea
  = PalParkEncounterArea {
      palParkEncounterAreaBase_score :: Integer -- ^ The base score given to the player when the referenced Pokémon is caught during a pal park run
    , palParkEncounterAreaRate :: Integer -- ^ The base rate for encountering the referenced Pokémon in this pal park area
    , palParkEncounterAreaArea :: NamedAPIResource -- ^ The pal park area where this encounter happens
  } deriving (Eq, Ord, Show)

instance FromJSON PalParkEncounterArea where
  parseJSON (Object o) =
    PalParkEncounterArea
      <$> o .: "base_score"
      <*> o .: "rate"
      <*> o .: "area"
  parseJSON _ = mzero

data PokemonSpeciesVariety
  = PokemonSpeciesVariety {
      pokemonSpeciesVarietyIs_default :: Bool -- ^ Whether this variety is the default variety
    , pokemonSpeciesVarietyPokemon :: NamedAPIResource -- ^ The Pokémon variety
  } deriving (Eq, Ord, Show)

instance FromJSON PokemonSpeciesVariety where
  parseJSON (Object o) =
    PokemonSpeciesVariety
      <$> o .: "is_default"
      <*> o .: "pokemon"
  parseJSON _ = mzero

data Stat
  = Stat {
      statId :: Integer -- ^ The identifier for this stat resource
    , statName :: String -- ^ The name for this stat resource
    , statGame_index :: Integer -- ^ ID the games use for this stat
    , statIs_battle_only :: Bool -- ^ Whether this stat only exists within a battle
    , statAffecting_moves :: MoveStatAffectSets -- ^ A detail of moves which affect this stat positively or negatively
    , statAffecting_natures :: NatureStatAffectSets -- ^ A detail of natures which affect this stat positively or negatively
    , statCharacteristics :: [APIResource] -- ^ A list of characteristics that are set on a Pokémon when its highest base stat is this stat
    , statMove_damage_class :: NamedAPIResource -- ^ The class of damage this stat is directly related to
    , statNames :: [Name] -- ^ The name of this region listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON Stat where
  parseJSON (Object o) =
    Stat
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "game_index"
      <*> o .: "is_battle_only"
      <*> o .: "affecting_moves"
      <*> o .: "affecting_natures"
      <*> o .: "characteristics"
      <*> o .: "move_damage_class"
      <*> o .: "names"
  parseJSON _ = mzero

data MoveStatAffectSets
  = MoveStatAffectSets {
      moveStatAffectSetsIncrease :: [MoveStatAffect] -- ^ A list of moves and how they change the referenced stat
    , moveStatAffectSetsDecrease :: [MoveStatAffect] -- ^ A list of moves and how they change the referenced stat
  } deriving (Eq, Ord, Show)

instance FromJSON MoveStatAffectSets where
  parseJSON (Object o) =
    MoveStatAffectSets
      <$> o .: "increase"
      <*> o .: "decrease"
  parseJSON _ = mzero

data MoveStatAffect
  = MoveStatAffect {
      moveStatAffectChange :: Integer -- ^ The maximum amount of change to the referenced stat
    , moveStatAffectMove :: NamedAPIResource -- ^ The move causing the change
  } deriving (Eq, Ord, Show)

instance FromJSON MoveStatAffect where
  parseJSON (Object o) =
    MoveStatAffect
      <$> o .: "change"
      <*> o .: "move"
  parseJSON _ = mzero

data NatureStatAffectSets
  = NatureStatAffectSets {
      natureStatAffectSetsIncrease :: [NamedAPIResource] -- ^ A list of natures and how they change the referenced stat
    , natureStatAffectSetsDecrease :: [NamedAPIResource] -- ^ A list of nature sand how they change the referenced stat
  } deriving (Eq, Ord, Show)

instance FromJSON NatureStatAffectSets where
  parseJSON (Object o) =
    NatureStatAffectSets
      <$> o .: "increase"
      <*> o .: "decrease"
  parseJSON _ = mzero

data Type
  = Type {
      typeId :: Integer -- ^ The identifier for this type resource
    , typeName :: String -- ^ The name for this type resource
    , typeDamage_relations :: TypeRelations -- ^ A detail of how effective this type is toward others and vice versa
    , typeGame_indices :: [GenerationGameIndex] -- ^ A list of game indices relevent to this item by generation
    , typeGeneration :: NamedAPIResource -- ^ The generation this type was introduced in
    , typeMove_damage_class :: NamedAPIResource -- ^ The class of damage inflicted by this type
    , typeNames :: [Name] -- ^ The name of this type listed in different languages
    , typePokemon :: [TypePokemon] -- ^ A list of details of Pokémon that have this type
    , typeMoves :: [NamedAPIResource] -- ^ A list of moves that have this type
  } deriving (Eq, Ord, Show)

instance FromJSON Type where
  parseJSON (Object o) =
    Type
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "damage_relations"
      <*> o .: "game_indices"
      <*> o .: "generation"
      <*> o .: "move_damage_class"
      <*> o .: "names"
      <*> o .: "pokemon"
      <*> o .: "moves"
  parseJSON _ = mzero

data TypePokemon
  = TypePokemon {
      typePokemonSlot :: Integer -- ^ The order the Pokémon's types are listed in
    , typePokemonPokemon :: NamedAPIResource -- ^ The Pokémon that has the referenced type
  } deriving (Eq, Ord, Show)

instance FromJSON TypePokemon where
  parseJSON (Object o) =
    TypePokemon
      <$> o .: "slot"
      <*> o .: "pokemon"
  parseJSON _ = mzero

data TypeRelations
  = TypeRelations {
      typeRelationsNo_damage_to :: [NamedAPIResource] -- ^ A list of types this type has no effect on
    , typeRelationsHalf_damage_to :: [NamedAPIResource] -- ^ A list of types this type is not very effect against
    , typeRelationsDouble_damage_to :: [NamedAPIResource] -- ^ A list of types this type is very effect against
    , typeRelationsNo_damage_from :: [NamedAPIResource] -- ^ A list of types that have no effect on this type
    , typeRelationsHalf_damage_from :: [NamedAPIResource] -- ^ A list of types that are not very effective against this type
    , typeRelationsDouble_damage_from :: [NamedAPIResource] -- ^ A list of types that are very effective against this type
  } deriving (Eq, Ord, Show)

instance FromJSON TypeRelations where
  parseJSON (Object o) =
    TypeRelations
      <$> o .: "no_damage_to"
      <*> o .: "half_damage_to"
      <*> o .: "double_damage_to"
      <*> o .: "no_damage_from"
      <*> o .: "half_damage_from"
      <*> o .: "double_damage_from"
  parseJSON _ = mzero

data Language
  = Language {
      languageId :: Integer -- ^ The identifier for this language resource
    , languageName :: String -- ^ The name for this language resource
    , languageOfficial :: Bool -- ^ Whether or not the games are published in this language
    , languageIso639 :: String -- ^ The two-letter code of the country where this language is spoken. Note that it is not unique.
    , languageIso3166 :: String -- ^ The two-letter code of the language. Note that it is not unique.
    , languageNames :: [Name] -- ^ The name of this language listed in different languages
  } deriving (Eq, Ord, Show)

instance FromJSON Language where
  parseJSON (Object o) =
    Language
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "official"
      <*> o .: "iso639"
      <*> o .: "iso3166"
      <*> o .: "names"
  parseJSON _ = mzero

data APIResource
  = APIResource {
      aPIResourceUrl :: String -- ^ The URL of the referenced resource
  } deriving (Eq, Ord, Show)

instance FromJSON APIResource where
  parseJSON (Object o) =
    APIResource
      <$> o .: "url"
  parseJSON _ = mzero

data Description
  = Description {
      descriptionDescription :: String -- ^ The localized description for an API resource in a specific language
    , descriptionLanguage :: NamedAPIResource -- ^ The language this name is in
  } deriving (Eq, Ord, Show)

instance FromJSON Description where
  parseJSON (Object o) =
    Description
      <$> o .: "description"
      <*> o .: "language"
  parseJSON _ = mzero

data Effect
  = Effect {
      effectEffect :: String -- ^ The localized effect text for an API resource in a specific language
    , effectLanguage :: NamedAPIResource -- ^ The language this effect is in
  } deriving (Eq, Ord, Show)

instance FromJSON Effect where
  parseJSON (Object o) =
    Effect
      <$> o .: "effect"
      <*> o .: "language"
  parseJSON _ = mzero

data Encounter
  = Encounter {
      encounterMin_level :: Integer -- ^ The lowest level the Pokémon could be encountered at
    , encounterMax_level :: Integer -- ^ The highest level the Pokémon could be encountered at
    , encounterCondition_values :: [NamedAPIResource] -- ^ A list of condition values that must be in effect for this encounter to occur
    , encounterChance :: Integer -- ^ percent chance that this encounter will occur
    , encounterMethod :: NamedAPIResource -- ^ The method by which this encounter happens
  } deriving (Eq, Ord, Show)

instance FromJSON Encounter where
  parseJSON (Object o) =
    Encounter
      <$> o .: "min_level"
      <*> o .: "max_level"
      <*> o .: "condition_values"
      <*> o .: "chance"
      <*> o .: "method"
  parseJSON _ = mzero

data FlavorText
  = FlavorText {
      flavorTextFlavor_text :: String -- ^ The localized flavor text for an API resource in a specific language
    , flavorTextLanguage :: NamedAPIResource -- ^ The language this name is in
  } deriving (Eq, Ord, Show)

instance FromJSON FlavorText where
  parseJSON (Object o) =
    FlavorText
      <$> o .: "flavor_text"
      <*> o .: "language"
  parseJSON _ = mzero

data GenerationGameIndex
  = GenerationGameIndex {
      generationGameIndexGame_index :: Integer -- ^ The internal id of an API resource within game data
    , generationGameIndexGeneration :: NamedAPIResource -- ^ The generation relevent to this game index
  } deriving (Eq, Ord, Show)

instance FromJSON GenerationGameIndex where
  parseJSON (Object o) =
    GenerationGameIndex
      <$> o .: "game_index"
      <*> o .: "generation"
  parseJSON _ = mzero

data MachineVersionDetail
  = MachineVersionDetail {
      machineVersionDetailMachine :: APIResource -- ^ The machine that teaches a move from an item
    , machineVersionDetailVersion_group :: NamedAPIResource -- ^ The version group of this specific machine
  } deriving (Eq, Ord, Show)

instance FromJSON MachineVersionDetail where
  parseJSON (Object o) =
    MachineVersionDetail
      <$> o .: "machine"
      <*> o .: "version_group"
  parseJSON _ = mzero

data Name
  = Name {
      nameName :: String -- ^ The localized name for an API resource in a specific language
    , nameLanguage :: NamedAPIResource -- ^ The language this name is in
  } deriving (Eq, Ord, Show)

instance FromJSON Name where
  parseJSON (Object o) =
    Name
      <$> o .: "name"
      <*> o .: "language"
  parseJSON _ = mzero

data NamedAPIResource
  = NamedAPIResource {
      namedAPIResourceName :: String -- ^ The name of the referenced resource
    , namedAPIResourceUrl :: String -- ^ The URL of the referenced resource
  } deriving (Eq, Ord, Show)

instance FromJSON NamedAPIResource where
  parseJSON (Object o) =
    NamedAPIResource
      <$> o .: "name"
      <*> o .: "url"
  parseJSON _ = mzero

data VerboseEffect
  = VerboseEffect {
      verboseEffectEffect :: String -- ^ The localized effect text for an API resource in a specific language
    , verboseEffectShort_effect :: String -- ^ The localized effect text in brief
    , verboseEffectLanguage :: NamedAPIResource -- ^ The language this effect is in
  } deriving (Eq, Ord, Show)

instance FromJSON VerboseEffect where
  parseJSON (Object o) =
    VerboseEffect
      <$> o .: "effect"
      <*> o .: "short_effect"
      <*> o .: "language"
  parseJSON _ = mzero

data VersionEncounterDetail
  = VersionEncounterDetail {
      versionEncounterDetailVersion :: NamedAPIResource -- ^ The game version this encounter happens in
    , versionEncounterDetailMax_chance :: Integer -- ^ The total percentage of all encounter potential
    , versionEncounterDetailEncounter_details :: [Encounter] -- ^ A list of encounters and their specifics
  } deriving (Eq, Ord, Show)

instance FromJSON VersionEncounterDetail where
  parseJSON (Object o) =
    VersionEncounterDetail
      <$> o .: "version"
      <*> o .: "max_chance"
      <*> o .: "encounter_details"
  parseJSON _ = mzero

data VersionGameIndex
  = VersionGameIndex {
      versionGameIndexGame_index :: Integer -- ^ The internal id of an API resource within game data
    , versionGameIndexVersion :: NamedAPIResource -- ^ The version relevent to this game index
  } deriving (Eq, Ord, Show)

instance FromJSON VersionGameIndex where
  parseJSON (Object o) =
    VersionGameIndex
      <$> o .: "game_index"
      <*> o .: "version"
  parseJSON _ = mzero

data VersionGroupFlavorText
  = VersionGroupFlavorText {
      versionGroupFlavorTextText :: String -- ^ The localized name for an API resource in a specific language
    , versionGroupFlavorTextLanguage :: NamedAPIResource -- ^ The language this name is in
    , versionGroupFlavorTextVersion_group :: NamedAPIResource -- ^ The version group which uses this flavor text
  } deriving (Eq, Ord, Show)

instance FromJSON VersionGroupFlavorText where
  parseJSON (Object o) =
    VersionGroupFlavorText
      <$> o .: "text"
      <*> o .: "language"
      <*> o .: "version_group"
  parseJSON _ = mzero

makeFields ''APIResourceList
makeFields ''NamedAPIResourceList
makeFields ''Berry
makeFields ''BerryFlavorMap
makeFields ''BerryFirmness
makeFields ''BerryFlavor
makeFields ''FlavorBerryMap
makeFields ''ContestType
makeFields ''ContestName
makeFields ''ContestEffect
makeFields ''SuperContestEffect
makeFields ''EncounterMethod
makeFields ''EncounterCondition
makeFields ''EncounterConditionValue
makeFields ''EvolutionChain
makeFields ''ChainLink
makeFields ''EvolutionDetail
makeFields ''EvolutionTrigger
makeFields ''Generation
makeFields ''Pokedex
makeFields ''PokemonEntry
makeFields ''Version
makeFields ''VersionGroup
makeFields ''Item
makeFields ''ItemSprites
makeFields ''ItemHolderPokemon
makeFields ''ItemHolderPokemonVersionDetail
makeFields ''ItemAttribute
makeFields ''ItemCategory
makeFields ''ItemFlingEffect
makeFields ''ItemPocket
makeFields ''Move
makeFields ''ContestComboSets
makeFields ''ContestComboDetail
makeFields ''MoveFlavorText
makeFields ''MoveMetaData
makeFields ''MoveStatChange
makeFields ''PastMoveStatValues
makeFields ''MoveAilment
makeFields ''MoveBattleStyle
makeFields ''MoveCategory
makeFields ''MoveDamageClass
makeFields ''MoveLearnMethod
makeFields ''MoveTarget
makeFields ''Location
makeFields ''LocationArea
makeFields ''EncounterMethodRate
makeFields ''EncounterVersionDetails
makeFields ''PokemonEncounter
makeFields ''PalParkArea
makeFields ''PalParkEncounterSpecies
makeFields ''Region
makeFields ''Ability
makeFields ''AbilityEffectChange
makeFields ''AbilityFlavorText
makeFields ''AbilityPokemon
makeFields ''Characteristic
makeFields ''EggGroup
makeFields ''Gender
makeFields ''PokemonSpeciesGender
makeFields ''GrowthRate
makeFields ''GrowthRateExperienceLevel
makeFields ''Nature
makeFields ''NatureStatChange
makeFields ''MoveBattleStylePreference
makeFields ''PokeathlonStat
makeFields ''NaturePokeathlonStatAffectSets
makeFields ''NaturePokeathlonStatAffect
makeFields ''Pokemon
makeFields ''PokemonAbility
makeFields ''PokemonType
makeFields ''PokemonHeldItem
makeFields ''PokemonHeldItemVersion
makeFields ''PokemonMove
makeFields ''PokemonMoveVersion
makeFields ''PokemonStat
makeFields ''PokemonSprites
makeFields ''LocationAreaEncounter
makeFields ''PokemonColor
makeFields ''PokemonForm
makeFields ''PokemonFormSprites
makeFields ''PokemonHabitat
makeFields ''PokemonShape
makeFields ''AwesomeName
makeFields ''PokemonSpecies
makeFields ''Genus
makeFields ''PokemonSpeciesDexEntry
makeFields ''PalParkEncounterArea
makeFields ''PokemonSpeciesVariety
makeFields ''Stat
makeFields ''MoveStatAffectSets
makeFields ''MoveStatAffect
makeFields ''NatureStatAffectSets
makeFields ''Type
makeFields ''TypePokemon
makeFields ''TypeRelations
makeFields ''Language
makeFields ''APIResource
makeFields ''Description
makeFields ''Effect
makeFields ''Encounter
makeFields ''FlavorText
makeFields ''GenerationGameIndex
makeFields ''MachineVersionDetail
makeFields ''Name
makeFields ''NamedAPIResource
makeFields ''VerboseEffect
makeFields ''VersionEncounterDetail
makeFields ''VersionGameIndex
makeFields ''VersionGroupFlavorText
