# conflate

Conflate is a library that helps parse and document congifurations that are defined via environment variables. 

The library uses the higher kinded data (HKD) and type level strings to define the shape of a configuration. 

The same shape can be reused to:
 * read the confirguation from the system environment
 * define defaults (where appropriate)
 * generate documentation 

## Examples

```haskell
data NestedConfigF f = NestedConfig
  { nestedField :: HKD f 'Optional "NESTED_FIELD" String
  } deriving (Generic, HasParser)

deriving instance Show (NestedConfigF ConfigIdentity)

data ConfigF f = Config
  { connectionString :: HKD f 'Required "CONNECTION_STRING" String
  , connectionPoolSize :: HKD f 'Optional "CONNECTION_POOL_SIZE" Int
  , nestedConfig :: NestedConfigF f
  } deriving (Generic, HasParser)

deriving instance Show (ConfigF ConfigIdentity)

type Config = ConfigF ConfigIdentity

configDefaults :: ConfigF Defaults
configDefaults = Config
  { connectionString = NoDefault
  , connectionPoolSize = Default 10
  , nestedConfig = NestedConfig
    { nestedField = Default "hello!"
    }
  }

parseConfig :: IO Config
parseConfig = parseDefault configDefaults
```
