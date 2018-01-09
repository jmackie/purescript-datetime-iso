# purescript-datetime-iso

Convenience `newtype` for encoding and decoding [`DateTime`](https://github.com/purescript/purescript-datetime/) values.

## Installation

```
bower install purescript-datetime-iso
```

## Usage

Here's a motivating example:

```purescript
module Data.Event where

import Prelude

import Data.DateTime.ISO (ISO)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.?))
import Data.DateTime (DateTime)
import Data.Newtype (unwrap)

newtype Event = Event
    { timestamp   :: DateTime
    , description :: String
    }

instance decodeJsonEvent :: DecodeJson Event where
    decodeJson json = do
        obj <- decodeJson json
        (iso :: ISO) <- obj .? "timestamp"  -- needs unwrapping
        description <- obj .? "description"
        pure $ Event { timestamp: unwrap iso, description }
```

## Documentation

This library provides a minimal `DateTime` wrapper that encodes/decodes to/from the *simplified* extended ISO format ([`ISO 8601`](https://en.wikipedia.org/wiki/ISO_8601)); specifically `YYYY-MM-DDTHH:mm:ss[.sss]Z` where hyphens and colons can be omitted.

If you're handling dates that *aren't* in this format you should check out the [purescript-formatters](https://github.com/slamdata/purescript-formatters) package instead.

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-datetime-iso).
