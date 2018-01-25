# purescript-datetime-iso

[![Pursuit version](https://pursuit.purescript.org/packages/purescript-datetime-iso/badge)](https://pursuit.purescript.org/packages/purescript-datetime-iso)
![Current tag](https://img.shields.io/github/tag/jmackie4/purescript-datetime-iso.svg)
[![Build status](https://travis-ci.org/jmackie4/purescript-datetime-iso.svg?branch=master)](https://travis-ci.org/jmackie4/purescript-datetime-iso)

## Description

Provides a minimal [`DateTime`](https://github.com/purescript/purescript-datetime/)  wrapper that encodes/decodes to/from the *simplified* extended ISO format ([`ISO 8601`](https://en.wikipedia.org/wiki/ISO_8601)); specifically `YYYY-MM-DDTHH:mm:ss[.sss]Z` where hyphens and colons can be omitted.

If you're handling dates that *aren't* in this format you should check out the [purescript-formatters](https://github.com/slamdata/purescript-formatters) package instead.

## Installation

```
bower install purescript-datetime-iso
```

## Usage

Here's a motivating example:

```purescript
module Data.Event where

import Prelude

import Data.DateTime.ISO (ISO, unwrapISO)
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
        timestamp <- obj .? "timestamp" <#> unwrapISO
        description <- obj .? "description"
        pure $ Event { timestamp, description }
```

## Documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-datetime-iso).
