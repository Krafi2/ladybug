use anyhow::{anyhow, Context, Error, Result};
use ignore::{
    overrides::{Override, OverrideBuilder},
    Walk, WalkBuilder,
};
use std::path::Path;

pub fn extend_glob<'a, I>(mut builder: &mut OverrideBuilder, globs: I) -> Result<Override>
where
    I: IntoIterator<Item = &'a str>,
{
    for s in globs {
        builder
            .add(s)
            .with_context(|| anyhow!("Failed to construct glob"))?;
    }
    builder.build().map_err(Error::new)
}

pub fn from_strings<'a, I>(root: &Path, globs: I) -> Result<Override>
where
    I: IntoIterator<Item = &'a str>,
{
    let mut builder = OverrideBuilder::new(root);
    extend_glob(&mut builder, globs)
}

pub fn new_walker(globs: Override) -> WalkBuilder {
    let mut builder = WalkBuilder::new(globs.path());
    builder.overrides(globs).standard_filters(false);
    builder
}

// pub mod de {
//     use ignore::overrides::{Override, OverrideBuilder};
//     use serde::{de::Error, Deserialize, Deserializer};
//
//     pub fn pattern<'de, D>(deserializer: D) -> Result<Override, D::Error>
//     where
//         D: Deserializer<'de>,
//     {
//         let string = <String as Deserialize>::deserialize(deserializer)?;
//         Pattern::new(&string).map_err(D::Error::custom)
//     }
//
//     pub fn patterns<'de, D>(deserializer: D) -> Result<Override, D::Error>
//     where
//         D: Deserializer<'de>,
//     {
//         let vec = <Vec<String> as Deserialize>::deserialize(deserializer)?;
//         vec.iter()
//             .map(|s| Pattern::new(&s))
//             .collect::<Result<Vec<_>, _>>()
//             .map_err(D::Error::custom)
//     }
// }
