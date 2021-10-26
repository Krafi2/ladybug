use crate::topic::TopicConfig;
use anyhow::{Context, Result};
use figment::{
    providers::{Format, Serialized, Toml},
    Figment,
};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(deny_unknown_fields)]
pub struct Config {
    /// The directory where topics are located
    pub dotfile_dir: PathBuf,
    // This figment contains the default values to construct a [`TopicConfig`], however it may not
    // contain all of them so we can't rely on being able to deserialize it into a one. It is also
    // quite handy as we don't need to construct a figment when it comes to it.
    /// Default topic config
    #[serde(with = "serde_figment")]
    pub topic_config: Figment,
}

/// We implent serialize and deserialize for [`Figment`] by converting it to a struct that alreaddy
/// implements these functions, in this case [`Dict]. This approach is a bit hacky and inneficient,
/// however it is much more concise than the alternatives so I will keep it for now.
mod serde_figment {
    use figment::{providers::Serialized, value::Dict, Figment};
    use serde::{ser, Deserialize, Deserializer, Serialize, Serializer};

    pub fn serialize<S>(figment: &Figment, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match figment.extract::<Dict>() {
            Ok(dict) => dict.serialize(serializer),
            Err(err) => Err(ser::Error::custom(err)),
        }
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Figment, D::Error>
    where
        D: Deserializer<'de>,
    {
        match Dict::deserialize(deserializer) {
            Ok(dict) => Ok(Figment::from(Serialized::defaults(dict))),
            Err(err) => Err(err),
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            dotfile_dir: paths::dotfile_dir(),
            topic_config: Figment::from(Serialized::defaults(TopicConfig::default())),
        }
    }
}

impl Config {
    pub fn from_file(path: &Path) -> Result<Self> {
        Figment::from(Serialized::defaults(Self::default()))
            .merge(Toml::file(path))
            .extract()
            .with_context(|| {
                format!(
                    "Failed to load config from file: '{}'",
                    path.to_string_lossy()
                )
            })
    }

    pub fn new() -> Result<Self> {
        let path = paths::config_path();
        Self::from_file(&path)
    }
}

pub mod paths {
    use directories_next::{BaseDirs, ProjectDirs, UserDirs};
    use std::path::PathBuf;

    pub fn project_dirs() -> ProjectDirs {
        ProjectDirs::from("com", "Ladybug", "ladybug").expect("This system isn't supported.")
    }

    pub fn config_dir() -> PathBuf {
        project_dirs().config_dir().to_owned()
    }

    pub fn user_dirs() -> UserDirs {
        UserDirs::new().expect("This system isn't supported.")
    }

    pub fn base_dirs() -> BaseDirs {
        BaseDirs::new().expect("This system isn't supported")
    }

    pub fn user_config() -> PathBuf {
        base_dirs().config_dir().to_owned()
    }

    pub fn home_dir() -> PathBuf {
        user_dirs().home_dir().to_owned()
    }

    pub fn config_path() -> PathBuf {
        config_dir().join("ladybug.toml")
    }

    pub fn dotfile_dir() -> PathBuf {
        home_dir().join("ladybug")
    }
}
