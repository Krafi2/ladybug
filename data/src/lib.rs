use std::{fmt::Display, hash::Hash, path::Path};

use rusqlite::{params, Result};

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Rusqlite(#[from] rusqlite::Error),
}

pub struct Connection {
    conn: rusqlite::Connection,
}

impl Connection {
    /// Open a new connection to the database at `path`
    pub fn open(path: &Path) -> Result<Self, Error> {
        let conn = rusqlite::Connection::open(path)?;
        init_database(conn)
    }

    /// Open a new database connection in memory
    pub fn open_in_memory() -> Result<Self, Error> {
        let conn = rusqlite::Connection::open_in_memory()?;
        init_database(conn)
    }

    /// Register a new topic
    pub fn new_topic(&mut self, topic: String) -> Result<Topic, Error> {
        self.conn
            .prepare_cached("INSERT OR IGNORE INTO topics (name) VALUES (?1)")?
            .execute([&topic])?;
        self.conn
            .prepare_cached("SELECT topic_id FROM topics WHERE name = ?1")?
            .query_row([&topic], |row| Ok(TopicId(row.get(0)?)))
            .map(|id| Topic { name: topic, id })
            .map_err(Into::into)
    }

    /// Mark a package as removed
    pub fn installed(&mut self, package: &Package) -> Result<PackageId, Error> {
        self.conn.execute(
            "INSERT OR IGNORE INTO packages (name, metadata, provider_id, topic_id) VALUES (?1, ?2, ?3, ?4)",
            params![
                &package.name,
                &package.metadata,
                package.provider.0,
                package.topic.map(|t| t.0)
            ],
        )?;
        Ok(PackageId(self.conn.last_insert_rowid()))
    }

    /// Remove a package
    pub fn removed(&mut self, package: &Package) -> Result<(), Error> {
        // Package names should be unique per provider
        let rows = self.conn.execute(
            "DELETE FROM packages WHERE name = ?1 AND provider_id = ?2",
            params![&package.name, package.provider.0],
        )?;
        assert!(rows <= 1);
        Ok(())
    }

    /// Get packages in this topic
    pub fn get_topic(&mut self, topic: TopicId) -> Result<Vec<(PackageId, Package)>, Error> {
        self.conn
            .prepare_cached("SELECT * FROM packages WHERE topic_id = ?1")?
            .query_map([topic.0], |row| {
                Ok((
                    PackageId(row.get(0)?),
                    Package {
                        name: row.get(1)?,
                        metadata: row.get(2)?,
                        topic: row.get::<_, Option<_>>(3)?.map(TopicId),
                        provider: ProviderId(row.get(4)?),
                    },
                ))
            })
            .and_then(|rows| Result::from_iter(rows))
            .map_err(Into::into)
    }

    /// Get all packages
    pub fn get_all(&mut self) -> Result<Vec<(PackageId, Package)>, Error> {
        self.conn
            .prepare_cached("SELECT * FROM packages")?
            .query_map([], |row| {
                Ok((
                    PackageId(row.get(0)?),
                    Package {
                        name: row.get(1)?,
                        metadata: row.get(2)?,
                        topic: row.get::<_, Option<_>>(3)?.map(TopicId),
                        provider: ProviderId(row.get(4)?),
                    },
                ))
            })
            .and_then(|rows| Result::from_iter(rows))
            .map_err(Into::into)
    }
}

fn init_database(conn: rusqlite::Connection) -> std::result::Result<Connection, Error> {
    conn.set_db_config(
        rusqlite::config::DbConfig::SQLITE_DBCONFIG_ENABLE_FKEY,
        true,
    )?;
    conn.execute_batch(
        "
        CREATE TABLE IF NOT EXISTS topics (
            topic_id INTEGER NOT NULL PRIMARY KEY,
            name     STRING NOT NULL UNIQUE
        );
        CREATE TABLE IF NOT EXISTS packages (
            package_id  INTEGER NOT NULL PRIMARY KEY,
            name        STRING NOT NULL,
            metadata    BLOB NOT NULL,
            provider_id INTEGER NOT NULL,
            topic_id    INTEGER,
            FOREIGN KEY (topic_id) REFERENCES topics(topic_id), -- Note that this doesn't hold for NULL
            UNIQUE(name, provider_id)
        );
        ",
    )?;
    Ok(Connection { conn })
}

#[derive(Debug)]
pub struct Topic {
    name: String,
    id: TopicId,
}

impl Topic {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn id(&self) -> TopicId {
        self.id
    }
}

impl Display for Topic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

// Delegate hash to the unique id
impl Hash for Topic {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl PartialEq for Topic {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl Eq for Topic {}

#[derive(Debug)]
pub struct Package {
    name: String,
    metadata: Vec<u8>,
    topic: Option<TopicId>,
    provider: ProviderId,
}

impl Package {
    pub fn new<I: Into<ProviderId>>(
        name: String,
        metadata: Vec<u8>,
        topic: Option<TopicId>,
        provider: I,
    ) -> Self {
        Self {
            name,
            metadata,
            topic,
            provider: provider.into(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn metadata(&self) -> &[u8] {
        &self.metadata
    }

    pub fn provider(&self) -> ProviderId {
        self.provider
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PackageId(i64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TopicId(i64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProviderId(i64);

impl ProviderId {
    /// Create a new ProviderId. Please make sure that the numerical ids remain
    /// consistent between versions and runs!
    pub fn new(id: i64) -> Self {
        Self(id)
    }

    /// Get the inner value of the id
    pub fn inner(&self) -> i64 {
        self.0
    }
}
