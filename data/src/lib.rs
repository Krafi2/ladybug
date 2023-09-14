use std::path::Path;

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
    pub fn open(database: &Path) -> Result<Self, Error> {
        let conn = rusqlite::Connection::open(database)?;
        init_database(conn)
    }

    pub fn open_in_memory() -> Result<Self, Error> {
        let conn = rusqlite::Connection::open_in_memory()?;
        init_database(conn)
    }

    pub fn new_topic(&mut self, topic: String) -> Result<Topic, Error> {
        let mut stmt = self.conn.prepare_cached(
            "
            INSERT OR IGNORE INTO topics (name) VALUES (?1);
            SELECT topic_id FROM topics WHERE name = ?1;
            ",
        )?;
        stmt.query_row([&topic], |row| Ok(TopicId(row.get(0)?)))
            .map(|id| Topic { name: topic, id })
            .map_err(Into::into)
    }

    pub fn installed(&mut self, package: &Package) -> Result<(), Error> {
        self.conn.execute(
            "INSERT INTO packages (name, metadata, provider_id, topic_id) VALUES (?1, ?2, ?3, ?4)",
            params![
                &package.name,
                &package.metadata,
                package.provider.0,
                package.topic.map(|t| t.0)
            ],
        )?;
        Ok(())
    }

    pub fn removed(&mut self, package: &Package) -> Result<(), Error> {
        // Package names should be unique per provider
        let rows = self.conn.execute(
            "DELETE FROM packages WHERE name = ?1 AND provider_id = ?2",
            params![&package.name, package.provider.0],
        )?;
        assert_eq!(rows, 1);
        Ok(())
    }

    pub fn get_topic(&mut self, topic: TopicId) -> Result<Vec<Package>, Error> {
        self.conn
            .prepare_cached("SELECT * FROM packages WHERE topic_id = ?1")?
            .query_map([topic.0], |row| {
                Ok(Package {
                    name: row.get(0)?,
                    metadata: row.get(1)?,
                    topic: row.get::<_, Option<_>>(3)?.map(TopicId),
                    provider: ProviderId(row.get(3)?),
                })
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
            name     STRING NOT NULL UNIQUE,
        );
        CREATE TABLE IF NOT EXISTS packages (
            package_id  INTEGER NOT NULL PRIMARY KEY,
            name        STRING NOT NULL,
            metadata    BLOB NOT NULL,
            provider_id INTEGER NOT NULL,
            topic_id    INTEGER,
            FOREIGN KEY (topic_id) REFERENCES topics(topic_id) -- Note that this doesn't hold for NULL
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PackageId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TopicId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ProviderId(u32);

impl ProviderId {
    /// Create a new ProviderId. Please make sure that the numerical ids remain
    /// consistent between versions and runs!
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    /// Get the inner value of the id
    pub fn inner(&self) -> u32 {
        self.0
    }
}
