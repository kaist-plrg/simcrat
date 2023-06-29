use std::time::Duration;

use etrace::some_or;
use mongodb::{
    bson::doc,
    options::{ClientOptions, Credential, ServerAddress},
    Client as MongoClient, Collection,
};
use serde::{de::DeserializeOwned, Deserialize, Deserializer, Serialize};

pub trait HasElapsed {
    fn elapsed(&self) -> f32;
}

#[derive(Debug, Clone, Serialize)]
pub struct CacheData<K, V>
where
    K: Sync + Send + Unpin + Serialize + DeserializeOwned,
    V: Sync + Send + Unpin + Serialize + DeserializeOwned + HasElapsed,
{
    _id: K,
    val: V,
}

impl<'d, K, V> Deserialize<'d> for CacheData<K, V>
where
    K: Sync + Send + Unpin + Serialize + DeserializeOwned,
    V: Sync + Send + Unpin + Serialize + DeserializeOwned + HasElapsed,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: Deserializer<'d> {
        let mut data = serde_json::Value::deserialize(deserializer)?;
        let _id = data["_id"].take();
        let val = data["val"].take();
        let _id = serde_json::from_value(_id).map_err(serde::de::Error::custom)?;
        let val = serde_json::from_value(val).map_err(serde::de::Error::custom)?;
        Ok(Self { _id, val })
    }
}

pub struct Cache<K, V>
where
    K: Sync + Send + Unpin + Serialize + DeserializeOwned,
    V: Sync + Send + Unpin + Serialize + DeserializeOwned + HasElapsed,
{
    collection: Option<Collection<CacheData<K, V>>>,
    real_time: bool,
}

pub struct DbConfig {
    pub name: Option<String>,
    pub host: Option<String>,
    pub port: Option<String>,
    pub password: Option<String>,
    pub real_time: bool,
}

impl<K, V> Cache<K, V>
where
    K: Sync + Send + Unpin + Serialize + DeserializeOwned,
    V: Sync + Send + Unpin + Serialize + DeserializeOwned + HasElapsed,
{
    pub fn new(conf: DbConfig) -> Self {
        let collection = if let Some(name) = conf.name {
            let host = conf.host.unwrap_or("localhost".to_string());
            let port = conf.port.unwrap_or("27017".to_string()).parse().ok();
            let addr = ServerAddress::Tcp { host, port };
            let credential = conf.password.map(|password| {
                Credential::builder()
                    .username("admin".to_string())
                    .password(password)
                    .build()
            });
            let client_options = ClientOptions::builder()
                .hosts(vec![addr])
                .credential(credential)
                .build();
            let client = MongoClient::with_options(client_options).unwrap();
            let db = client.database(&name);
            Some(db.collection::<CacheData<K, V>>("cache"))
        } else {
            None
        };
        Self {
            collection,
            real_time: conf.real_time,
        }
    }

    pub async fn get(&self, key: &K) -> Option<V> {
        let collection = self.collection.as_ref()?;
        let data = collection
            .find_one(doc! { "_id": mongodb::bson::to_bson(key).unwrap() }, None)
            .await
            .unwrap()?;
        if self.real_time {
            tokio::time::sleep(Duration::from_secs_f32(data.val.elapsed())).await;
        }
        Some(data.val)
    }

    pub async fn insert(&self, key: K, value: V) {
        let collection = some_or!(self.collection.as_ref(), return);
        let data = CacheData {
            _id: key,
            val: value,
        };
        let _ = collection.insert_one(data, None).await;
    }
}
