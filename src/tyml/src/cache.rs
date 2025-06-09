use std::{
    collections::HashMap,
    io,
    path::{Path, PathBuf},
    sync::{Arc, LazyLock},
};

use fs2::FileExt; // lock_exclusive / unlock :contentReference[oaicite:5]{index=5}
use sha2::{Digest, Sha256};
use tokio::{
    fs,
    sync::{Mutex, Notify, OnceCell},
};

/// Equivalent to ~/.cache/tyml
static CACHE_DIR: LazyLock<PathBuf> = LazyLock::new(|| {
    dirs::cache_dir() // OS-specific cache directory :contentReference[oaicite:6]{index=6}
        .unwrap_or_else(|| std::env::temp_dir())
        .join("tyml")
});

/// Initialize the cache only once at startup
static INIT_DONE: OnceCell<()> = OnceCell::const_new();

/// Map URL → Notify used to prevent duplicate downloads within the same process
static INFLIGHT: LazyLock<Mutex<HashMap<String, Arc<Notify>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// Public API — asynchronous cache retrieval
pub async fn get_cached_file(url: &str) -> io::Result<PathBuf> {
    ensure_cache_ready().await?; // Clear cache on first call after startup

    let cache_path = CACHE_DIR.join(format!("{}.tyml", hash_url(url)));
    if cache_path.exists() {
        return Ok(cache_path); // Cache hit
    }

    // ------- Prevent duplicate downloads inside this process -------
    let notify = {
        let mut map = INFLIGHT.lock().await;
        map.entry(url.to_string())
            .or_insert_with(|| Arc::new(Notify::new()))
            .clone()
    };

    // ------- Inter-process exclusive lock (run blocking I/O in a dedicated thread) -------
    let lock_path = cache_path.with_extension("tyml.lock");
    let _file_lock = tokio::task::spawn_blocking(move || acquire_file_lock(&lock_path))
        .await
        .expect("join failure")?;

    // Double-check: another process may have completed already
    if cache_path.exists() {
        return Ok(cache_path);
    }

    // ------- This task becomes the downloader -------
    let bytes = reqwest::Client::new()
        .get(url)
        .send()
        .await
        .map_err(|e| io::Error::other(e))?
        .bytes()
        .await
        .map_err(|e| io::Error::other(e))?;

    let tmp = cache_path.with_extension("tyml.tmp");
    fs::write(&tmp, &bytes).await?; // Asynchronous file write :contentReference[oaicite:7]{index=7}
    fs::rename(&tmp, &cache_path).await?; // Atomic if on the same filesystem :contentReference[oaicite:8]{index=8}

    notify.notify_waiters(); // Wake up waiting tasks
    INFLIGHT.lock().await.remove(url);
    Ok(cache_path)
}

//
// ---------- Internal helpers ----------
//
async fn ensure_cache_ready() -> io::Result<()> {
    INIT_DONE.get_or_try_init(init_once).await.map(|_| ())
}

async fn init_once() -> io::Result<()> {
    // Acquire .init.lock (spawn_blocking because it's blocking I/O)
    let init_lock = {
        let path = CACHE_DIR.join(".init.lock");
        tokio::task::spawn_blocking(move || acquire_file_lock(&path))
            .await
            .expect("join failure")?
    };

    // Delete and recreate the cache folder
    if CACHE_DIR.exists() {
        fs::remove_dir_all(&*CACHE_DIR).await?; // Asynchronous recursive delete :contentReference[oaicite:9]{index=9}
    }
    fs::create_dir_all(&*CACHE_DIR).await?; // Precreate to avoid NotFound errors later :contentReference[oaicite:10]{index=10}

    drop(init_lock); // Release the lock
    Ok(())
}

/// Ensure parent directories exist, then acquire an exclusive lock
fn acquire_file_lock(lock_path: &Path) -> io::Result<std::fs::File> {
    if let Some(parent) = lock_path.parent() {
        std::fs::create_dir_all(parent)?; // open fails if the parent folder is missing :contentReference[oaicite:11]{index=11}
    }
    let f = std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .open(lock_path)?;
    f.lock_exclusive()?; // Blocking exclusive lock :contentReference[oaicite:12]{index=12}
    Ok(f)
}

/// URL → collision-free hashed filename
fn hash_url(u: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(u.as_bytes());
    format!("{:x}", hasher.finalize()) // Hex string :contentReference[oaicite:13]{index=13}
}
