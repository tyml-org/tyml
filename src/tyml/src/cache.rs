use bytes::Bytes;
use fs2::FileExt;
use sha2::{Digest, Sha256};
use std::{
    collections::HashMap,
    io,
    path::{Path, PathBuf},
    sync::{Arc, LazyLock},
};
use tokio::{
    fs,
    sync::{Mutex, Notify, OnceCell},
};

/// Path equivalent to `~/.cache/tyml`
static CACHE_DIR: LazyLock<PathBuf> = LazyLock::new(|| {
    dirs::cache_dir()
        .unwrap_or_else(|| std::env::temp_dir())
        .join("tyml")
});

/// Ensures initialization is run only once per process
static INIT_DONE: OnceCell<()> = OnceCell::const_new();

/// Map URL → `Notify` (suppresses duplicate downloads within the same process)
static INFLIGHT: LazyLock<Mutex<HashMap<String, Arc<Notify>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// ---------------------------------------------------------------------------
/// Public API
pub async fn get_cached_file(url: &str) -> io::Result<PathBuf> {
    ensure_cache_ready().await?;
    let cache_path = CACHE_DIR.join(format!("{}.tyml", hash_url(url)));

    // ---------- Prevent duplicate downloads inside this process ----------
    let notify = {
        let mut map = INFLIGHT.lock().await;
        map.entry(url.to_string())
            .or_insert_with(|| Arc::new(Notify::new()))
            .clone()
    };

    // ---------- Inter-process exclusive lock ----------
    let lock_path = cache_path.with_extension("tyml.lock");
    let _file_lock = tokio::task::spawn_blocking(move || acquire_file_lock(&lock_path))
        .await
        .expect("join failure")?;

    // ---------- Always attempt to fetch from the network ----------
    match try_download(url).await {
        Ok(bytes) => {
            // On success, update the cache
            write_atomically(&cache_path, &bytes).await?;
        }
        Err(net_err) => {
            // On failure: fall back to cache if it exists
            if cache_path.exists() {
                // Notify waiters and return
                finish_waiters(url, &notify).await;
                return Ok(cache_path);
            } else {
                // No cache either → return the original error
                finish_waiters(url, &notify).await;
                return Err(net_err);
            }
        }
    }

    finish_waiters(url, &notify).await;
    Ok(cache_path)
}

/// ---------------------------------------------------------------------------
/// Network fetch (HTTP 4xx/5xx are treated as `Err`)
async fn try_download(url: &str) -> io::Result<Bytes> {
    let resp = reqwest::Client::new()
        .get(url)
        .send()
        .await
        .map_err(io::Error::other)?;

    if !resp.status().is_success() {
        return Err(io::Error::other(format!("HTTP error: {}", resp.status())));
    }

    resp.bytes().await.map_err(io::Error::other)
}

/// ---------------------------------------------------------------------------
/// Create the cache directory once
async fn ensure_cache_ready() -> io::Result<()> {
    INIT_DONE
        .get_or_try_init(|| async {
            if !CACHE_DIR.exists() {
                fs::create_dir_all(&*CACHE_DIR).await?;
            }
            Ok(())
        })
        .await
        .map(|_| ())
}

/// ---------------------------------------------------------------------------
/// Atomic update via temporary file + `rename()`
async fn write_atomically(path: &Path, bytes: &Bytes) -> io::Result<()> {
    let tmp = path.with_extension("tyml.tmp");
    fs::write(&tmp, bytes).await?;
    fs::rename(&tmp, path).await?;
    Ok(())
}

/// ---------------------------------------------------------------------------
/// Wake waiting tasks and remove entry from `INFLIGHT`
async fn finish_waiters(url: &str, notify: &Notify) {
    notify.notify_waiters();
    INFLIGHT.lock().await.remove(url);
}

/// ---------------------------------------------------------------------------
/// Acquire a file lock
fn acquire_file_lock(lock_path: &Path) -> io::Result<std::fs::File> {
    if let Some(p) = lock_path.parent() {
        std::fs::create_dir_all(p)?;
    }
    let f = std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .open(lock_path)?;
    f.lock_exclusive()?;
    Ok(f)
}

/// ---------------------------------------------------------------------------
/// URL → SHA-256 → hex string
fn hash_url(u: &str) -> String {
    let mut h = Sha256::new();
    h.update(u.as_bytes());
    format!("{:x}", h.finalize())
}
