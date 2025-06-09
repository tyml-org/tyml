use std::{
    collections::HashMap,
    fs::{self, File, OpenOptions},
    io::{self},
    path::{Path, PathBuf},
    sync::{Arc, Condvar, LazyLock, Mutex, OnceLock},
};

use fs2::FileExt;
use reqwest::blocking;
use sha2::{Digest, Sha256};

/// Path equivalent to `~/.cache/tyml`
static CACHE_DIR: LazyLock<PathBuf> = LazyLock::new(|| {
    dirs::cache_dir() // Appropriate OS-specific cache directory citeturn0search5
        .unwrap_or_else(|| std::env::temp_dir())
        .join("tyml")
});

/// Stores the result of the one-time “clean cache on startup” operation
static INIT_RESULT: OnceLock<io::Result<()>> = OnceLock::new();

/// Shared map URL → wait handle `(Mutex<bool>, Condvar)`
static INFLIGHT: LazyLock<Mutex<HashMap<String, Arc<(Mutex<bool>, Condvar)>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// fetch the file for the given `url` and return the local path (synchronous)
pub fn get_cached_file(url: &str) -> io::Result<PathBuf> {
    ensure_cache_ready()?; // Clean only once at startup

    let cache_path = CACHE_DIR.join(hash_url(url));
    if cache_path.exists() {
        return Ok(cache_path); // Immediate hit
    }

    // ---------- Prevent duplicate downloads inside the same process ----------
    let (wait_arc, downloader) = {
        let mut map = INFLIGHT.lock().unwrap();
        match map.entry(url.to_string()) {
            std::collections::hash_map::Entry::Occupied(e) => (e.get().clone(), false),
            std::collections::hash_map::Entry::Vacant(e) => {
                let w = Arc::new((Mutex::new(false), Condvar::new()));
                e.insert(w.clone());
                (w, true) // This thread becomes the downloader
            }
        }
    };

    // If not the downloader, wait until the download is finished
    if !downloader {
        let (lock, cvar) = &*wait_arc;
        let mut done = lock.lock().unwrap();
        while !*done {
            done = cvar.wait(done).unwrap();
        }
        return Ok(cache_path);
    }

    // ---------- Inter-process exclusive lock ----------
    let lock_file = acquire_file_lock(&cache_path.with_extension("lock"))?;

    // Double-check if another process finished first
    if cache_path.exists() {
        finish_waiters(url, &wait_arc);
        drop(lock_file);
        return Ok(cache_path);
    }

    // ---------- Execute the download ----------
    let bytes = blocking::get(url)
        .map_err(|e| io::Error::other(e))?
        .bytes()
        .map_err(|e| io::Error::other(e))?;

    let tmp = cache_path.with_extension("tmp");
    fs::write(&tmp, &bytes)?;
    fs::rename(&tmp, &cache_path)?; // On the same filesystem `rename` is atomic citeturn0search9

    finish_waiters(url, &wait_arc); // Notify waiters
    drop(lock_file); // `lock_exclusive` is released automatically
    Ok(cache_path)
}

/// Delete the entire cache at startup (exactly once across processes)
fn ensure_cache_ready() -> io::Result<()> {
    INIT_RESULT
        .get_or_init(|| {
            // Shared lock file
            let lock = acquire_file_lock(&CACHE_DIR.join(".init.lock"))?;
            if CACHE_DIR.exists() {
                fs::remove_dir_all(&*CACHE_DIR)?; // Recursive deletion citeturn0search7
            }
            fs::create_dir_all(&*CACHE_DIR)?;
            drop(lock);
            Ok(())
        })
        .as_ref()
        .map_err(|error| io::Error::new(error.kind(), error.to_string()))
        .cloned()
}

/// Release all waiters for the given URL
fn finish_waiters(url: &str, wait_arc: &Arc<(Mutex<bool>, Condvar)>) {
    {
        let (lock, cvar) = &**wait_arc;
        let mut done = lock.lock().unwrap();
        *done = true;
        cvar.notify_all();
    }
    INFLIGHT.lock().unwrap().remove(url);
}

/// Simple helper to acquire an exclusive file lock
fn acquire_file_lock(lock_path: &Path) -> io::Result<File> {
    if let Some(parent) = lock_path.parent() {
        fs::create_dir_all(parent)?; // Create parent directories first
    }
    let f = OpenOptions::new()
        .create(true)
        .write(true)
        .open(lock_path)?;
    f.lock_exclusive()?; // Obtain exclusive file lock via flock / LockFileEx
    Ok(f)
}

/// SHA-256 for a collision-free file name
fn hash_url(u: &str) -> String {
    let mut h = Sha256::new();
    h.update(u.as_bytes());
    format!("{:x}", h.finalize()) // Hex string citeturn0search6
}

