use std::{collections::HashMap, num::NonZeroU32};

use fxhash::FxBuildHasher;
use parking_lot::RwLock;

use crate::hash::Fingerprint;

/// A global upper bound on the shard size.
/// If there are too many shards, the memory overhead is unacceptable.
const MAX_SHARD_SIZE: u32 = 512;

/// Returns a read-only default shard size.
fn default_shard_size() -> NonZeroU32 {
    static ITEM_SHARD_SIZE: std::sync::OnceLock<NonZeroU32> = std::sync::OnceLock::new();

    /// By testing, we found that the optimal shard size is 2 * number of
    /// threads.
    fn determine_default_shard_size() -> NonZeroU32 {
        // This detection is from rayon.
        let thread_cnt = {
            std::thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(1)
        };

        // A valid shard size is a power of two.
        let size = (thread_cnt.next_power_of_two() * 2) as u32;
        // Perform early non-zero check to avoid panics.
        NonZeroU32::new(size.min(MAX_SHARD_SIZE)).unwrap()
    }

    *ITEM_SHARD_SIZE.get_or_init(determine_default_shard_size)
}

type FMapBase<V> = RwLock<HashMap<Fingerprint, V, FxBuildHasher>>;

/// A map that shards items by their fingerprint. Uses fxhash to reduce hashing
/// overhead; fingerprints are already well-distributed.
pub struct FingerprintMap<V> {
    mask: u32,
    shards: Vec<FMapBase<V>>,
}

impl<V> Default for FingerprintMap<V> {
    fn default() -> Self {
        Self::new(default_shard_size())
    }
}

impl<V> FingerprintMap<V> {
    /// Creates a new `FingerprintMap` with the given shard size.
    pub fn new(shard_size: NonZeroU32) -> Self {
        let shard_size = shard_size.get().next_power_of_two().min(MAX_SHARD_SIZE);

        assert!(
            shard_size.is_power_of_two(),
            "shard size must be a power of two"
        );
        assert!(shard_size > 0, "shard size must be greater than zero");
        Self {
            mask: shard_size - 1,
            shards: (0..shard_size)
                .map(|_| RwLock::new(HashMap::with_hasher(FxBuildHasher::default())))
                .collect(),
        }
    }

    /// Iterates over all items in the map.
    pub fn into_items(self) -> impl Iterator<Item = (Fingerprint, V)> {
        self.shards
            .into_iter()
            .flat_map(|shard| shard.into_inner().into_iter())
    }

    /// Gets the shard
    pub fn shard(&self, fg: Fingerprint) -> &FMapBase<V> {
        let shards = &self.shards;
        let route_idx = (fg.lower32() & self.mask) as usize;

        debug_assert!(route_idx < shards.len());
        unsafe { shards.get_unchecked(route_idx) }
    }

    /// Gets the mutable shard slice useful for parallel iteration.
    pub fn as_mut_slice(&mut self) -> &mut [FMapBase<V>] {
        &mut self.shards
    }

    /// Checks if the map contains the given key.
    pub fn contains_key(&self, fg: &Fingerprint) -> bool {
        self.shard(*fg).read().contains_key(fg)
    }

    /// Sum of lengths across shards (approximate size).
    pub fn shards_len(&self) -> usize {
        self.shards.iter().map(|s| s.read().len()).sum()
    }
}
