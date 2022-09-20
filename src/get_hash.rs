use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
};

pub(crate) trait GetHash: Hash {
    /// Returns the hash of `self` in a single call, without having to create a `Hasher`.
    fn get_hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

impl<T: Hash> GetHash for T {}
