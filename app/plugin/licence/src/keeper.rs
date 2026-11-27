// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! The plugin's side of a licence: where one is kept, whether the one kept stands, and what a person is told when
//! the one they enter does not.
//!
//! A licence is kept as a file of the user's own, the licence's text and nothing else, so every instance of the
//! plugin and every format of it (VST3, Audio Unit) finds the one licence, entered once. It is judged each time it is
//! read: against the keys this plugin carries, the Sonic Pi version it is, and the marks of the licences revoked
//! when it was built. Nothing here uses the network.

use crate::{block, read, token_in, Licence, PublicKey, ReadError, Refusal, PRODUCT};
use std::path::{Path, PathBuf};

/// Why a plugin is not licensed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Why {
    /// No licence has been entered.
    None,
    /// What was entered (or is kept) has no licence in it.
    NotALicence,
    /// There is a licence in it, with part of it missing or changed.
    Damaged,
    /// It is a kind of licence newer than this plugin.
    Unsupported,
    /// It was not signed by a key this plugin carries.
    NotGenuine,
    /// It is a licence for something else.
    WrongProduct,
    /// It is for other versions of Sonic Pi, which are given.
    Version { from: (u16, u16), until: (u16, u16) },
    /// It has been revoked.
    Revoked,
}

impl Why {
    /// The word the page knows it by.
    pub fn word(&self) -> &'static str {
        match self {
            Why::None => "none",
            Why::NotALicence => "not-a-licence",
            Why::Damaged => "damaged",
            Why::Unsupported => "unsupported",
            Why::NotGenuine => "not-genuine",
            Why::WrongProduct => "wrong-product",
            Why::Version { .. } => "version",
            Why::Revoked => "revoked",
        }
    }
}

/// How a plugin stands: licensed, to whom, or not, and why.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Standing {
    Licensed { name: String, id: String },
    Unlicensed(Why),
}

impl Standing {
    pub fn licensed(&self) -> bool {
        matches!(self, Standing::Licensed { .. })
    }

    /// As the page is told it: { licensed, name, id } or { licensed: false, why } (and the versions, for "version").
    pub fn to_json(&self) -> String {
        let dotted = |v: &(u16, u16)| format!("{}.{}", v.0, v.1);
        match self {
            Standing::Licensed { name, id } => serde_json::json!({ "licensed": true, "name": name, "id": id }),
            Standing::Unlicensed(Why::Version { from, until }) => serde_json::json!({ "licensed": false, "why": "version", "from": dotted(from), "until": dotted(until) }),
            Standing::Unlicensed(why) => serde_json::json!({ "licensed": false, "why": why.word() }),
        }
        .to_string()
    }
}

/// Keeps a plugin's licence and says how the plugin stands.
pub struct Keeper {
    file: Option<PathBuf>,
    keys: Vec<PublicKey>,
    version: (u16, u16),
    revoked: Vec<String>,
}

impl Keeper {
    /// `file`: where the licence is kept (none: nowhere, and a licence entered stands until the plugin is closed).
    /// `version`: the Sonic Pi version this plugin is. `revoked`: the marks of revoked licences, a line each, as a
    /// release carries them (blank lines and lines beginning with # are not marks).
    pub fn new(file: Option<PathBuf>, keys: Vec<PublicKey>, version: (u16, u16), revoked: &str) -> Keeper {
        let revoked = revoked.lines().map(str::trim).filter(|l| !l.is_empty() && !l.starts_with('#')).map(str::to_string).collect();
        Keeper { file, keys, version, revoked }
    }

    /// The licence in `text`, if it is one this plugin takes.
    fn judge(&self, text: &str) -> Result<Licence, Why> {
        let licence = read(text, &self.keys).map_err(|why| match why {
            ReadError::NotALicence => Why::NotALicence,
            ReadError::Damaged => Why::Damaged,
            ReadError::Unsupported => Why::Unsupported,
            ReadError::UnknownKey(_) | ReadError::BadSignature => Why::NotGenuine,
        })?;
        let marks: Vec<&str> = self.revoked.iter().map(String::as_str).collect();
        licence.covers(PRODUCT, self.version, &marks).map_err(|why| match why {
            Refusal::WrongProduct => Why::WrongProduct,
            Refusal::Version { from, until } => Why::Version { from, until },
            Refusal::Revoked => Why::Revoked,
        })?;
        Ok(licence)
    }

    /// Whose licence `text` holds, if it holds one this plugin takes. Nothing is kept: this is for offering a licence
    /// found somewhere (copied to the clipboard) before a person says to use it.
    pub fn whose(&self, text: &str) -> Option<String> {
        self.judge(text).ok().map(|l| l.name)
    }

    /// How the plugin stands by the licence that is kept.
    pub fn standing(&self) -> Standing {
        let Some(text) = self.file.as_ref().and_then(|f| std::fs::read_to_string(f).ok()) else { return Standing::Unlicensed(Why::None) };
        match self.judge(&text) {
            Ok(l) => Standing::Licensed { name: l.name, id: l.id },
            Err(why) => Standing::Unlicensed(why),
        }
    }

    /// A licence a person entered, as pasted: a whole email will do. If it is one this plugin takes it is kept, in
    /// place of any kept before; if not, what was kept is left as it was. A licence that cannot be written down
    /// (no file, or the file refused) still stands for now: the person has shown it.
    pub fn enter(&self, text: &str) -> Standing {
        let licence = match self.judge(text) {
            Ok(licence) => licence,
            Err(why) => return Standing::Unlicensed(why),
        };
        // kept as it was issued: the licence alone, not the email it came in
        if let (Some(file), Some(token)) = (&self.file, token_in(text)) {
            let _ = keep(file, &block(&token));
        }
        Standing::Licensed { name: licence.name, id: licence.id }
    }
}

/// `text` written to `file`, its folder made if need be, readable by its owner alone, and whole or not at all: it is
/// written beside the file and moved over it.
fn keep(file: &Path, text: &str) -> std::io::Result<()> {
    use std::io::Write;
    if let Some(dir) = file.parent() {
        std::fs::create_dir_all(dir)?;
    }
    let beside = file.with_extension("writing");
    let mut options = std::fs::OpenOptions::new();
    options.write(true).create(true).truncate(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.mode(0o600);
    }
    let mut out = options.open(&beside)?;
    out.write_all(text.as_bytes())?;
    out.sync_all()?;
    drop(out);
    std::fs::rename(&beside, file)
}

/// Where a user's licence is kept, on `os` (std::env::consts::OS), given their home folder and, on Windows, their
/// roaming application data. `named`: a file named outright (SONIC_PI_LICENCE_FILE), which wins.
pub fn file_in(named: Option<&str>, os: &str, home: Option<&Path>, appdata: Option<&Path>) -> Option<PathBuf> {
    if let Some(named) = named.filter(|n| !n.is_empty()) {
        return Some(PathBuf::from(named));
    }
    match os {
        "macos" => Some(home?.join("Library").join("Application Support").join("Sonic Pi").join("plugin-licence.txt")),
        "windows" => Some(appdata?.join("Sonic Pi").join("plugin-licence.txt")),
        _ => Some(home?.join(".config").join("sonic-pi").join("plugin-licence.txt")),
    }
}

/// Where this user's licence is kept, on this machine.
pub fn file() -> Option<PathBuf> {
    let var = |name: &str| std::env::var_os(name).filter(|v| !v.is_empty());
    let named = std::env::var("SONIC_PI_LICENCE_FILE").ok();
    file_in(named.as_deref(), std::env::consts::OS, var("HOME").map(PathBuf::from).as_deref(), var("APPDATA").map(PathBuf::from).as_deref())
}

#[cfg(test)]
#[path = "keeper_tests.rs"]
mod tests;
