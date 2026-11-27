// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! Whether this plugin is licensed, and to whom (the licence itself: app/plugin/licence).
//!
//! One answer for the whole process: every instance of the plugin a host holds, in either format, reads the one
//! licence its user keeps. Unlicensed, the plugin opens and shows its page, which asks for a licence, and runs no
//! code (lib.rs refuses a run, and takes no armed buffers). Nothing here uses the network.

use sonic_pi_licence::{keeper, version_of, Keeper, PublicKey, Standing};
use std::sync::{Mutex, OnceLock};

/// The public halves of the keys licences are signed with: the one in use, and a spare kept offline for the day the
/// first is lost. A licence names its key by number.
const KEYS: &[PublicKey] = &[
    // key-1.public: 25dc379e2af620483773bd75eb234cdb02e69dd32a3cc0a985f45b4f4c20999e
    PublicKey {
        number: 1,
        bytes: [
            0x25, 0xdc, 0x37, 0x9e, 0x2a, 0xf6, 0x20, 0x48, 0x37, 0x73, 0xbd, 0x75, 0xeb, 0x23, 0x4c, 0xdb,
            0x02, 0xe6, 0x9d, 0xd3, 0x2a, 0x3c, 0xc0, 0xa9, 0x85, 0xf4, 0x5b, 0x4f, 0x4c, 0x20, 0x99, 0x9e,
        ],
    },
    // key-2.public: e26e76c69851e7870b014829756b0b0a3d841df6f41893990277f2c95c17a5f4
    PublicKey {
        number: 2,
        bytes: [
            0xe2, 0x6e, 0x76, 0xc6, 0x98, 0x51, 0xe7, 0x87, 0x0b, 0x01, 0x48, 0x29, 0x75, 0x6b, 0x0b, 0x0a,
            0x3d, 0x84, 0x1d, 0xf6, 0xf4, 0x18, 0x93, 0x99, 0x02, 0x77, 0xf2, 0xc9, 0x5c, 0x17, 0xa5, 0xf4,
        ],
    },
];

/// A plugin with no keys can be licensed by nobody: one is not built, on any system, unless it is a build for testing.
#[cfg(not(feature = "test-licence-key"))]
const _: () = assert!(!KEYS.is_empty(), "this plugin carries no licence keys (core/licence.rs KEYS)");

/// A build for testing takes licences signed with a key anyone can read here as well (number 255, private half 32
/// bytes of 42). It says so in its binary, so that a release can be checked for it (scripts/release-mac.sh).
#[cfg(feature = "test-licence-key")]
#[used]
static TEST_KEY_SAYS: &str = "sonic-pi-plugin: this build accepts the test licence key";

/// The marks of the licences revoked when this plugin was built.
const REVOKED: &str = include_str!("../licence/revoked.txt");
/// The Sonic Pi release this plugin is of, as the repository's VERSION has it.
const SONIC_PI: &str = include_str!("../../../VERSION");

fn the_keeper() -> &'static Keeper {
    static KEEPER: OnceLock<Keeper> = OnceLock::new();
    KEEPER.get_or_init(|| {
        #[allow(unused_mut)]
        let mut keys = KEYS.to_vec();
        #[cfg(feature = "test-licence-key")]
        keys.push(sonic_pi_licence::public_key_of(&[42; 32], 255));
        Keeper::new(keeper::file(), keys, version_of(SONIC_PI).unwrap_or((0, 0)), REVOKED)
    })
}

/// Licensed once, licensed until the host closes: the file is not read again for every run.
static LICENSED: Mutex<Option<Standing>> = Mutex::new(None);

/// How the plugin stands. Unlicensed, the kept licence is looked for again each time: another instance, or the other
/// format of the plugin, may have been given one since.
pub fn standing() -> Standing {
    let mut licensed = LICENSED.lock().unwrap_or_else(|e| e.into_inner());
    if let Some(standing) = licensed.as_ref() {
        return standing.clone();
    }
    let standing = the_keeper().standing();
    if standing.licensed() {
        *licensed = Some(standing.clone());
    }
    standing
}

pub fn licensed() -> bool {
    standing().licensed()
}

unsafe extern "C" {
    fn sp_clipboard_text(behaviour: *mut std::ffi::c_int) -> *mut std::ffi::c_char;
    fn sp_clipboard_free(text: *mut std::ffi::c_char);
}

/// What a person has copied, where the system lets a program read it without asking them (mac.mm, win.cpp), and what
/// the system said of reading it (mac.mm sp_clipboard_text).
fn copied_text() -> (Option<String>, i32) {
    let mut behaviour: std::ffi::c_int = -1;
    // SAFETY: the text, if any, is a C string of ours to free.
    let text = unsafe {
        let raw = sp_clipboard_text(&mut behaviour);
        if raw.is_null() {
            None
        } else {
            let text = std::ffi::CStr::from_ptr(raw).to_string_lossy().into_owned();
            sp_clipboard_free(raw);
            Some(text)
        }
    };
    (text, behaviour)
}

/// Whose licence a person has copied, if they have copied one this plugin takes: offered on the licence screen, so
/// that entering it is one press. Looked for only while the plugin is unlicensed, and nothing else of what was copied
/// is kept or said. The second value is what the system said of reading the clipboard, for the log.
pub fn copied() -> (Option<String>, i32) {
    let (text, behaviour) = copied_text();
    (text.and_then(|t| the_keeper().whose(&t)), behaviour)
}

/// The licence a person has copied, entered as if they had pasted it.
pub fn enter_copied() -> Standing {
    match copied_text().0 {
        Some(text) => enter(&text),
        None => standing(),
    }
}

/// How the plugin stands, as its page is told (Standing::to_json): and, unlicensed, whose licence was found copied.
pub fn said(standing: &Standing) -> String {
    let mut json: serde_json::Value = serde_json::from_str(&standing.to_json()).unwrap_or_default();
    if !standing.licensed() {
        if let (Some(name), _) = copied() {
            json["copied"] = serde_json::Value::String(name);
        }
    }
    json.to_string()
}

/// A licence a person entered, as pasted. One the plugin takes is kept and the plugin is licensed; one it does not
/// take changes nothing, and the answer says why.
pub fn enter(text: &str) -> Standing {
    let standing = the_keeper().enter(text);
    if standing.licensed() {
        *LICENSED.lock().unwrap_or_else(|e| e.into_inner()) = Some(standing.clone());
    }
    standing
}

#[cfg(test)]
mod tests {
    use super::KEYS;

    /// A plugin with no keys can be licensed by nobody; one without the spare has to be released again the day the
    /// key in use is lost.
    #[test]
    fn the_plugin_carries_the_key_in_use_and_its_spare() {
        let numbers: Vec<u8> = KEYS.iter().map(|k| k.number).collect();
        assert_eq!(numbers, [1, 2], "the keys a plugin carries, by number");
        assert_ne!(KEYS[0].bytes, KEYS[1].bytes, "the spare is a key of its own");
        assert!(KEYS.iter().all(|k| k.bytes != [0; 32]), "a key is not left as noughts");
    }
}
