// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! A Sonic Pi plugin licence: what one holds, how one is signed and how one is checked.
//!
//! A licence is for a person. It names them, and covers a range of Sonic Pi versions on any machine and in any
//! plugin format. It is a short text signed with an Ed25519 key: the plugin holds the public half and checks a
//! licence with no network and no server (`read`, then `Licence::covers`), and whoever issues licences holds the
//! private half (`sign`).
//!
//! As text it is one token between two marker lines:
//!
//! ```text
//! -----BEGIN SONIC PI LICENCE-----
//! SPL1.<the licence's fields, as JSON, in base64>.<the signature, in base64>
//! -----END SONIC PI LICENCE-----
//! ```
//!
//! The signature is over the fields' bytes exactly as they are in the token, so nothing has to be put back into a
//! canonical form to check it. Email rewraps lines, quotes them with `>` and changes line endings: `read` takes
//! the token out of whatever is around it and ignores white space and `>` inside it. The base64 is the standard
//! alphabet, not the URL one, because that one has `-` in it and the marker lines begin with one.

use base64::Engine;
use base64::engine::general_purpose::STANDARD_NO_PAD as BASE64;
use ed25519_dalek::{Signature, Signer, SigningKey, VerifyingKey};
use sha2::{Digest, Sha256};

/// The product a plugin licence is for.
pub const PRODUCT: &str = "sonic-pi-plugin";

const TAG: &str = "SPL1.";
const BEGIN: &str = "-----BEGIN SONIC PI LICENCE-----";
const END: &str = "-----END SONIC PI LICENCE-----";
/// What is signed is this, then the fields' bytes: a signature made for a licence is good for nothing else.
const SIGNED_AS: &[u8] = b"sonic-pi-licence-1\n";
/// The longest name a licence holds, in characters.
const NAME_MAX: usize = 200;

/// A licence's fields. `from` and `until` are Sonic Pi versions as (major, minor): the licence covers `from` and
/// every version after it up to, and not including, `until`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Licence {
    /// The licence's number, as `new_id` makes one. A revocation lists its mark (`revocation_mark`), not the number.
    pub id: String,
    /// Who the licence is for, as the plugin shows it.
    pub name: String,
    pub email: String,
    /// The day it was issued, as YYYY-MM-DD.
    pub issued: String,
    pub product: String,
    pub from: (u16, u16),
    pub until: (u16, u16),
    /// The number of the key that signed it: a plugin holds more than one public key (one in use, one spare).
    pub key: u8,
}

/// A public key a plugin checks licences with, and the number licences name it by.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PublicKey {
    pub number: u8,
    pub bytes: [u8; 32],
}

/// Why a text was not read as a licence.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReadError {
    /// There is no licence in the text.
    NotALicence,
    /// There is one, but part of it is missing or is not what a licence holds.
    Damaged,
    /// It is a kind of licence made after this code was.
    Unsupported,
    /// It names a key this plugin does not hold.
    UnknownKey(u8),
    /// It was not signed by the key it names: its fields were changed, or someone else signed it.
    BadSignature,
}

/// Why a genuine licence does not cover what was asked of it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Refusal {
    WrongProduct,
    /// The version asked about is outside the licence's range, which is given.
    Version { from: (u16, u16), until: (u16, u16) },
    Revoked,
}

/// Why a licence was not signed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IssueError {
    /// No name, one over 200 characters, or one with a line break or another control character in it.
    Name,
    Email,
    /// `until` is not after `from`.
    Range,
}

/// The public half of a private key, under the number licences will name it by.
pub fn public_key_of(private: &[u8; 32], number: u8) -> PublicKey {
    PublicKey { number, bytes: SigningKey::from_bytes(private).verifying_key().to_bytes() }
}

/// The licence as text, signed with `private`: the key whose number is `licence.key`.
pub fn sign(licence: &Licence, private: &[u8; 32]) -> Result<String, IssueError> {
    let name = licence.name.trim();
    if name.is_empty() || name.chars().count() > NAME_MAX || name.chars().any(char::is_control) {
        return Err(IssueError::Name);
    }
    let email = licence.email.trim();
    let address = email.split_once('@').filter(|(who, place)| {
        !who.is_empty() && place.contains('.') && !place.starts_with('.') && !place.ends_with('.') && !place.contains('@')
    });
    if address.is_none() || email.len() > 254 || email.chars().any(|c| c.is_whitespace() || c.is_control()) {
        return Err(IssueError::Email);
    }
    if licence.until <= licence.from {
        return Err(IssueError::Range);
    }
    let fields = serde_json::json!({
        "v": 1,
        "id": licence.id,
        "name": name,
        "email": email,
        "issued": licence.issued,
        "product": licence.product,
        "from": [licence.from.0, licence.from.1],
        "until": [licence.until.0, licence.until.1],
        "key": licence.key,
    })
    .to_string();
    let signature = SigningKey::from_bytes(private).sign(&signed(fields.as_bytes()));
    Ok(block(&format!("{}.{}", BASE64.encode(fields.as_bytes()), BASE64.encode(signature.to_bytes()))))
}

/// A licence as text from its token (the fields and the signature, in base64 with a dot between): the two marker
/// lines, and the tagged token between them in lines of 64.
pub(crate) fn block(token: &str) -> String {
    let tagged = format!("{TAG}{token}");
    let mut text = String::from(BEGIN);
    // the token is ASCII, so any 64 bytes of it are 64 characters
    for line in tagged.as_bytes().chunks(64) {
        text.push('\n');
        text.push_str(std::str::from_utf8(line).expect("a token is ASCII"));
    }
    text.push('\n');
    text.push_str(END);
    text.push('\n');
    text
}

/// The token of the licence in `text`, out of whatever is around it: white space and `>` (a quoted email) are not
/// part of one. None if there is no licence there.
pub(crate) fn token_in(text: &str) -> Option<String> {
    let packed: String = text.chars().filter(|c| !c.is_whitespace() && *c != '>').collect();
    let start = packed.find(TAG)? + TAG.len();
    packed[start..].split(|c: char| !(c.is_ascii_alphanumeric() || matches!(c, '+' | '/' | '.'))).next().map(str::to_string)
}

/// The licence in `text`, if there is one and it was signed by one of `keys`. The text may be a whole email.
/// This says a licence is genuine; `Licence::covers` says whether it is good for a product and a version.
pub fn read(text: &str, keys: &[PublicKey]) -> Result<Licence, ReadError> {
    let token = token_in(text).ok_or(ReadError::NotALicence)?;
    let (fields, signature) = token.split_once('.').ok_or(ReadError::Damaged)?;
    let fields = BASE64.decode(fields).map_err(|_| ReadError::Damaged)?;
    let signature = BASE64.decode(signature).map_err(|_| ReadError::Damaged)?;
    let signature = Signature::from_slice(&signature).map_err(|_| ReadError::Damaged)?;
    let json: serde_json::Value = serde_json::from_slice(&fields).map_err(|_| ReadError::Damaged)?;
    if json.get("v").and_then(|v| v.as_u64()).ok_or(ReadError::Damaged)? != 1 {
        return Err(ReadError::Unsupported);
    }
    let string = |field: &str| json.get(field).and_then(|v| v.as_str()).map(str::to_string).ok_or(ReadError::Damaged);
    let version = |field: &str| -> Result<(u16, u16), ReadError> {
        let pair = json.get(field).and_then(|v| v.as_array()).filter(|a| a.len() == 2).ok_or(ReadError::Damaged)?;
        let part = |i: usize| pair[i].as_u64().and_then(|n| u16::try_from(n).ok()).ok_or(ReadError::Damaged);
        Ok((part(0)?, part(1)?))
    };
    let key = json.get("key").and_then(|v| v.as_u64()).and_then(|n| u8::try_from(n).ok()).ok_or(ReadError::Damaged)?;
    let licence = Licence {
        id: string("id")?,
        name: string("name")?,
        email: string("email")?,
        issued: string("issued")?,
        product: string("product")?,
        from: version("from")?,
        until: version("until")?,
        key,
    };
    let public = keys.iter().find(|k| k.number == key).ok_or(ReadError::UnknownKey(key))?;
    let public = VerifyingKey::from_bytes(&public.bytes).map_err(|_| ReadError::BadSignature)?;
    public.verify_strict(&signed(&fields), &signature).map_err(|_| ReadError::BadSignature)?;
    Ok(licence)
}

fn signed(fields: &[u8]) -> Vec<u8> {
    [SIGNED_AS, fields].concat()
}

impl Licence {
    /// Whether this licence is good for `product` at Sonic Pi `version`, given the marks of the licences that have
    /// been revoked (`revocation_mark`; as they sit in a list: space around a mark and its case are ignored).
    pub fn covers(&self, product: &str, version: (u16, u16), revoked: &[&str]) -> Result<(), Refusal> {
        if self.product != product {
            return Err(Refusal::WrongProduct);
        }
        let mark = revocation_mark(&self.id);
        if revoked.iter().any(|listed| listed.trim().eq_ignore_ascii_case(&mark)) {
            return Err(Refusal::Revoked);
        }
        if version < self.from || version >= self.until {
            return Err(Refusal::Version { from: self.from, until: self.until });
        }
        Ok(())
    }
}

/// What a list of revoked licences holds for the licence numbered `id`: a hash of the number, in hex. The list is
/// in every plugin and in its public source, so it names no licence: a mark cannot be turned back into a number.
pub fn revocation_mark(id: &str) -> String {
    let mut hash = Sha256::new();
    hash.update(b"sonic-pi-licence-revoked\n");
    hash.update(id.trim().to_ascii_uppercase().as_bytes());
    hash.finalize().iter().map(|byte| format!("{byte:02x}")).collect()
}

/// A new licence number, `SP-XXXXX-XXXXX`: fifty random bits in letters and digits that are not mistaken for one
/// another when read out or typed (no I, L, O or U).
pub fn new_id() -> String {
    const ALPHABET: &[u8; 32] = b"0123456789ABCDEFGHJKMNPQRSTVWXYZ";
    let mut random = [0u8; 8];
    getrandom::fill(&mut random).expect("the system's random numbers");
    let mut bits = u64::from_le_bytes(random);
    let mut id = String::from("SP-");
    for i in 0..10 {
        if i == 5 {
            id.push('-');
        }
        id.push(ALPHABET[(bits & 31) as usize] as char);
        bits >>= 5;
    }
    id
}

/// The (major, minor) of a Sonic Pi release version as its VERSION file writes it: "5.1.0-beta1" is (5, 1).
pub fn version_of(text: &str) -> Option<(u16, u16)> {
    let text = text.trim();
    let mut parts = text.strip_prefix('v').unwrap_or(text).split('.');
    let number = |part: Option<&str>| -> Option<u16> {
        let digits: String = part?.chars().take_while(char::is_ascii_digit).collect();
        digits.parse().ok()
    };
    let major = number(parts.next())?;
    Some((major, number(parts.next()).unwrap_or(0)))
}

pub mod keeper;
pub use keeper::{Keeper, Standing, Why};

#[cfg(test)]
mod tests;
