// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! sonic-pi-licence: makes keys, and issues, revokes, lists and checks licences.
//!
//! It works in one folder, named with --dir (or SONIC_PI_LICENCE_DIR), and nowhere else:
//!
//!   key-<n>.private   a signing key, 32 bytes as hex, readable only by its owner. Whoever holds it can issue
//!                     licences: it is kept off any machine that does not issue them, and never in a repository
//!   key-<n>.public    its public half, which every plugin carries
//!   record.jsonl      every licence issued and every one revoked, a line each, added to and never rewritten.
//!                     It holds names and addresses: it is personal data, and is not for a repository either
//!
//!   sonic-pi-licence --dir D keygen 1
//!   sonic-pi-licence --dir D issue --name "Ada Lovelace" --email ada@example.com [--order txn_…] [--from 5.0] [--until 6.0] [--key 1]
//!   sonic-pi-licence --dir D revoke SP-XXXXX-XXXXX [--why refunded]
//!   sonic-pi-licence --dir D list
//!   sonic-pi-licence --dir D marks                  the revoked licences' marks, a line each: what a release carries
//!   sonic-pi-licence --dir D check --version 5.1 < an-email.txt
//!
//! `marks` is the one thing here meant to be published. A mark is a hash of a licence's number, which says nothing
//! of whose it was (lib.rs revocation_mark).

use sonic_pi_licence::{new_id, public_key_of, read, revocation_mark, sign, version_of, Licence, PublicKey, ReadError, Refusal, PRODUCT};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

const USAGE: &str = "usage: sonic-pi-licence --dir <folder> <keygen N | issue --name … --email … | revoke <number> | list | marks | check --version X.Y>";

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if let Err(why) = run(args) {
        eprintln!("{why}");
        std::process::exit(1);
    }
}

/// The value after `--name` in `args`, taken out of them.
fn option(args: &mut Vec<String>, name: &str) -> Result<Option<String>, String> {
    let Some(at) = args.iter().position(|a| a == name) else { return Ok(None) };
    if at + 1 >= args.len() {
        return Err(format!("{name} needs a value"));
    }
    args.remove(at);
    Ok(Some(args.remove(at)))
}

fn run(mut args: Vec<String>) -> Result<(), String> {
    let dir = option(&mut args, "--dir")?
        .or_else(|| std::env::var("SONIC_PI_LICENCE_DIR").ok().filter(|d| !d.is_empty()))
        .map(PathBuf::from)
        .ok_or_else(|| format!("no folder named: --dir <folder>, or SONIC_PI_LICENCE_DIR\n{USAGE}"))?;
    if !dir.is_dir() {
        return Err(format!("{} is not a folder", dir.display()));
    }
    let date = option(&mut args, "--date")?;
    let command = if args.is_empty() { String::new() } else { args.remove(0) };
    match command.as_str() {
        "keygen" => keygen(&dir, &args),
        "issue" => issue(&dir, args, date),
        "revoke" => revoke(&dir, args, date),
        "list" => list(&dir),
        "marks" => {
            let mut marks: Vec<String> = record(&dir)?.iter().filter(|e| e.revoked.is_some()).map(|e| revocation_mark(&e.licence.id)).collect();
            marks.sort();
            for mark in marks {
                println!("{mark}");
            }
            Ok(())
        }
        "check" => check(&dir, args),
        _ => Err(USAGE.to_string()),
    }
}

// ── Keys ────────────────────────────────────────────────────────────────────

fn key_number(text: &str) -> Result<u8, String> {
    text.parse::<u8>().ok().filter(|n| *n > 0).ok_or_else(|| format!("a key's number is 1 to 255, not {text}"))
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn unhex(text: &str) -> Option<[u8; 32]> {
    let text = text.trim();
    if text.len() != 64 || !text.is_ascii() {
        return None;
    }
    let mut bytes = [0u8; 32];
    for (i, byte) in bytes.iter_mut().enumerate() {
        *byte = u8::from_str_radix(&text[i * 2..i * 2 + 2], 16).ok()?;
    }
    Some(bytes)
}

fn keygen(dir: &Path, args: &[String]) -> Result<(), String> {
    let number = key_number(args.first().ok_or("keygen needs the key's number: keygen 1")?)?;
    let (private_path, public_path) = (dir.join(format!("key-{number}.private")), dir.join(format!("key-{number}.public")));
    if private_path.exists() || public_path.exists() {
        return Err(format!("key {number} is already in {}: a key is never made again over one", dir.display()));
    }
    let mut private = [0u8; 32];
    getrandom::fill(&mut private).map_err(|e| format!("no random numbers from the system: {e}"))?;
    let public = public_key_of(&private, number);
    // made readable by its owner alone, and failing if it has appeared meanwhile: never written over
    let mut options = std::fs::OpenOptions::new();
    options.write(true).create_new(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.mode(0o600);
    }
    let mut file = options.open(&private_path).map_err(|e| format!("{}: {e}", private_path.display()))?;
    writeln!(file, "{}", hex(&private)).map_err(|e| format!("{}: {e}", private_path.display()))?;
    std::fs::write(&public_path, format!("{}\n", hex(&public.bytes))).map_err(|e| format!("{}: {e}", public_path.display()))?;
    println!("key {number} made in {}", dir.display());
    println!("public: {}", hex(&public.bytes));
    println!("key-{number}.private signs licences: keep it where licences are issued and nowhere else, with a copy somewhere safe and offline.");
    Ok(())
}

fn private_key(dir: &Path, number: u8) -> Result<[u8; 32], String> {
    let path = dir.join(format!("key-{number}.private"));
    let text = std::fs::read_to_string(&path).map_err(|_| format!("no key {number} in {}: keygen {number} makes one", dir.display()))?;
    unhex(&text).ok_or_else(|| format!("{} is not a key", path.display()))
}

/// Every public key in the folder.
fn public_keys(dir: &Path) -> Result<Vec<PublicKey>, String> {
    let mut keys = Vec::new();
    for entry in std::fs::read_dir(dir).map_err(|e| format!("{}: {e}", dir.display()))?.flatten() {
        let name = entry.file_name().to_string_lossy().into_owned();
        let Some(number) = name.strip_prefix("key-").and_then(|n| n.strip_suffix(".public")).and_then(|n| n.parse::<u8>().ok()) else { continue };
        let bytes = std::fs::read_to_string(entry.path()).ok().and_then(|t| unhex(&t)).ok_or_else(|| format!("{name} is not a key"))?;
        keys.push(PublicKey { number, bytes });
    }
    Ok(keys)
}

// ── The record ──────────────────────────────────────────────────────────────

/// A licence as the record has it: what was issued, the order it was for, and its revoking if it was.
struct Entry {
    licence: Licence,
    order: String,
    revoked: Option<(String, String)>, // when, why
}

fn record_path(dir: &Path) -> PathBuf {
    dir.join("record.jsonl")
}

fn record(dir: &Path) -> Result<Vec<Entry>, String> {
    let path = record_path(dir);
    let text = match std::fs::read_to_string(&path) {
        Ok(text) => text,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(format!("{}: {e}", path.display())),
    };
    let mut entries: Vec<Entry> = Vec::new();
    for (n, line) in text.lines().enumerate().filter(|(_, l)| !l.trim().is_empty()) {
        let bad = || format!("{} line {}: not a line of the record", path.display(), n + 1);
        let json: serde_json::Value = serde_json::from_str(line).map_err(|_| bad())?;
        let text = |field: &str| json.get(field).and_then(|v| v.as_str()).map(str::to_string).ok_or_else(bad);
        let version = |field: &str| json.get(field).and_then(|v| v.as_str()).and_then(version_of).ok_or_else(bad);
        match text("t")?.as_str() {
            "issued" => entries.push(Entry {
                licence: Licence {
                    id: text("id")?,
                    name: text("name")?,
                    email: text("email")?,
                    issued: text("issued")?,
                    product: text("product")?,
                    from: version("from")?,
                    until: version("until")?,
                    key: json.get("key").and_then(|v| v.as_u64()).and_then(|k| u8::try_from(k).ok()).ok_or_else(bad)?,
                },
                order: text("order")?,
                revoked: None,
            }),
            "revoked" => {
                let id = text("id")?;
                entries.iter_mut().find(|e| e.licence.id == id).ok_or_else(bad)?.revoked = Some((text("when")?, text("why")?));
            }
            _ => return Err(bad()),
        }
    }
    Ok(entries)
}

/// A line added to the record, and on the disk before this returns.
fn record_add(dir: &Path, line: &serde_json::Value) -> Result<(), String> {
    let path = record_path(dir);
    let mut options = std::fs::OpenOptions::new();
    options.append(true).create(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.mode(0o600);
    }
    let mut file = options.open(&path).map_err(|e| format!("{}: {e}", path.display()))?;
    writeln!(file, "{line}").and_then(|_| file.sync_all()).map_err(|e| format!("{}: {e}", path.display()))
}

fn dotted(v: (u16, u16)) -> String {
    format!("{}.{}", v.0, v.1)
}

// ── The day ─────────────────────────────────────────────────────────────────

/// Today as YYYY-MM-DD (UTC), or the day given with --date.
fn day(given: Option<String>) -> Result<String, String> {
    if let Some(date) = given {
        let ok = date.len() == 10 && date.bytes().enumerate().all(|(i, b)| if i == 4 || i == 7 { b == b'-' } else { b.is_ascii_digit() });
        return if ok { Ok(date) } else { Err(format!("a date is YYYY-MM-DD, not {date}")) };
    }
    let seconds = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map_err(|_| "the clock is before 1970")?.as_secs();
    Ok(civil(seconds / 86_400))
}

/// The date `days` after 1 January 1970, in the Gregorian calendar (Howard Hinnant's civil_from_days).
fn civil(days: u64) -> String {
    let z = days as i64 + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z.rem_euclid(146_097);
    let yoe = (doe - doe / 1_460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let (d, m) = (doy - (153 * mp + 2) / 5 + 1, if mp < 10 { mp + 3 } else { mp - 9 });
    let y = yoe + era * 400 + i64::from(m <= 2);
    format!("{y:04}-{m:02}-{d:02}")
}

// ── The commands ────────────────────────────────────────────────────────────

fn issue(dir: &Path, mut args: Vec<String>, date: Option<String>) -> Result<(), String> {
    let name = option(&mut args, "--name")?.ok_or("issue needs --name: who the licence is for")?;
    let email = option(&mut args, "--email")?.ok_or("issue needs --email")?;
    let order = option(&mut args, "--order")?.unwrap_or_default();
    let version = |text: Option<String>, otherwise: (u16, u16)| match text {
        Some(t) => version_of(&t).ok_or_else(|| format!("a version is like 5.0, not {t}")),
        None => Ok(otherwise),
    };
    let from = version(option(&mut args, "--from")?, (5, 0))?;
    let until = version(option(&mut args, "--until")?, (6, 0))?;
    let key = match option(&mut args, "--key")? {
        Some(k) => key_number(&k)?,
        None => 1,
    };
    if let Some(stray) = args.first() {
        return Err(format!("issue does not know {stray}\n{USAGE}"));
    }
    let private = private_key(dir, key)?;
    let issued = record(dir)?;
    let id = loop {
        let id = new_id();
        if !issued.iter().any(|e| e.licence.id == id) {
            break id;
        }
    };
    let licence = Licence { id, name: name.trim().to_string(), email: email.trim().to_string(), issued: day(date)?, product: PRODUCT.to_string(), from, until, key };
    let text = sign(&licence, &private).map_err(|why| format!("not issued: {why:?} is not one a licence can hold"))?;
    // in the record before it is handed over: a licence nobody has a note of cannot be revoked
    record_add(
        dir,
        &serde_json::json!({ "t": "issued", "id": licence.id, "name": licence.name, "email": licence.email, "issued": licence.issued, "product": licence.product,
            "from": dotted(from), "until": dotted(until), "key": key, "order": order }),
    )?;
    print!("{text}");
    Ok(())
}

fn revoke(dir: &Path, mut args: Vec<String>, date: Option<String>) -> Result<(), String> {
    let why = option(&mut args, "--why")?.unwrap_or_default();
    let id = args.first().ok_or("revoke needs the licence's number: revoke SP-XXXXX-XXXXX")?.trim().to_ascii_uppercase();
    let issued = record(dir)?;
    let entry = issued.iter().find(|e| e.licence.id == id).ok_or_else(|| format!("no licence {id} was issued here"))?;
    if let Some((when, _)) = &entry.revoked {
        return Err(format!("{id} was revoked on {when}"));
    }
    record_add(dir, &serde_json::json!({ "t": "revoked", "id": id, "when": day(date)?, "why": why }))?;
    println!("{id} ({}) is revoked. Its mark, for the next release's list:", entry.licence.name);
    println!("{}", revocation_mark(&id));
    Ok(())
}

fn list(dir: &Path) -> Result<(), String> {
    for e in record(dir)? {
        let l = &e.licence;
        let mut line = format!("{}  {}  {} <{}>  Sonic Pi {} up to {}  key {}", l.id, l.issued, l.name, l.email, dotted(l.from), dotted(l.until), l.key);
        if !e.order.is_empty() {
            line += &format!("  order {}", e.order);
        }
        if let Some((when, why)) = &e.revoked {
            line += &format!("  revoked {when}");
            if !why.is_empty() {
                line += &format!(" ({why})");
            }
        }
        println!("{line}");
    }
    Ok(())
}

fn check(dir: &Path, mut args: Vec<String>) -> Result<(), String> {
    let version = option(&mut args, "--version")?.ok_or("check needs --version: the Sonic Pi version asked about, as 5.1")?;
    let version = version_of(&version).ok_or_else(|| format!("a version is like 5.1, not {version}"))?;
    let mut text = String::new();
    match args.first() {
        Some(path) => text = std::fs::read_to_string(path).map_err(|e| format!("{path}: {e}"))?,
        None => {
            std::io::stdin().read_to_string(&mut text).map_err(|e| format!("reading the licence: {e}"))?;
        }
    }
    let licence = read(&text, &public_keys(dir)?).map_err(|why| match why {
        ReadError::NotALicence => "there is no licence in that".to_string(),
        ReadError::Damaged => "the licence is damaged: part of it is missing or changed".to_string(),
        ReadError::Unsupported => "that is a kind of licence newer than this tool".to_string(),
        ReadError::UnknownKey(k) => format!("it names key {k}, which is not in {}", dir.display()),
        ReadError::BadSignature => "it is not genuine: it was not signed by the key it names".to_string(),
    })?;
    println!("genuine: {} for {} <{}>, issued {}, Sonic Pi {} up to {}, signed by key {}", licence.id, licence.name, licence.email, licence.issued, dotted(licence.from), dotted(licence.until), licence.key);
    let marks: Vec<String> = record(dir)?.iter().filter(|e| e.revoked.is_some()).map(|e| revocation_mark(&e.licence.id)).collect();
    let marks: Vec<&str> = marks.iter().map(String::as_str).collect();
    licence.covers(PRODUCT, version, &marks).map_err(|why| match why {
        Refusal::WrongProduct => format!("but it is for {}, not {PRODUCT}", licence.product),
        Refusal::Version { from, until } => format!("but it is for Sonic Pi {} up to {}, not {}", dotted(from), dotted(until), dotted(version)),
        Refusal::Revoked => "but it has been revoked".to_string(),
    })?;
    println!("and good for Sonic Pi {}", dotted(version));
    Ok(())
}
