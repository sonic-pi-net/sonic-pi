// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! The command-line tool: keys made, licences issued, revoked, listed and checked, in a folder of its own. Every test
//! works in a folder made for it, with keys made there.

use sonic_pi_licence::{read, revocation_mark, PublicKey, PRODUCT};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// A folder for one test, gone when the test is.
struct Folder(PathBuf);
impl Folder {
    fn new(name: &str) -> Folder {
        let path = std::env::temp_dir().join(format!("sonic-pi-licence-test-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&path);
        std::fs::create_dir_all(&path).unwrap();
        Folder(path)
    }
    fn file(&self, name: &str) -> PathBuf {
        self.0.join(name)
    }
}
impl Drop for Folder {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn tool(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_sonic-pi-licence")).arg("--dir").arg(dir).args(args).output().expect("the tool ran")
}
fn tool_with(dir: &Path, args: &[&str], input: &str) -> Output {
    use std::io::Write;
    let mut child = Command::new(env!("CARGO_BIN_EXE_sonic-pi-licence"))
        .arg("--dir")
        .arg(dir)
        .args(args)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("the tool ran");
    child.stdin.take().unwrap().write_all(input.as_bytes()).unwrap();
    child.wait_with_output().unwrap()
}
fn out(o: &Output) -> String {
    String::from_utf8_lossy(&o.stdout).into_owned()
}
fn err(o: &Output) -> String {
    String::from_utf8_lossy(&o.stderr).into_owned()
}

/// The public key the tool wrote for key `number`.
fn public_key(dir: &Folder, number: u8) -> PublicKey {
    let hex = std::fs::read_to_string(dir.file(&format!("key-{number}.public"))).unwrap();
    let bytes: Vec<u8> = (0..32).map(|i| u8::from_str_radix(&hex.trim()[i * 2..i * 2 + 2], 16).unwrap()).collect();
    PublicKey { number, bytes: bytes.try_into().unwrap() }
}

const ISSUE: &[&str] = &["issue", "--name", "Ada Lovelace", "--email", "ada@example.com", "--date", "2026-11-27"];

#[test]
fn keygen_makes_a_private_and_a_public_key_and_never_writes_over_one() {
    let dir = Folder::new("keygen");
    let made = tool(&dir.0, &["keygen", "1"]);
    assert!(made.status.success(), "{}", err(&made));
    let private = std::fs::read_to_string(dir.file("key-1.private")).unwrap();
    let public = std::fs::read_to_string(dir.file("key-1.public")).unwrap();
    assert_eq!(private.trim().len(), 64, "32 bytes as hex");
    assert_eq!(public.trim().len(), 64);
    assert!(out(&made).contains(public.trim()), "the public key is said, for the plugin to carry: {}", out(&made));
    assert!(!out(&made).contains(private.trim()), "the private key is never printed");
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        assert_eq!(std::fs::metadata(dir.file("key-1.private")).unwrap().permissions().mode() & 0o777, 0o600, "only its owner reads the private key");
    }

    let again = tool(&dir.0, &["keygen", "1"]);
    assert!(!again.status.success(), "a key that exists is not made again");
    assert_eq!(std::fs::read_to_string(dir.file("key-1.private")).unwrap(), private, "and is left as it was");

    let second = tool(&dir.0, &["keygen", "2"]);
    assert!(second.status.success());
    assert_ne!(std::fs::read_to_string(dir.file("key-2.private")).unwrap(), private, "another key is another key");
}

#[test]
fn a_licence_issued_is_genuine_and_is_written_in_the_record() {
    let dir = Folder::new("issue");
    assert!(tool(&dir.0, &["keygen", "1"]).status.success());
    let issued = tool(&dir.0, &[ISSUE, &["--order", "txn_123"]].concat());
    assert!(issued.status.success(), "{}", err(&issued));
    let text = out(&issued);
    assert!(text.contains("-----BEGIN SONIC PI LICENCE-----"), "{text}");

    let licence = read(&text, &[public_key(&dir, 1)]).expect("signed by the key made here");
    assert_eq!(licence.name, "Ada Lovelace");
    assert_eq!(licence.email, "ada@example.com");
    assert_eq!(licence.issued, "2026-11-27");
    assert_eq!(licence.product, PRODUCT);
    assert_eq!((licence.from, licence.until, licence.key), ((5, 0), (6, 0), 1), "Sonic Pi 5, by key 1, unless said otherwise");
    assert!(licence.id.starts_with("SP-"));

    let list = out(&tool(&dir.0, &["list"]));
    for part in [licence.id.as_str(), "Ada Lovelace", "ada@example.com", "2026-11-27", "txn_123", "5.0", "6.0"] {
        assert!(list.contains(part), "the record shows {part}: {list}");
    }
}

/// A shop is told of a sale more than once (a notice sent again, a delivery tried again), and the buyer is owed one
/// licence, not one for each telling.
#[test]
fn an_order_is_issued_one_licence_however_often_it_is_asked_for() {
    let dir = Folder::new("order-once");
    assert!(tool(&dir.0, &["keygen", "1"]).status.success());
    let ask = |order: &str, name: &str| tool(&dir.0, &["issue", "--name", name, "--email", "ada@example.com", "--order", order]);

    let first = ask("txn_01abc", "Ada Lovelace");
    assert!(first.status.success(), "{}", err(&first));
    let again = ask("txn_01abc", "Ada Lovelace");
    assert!(again.status.success(), "{}", err(&again));
    assert_eq!(out(&first), out(&again), "the same licence, to the letter");
    // asked for again with other details (the buyer's name corrected at the shop since): still the licence issued
    assert_eq!(out(&first), out(&ask("txn_01abc", "A. Lovelace")));

    let other = ask("txn_02xyz", "Ada Lovelace");
    assert_ne!(out(&first), out(&other), "another order is another licence");

    let record = std::fs::read_to_string(dir.file("record.jsonl")).unwrap();
    assert_eq!(record.lines().count(), 2, "one line for each order: {record}");

    // with no order named there is nothing to know it by: each asking is a licence, as before
    let loose = |()| tool(&dir.0, &["issue", "--name", "Ada Lovelace", "--email", "ada@example.com"]);
    assert_ne!(out(&loose(())), out(&loose(())));
}

/// A shop knows a refund by its order, not by the licence's number.
#[test]
fn a_licence_can_be_revoked_by_the_order_it_was_issued_for() {
    let dir = Folder::new("revoke-by-order");
    assert!(tool(&dir.0, &["keygen", "1"]).status.success());
    let issued = tool(&dir.0, &["issue", "--name", "Ada Lovelace", "--email", "ada@example.com", "--order", "txn_01abc"]);
    let licence = read(&out(&issued), &[public_key(&dir, 1)]).unwrap();

    let revoked = tool(&dir.0, &["revoke", "--order", "txn_01abc", "--why", "refunded"]);
    assert!(revoked.status.success(), "{}", err(&revoked));
    assert!(out(&tool(&dir.0, &["marks"])).contains(&revocation_mark(&licence.id)));

    let again = tool(&dir.0, &["revoke", "--order", "txn_01abc"]);
    assert!(!again.status.success());
    assert!(err(&again).contains("was revoked on"), "{}", err(&again));

    let nobody = tool(&dir.0, &["revoke", "--order", "txn_nobody"]);
    assert!(!nobody.status.success());
    assert!(err(&nobody).contains("no licence was issued here for order txn_nobody"), "{}", err(&nobody));
}

/// A refunded order's licence is revoked: asking for that order again does not hand the revoked licence out as good.
#[test]
fn an_order_whose_licence_was_revoked_is_issued_nothing_more() {
    let dir = Folder::new("order-revoked");
    assert!(tool(&dir.0, &["keygen", "1"]).status.success());
    let issued = tool(&dir.0, &["issue", "--name", "Ada Lovelace", "--email", "ada@example.com", "--order", "txn_01abc"]);
    let licence = read(&out(&issued), &[public_key(&dir, 1)]).unwrap();
    assert!(tool(&dir.0, &["revoke", &licence.id, "--why", "refunded"]).status.success());

    let again = tool(&dir.0, &["issue", "--name", "Ada Lovelace", "--email", "ada@example.com", "--order", "txn_01abc"]);
    assert!(!again.status.success());
    assert!(out(&again).is_empty(), "nothing is handed over");
    assert!(err(&again).contains("txn_01abc") && err(&again).contains("revoked"), "{}", err(&again));
}

#[test]
fn a_licence_can_be_for_other_versions_and_by_another_key() {
    let dir = Folder::new("range");
    assert!(tool(&dir.0, &["keygen", "2"]).status.success());
    let issued = tool(&dir.0, &[ISSUE, &["--from", "6.0", "--until", "7.0", "--key", "2"]].concat());
    assert!(issued.status.success(), "{}", err(&issued));
    let licence = read(&out(&issued), &[public_key(&dir, 2)]).unwrap();
    assert_eq!((licence.from, licence.until, licence.key), ((6, 0), (7, 0), 2));
}

#[test]
fn nothing_is_issued_without_a_key_a_name_or_an_address() {
    let dir = Folder::new("refuse");
    let no_key = tool(&dir.0, ISSUE);
    assert!(!no_key.status.success() && out(&no_key).is_empty(), "no key 1 yet: {}", err(&no_key));
    assert!(tool(&dir.0, &["keygen", "1"]).status.success());
    for args in [&["issue", "--email", "ada@example.com"][..], &["issue", "--name", "Ada"], &["issue", "--name", "Ada", "--email", "not an address"], &["issue", "--name", "Ada", "--email", "ada@example.com", "--until", "4.0"]] {
        let o = tool(&dir.0, args);
        assert!(!o.status.success() && out(&o).is_empty(), "{args:?} is refused, with nothing printed as a licence: {}", out(&o));
    }
    assert!(out(&tool(&dir.0, &["list"])).trim().is_empty(), "and nothing was recorded");
}

#[test]
fn a_revoked_licence_is_marked_in_the_record_and_its_mark_is_in_the_list_for_a_release() {
    let dir = Folder::new("revoke");
    assert!(tool(&dir.0, &["keygen", "1"]).status.success());
    let first = read(&out(&tool(&dir.0, ISSUE)), &[public_key(&dir, 1)]).unwrap();
    let second = read(&out(&tool(&dir.0, &["issue", "--name", "Grace Hopper", "--email", "grace@example.com", "--date", "2026-11-28"])), &[public_key(&dir, 1)]).unwrap();
    assert!(out(&tool(&dir.0, &["marks"])).trim().is_empty(), "nothing revoked yet");

    let revoked = tool(&dir.0, &["revoke", &first.id, "--why", "refunded", "--date", "2026-12-01"]);
    assert!(revoked.status.success(), "{}", err(&revoked));
    let marks = out(&tool(&dir.0, &["marks"]));
    assert_eq!(marks.trim(), revocation_mark(&first.id), "the one mark, and no licence number: {marks}");
    assert!(!marks.contains(&first.id) && !marks.contains("Ada"), "a mark names nobody");
    assert_eq!(first.covers(PRODUCT, (5, 1), &[marks.trim()]), Err(sonic_pi_licence::Refusal::Revoked));
    assert_eq!(second.covers(PRODUCT, (5, 1), &[marks.trim()]), Ok(()), "the other licence is untouched");

    let list = out(&tool(&dir.0, &["list"]));
    let line = list.lines().find(|l| l.contains(&first.id)).unwrap();
    assert!(line.contains("revoked 2026-12-01") && line.contains("refunded"), "{line}");
    assert!(!list.lines().find(|l| l.contains(&second.id)).unwrap().contains("revoked"));

    assert!(!tool(&dir.0, &["revoke", &first.id]).status.success(), "it is not revoked twice");
    assert!(!tool(&dir.0, &["revoke", "SP-00000-00000"]).status.success(), "nor is a licence that was never issued");
}

#[test]
fn check_says_whether_a_licence_is_genuine_whose_it_is_and_whether_it_still_stands() {
    let dir = Folder::new("check");
    assert!(tool(&dir.0, &["keygen", "1"]).status.success());
    let text = out(&tool(&dir.0, ISSUE));
    let id = read(&text, &[public_key(&dir, 1)]).unwrap().id;

    let good = tool_with(&dir.0, &["check", "--version", "5.1"], &format!("Hi Ada,\n\nyour licence:\n\n{text}\nthanks\n"));
    assert!(good.status.success(), "{}", err(&good));
    for part in ["Ada Lovelace", "ada@example.com", id.as_str(), "genuine"] {
        assert!(out(&good).contains(part), "{}", out(&good));
    }

    let v6 = tool_with(&dir.0, &["check", "--version", "6.0"], &text);
    assert!(!v6.status.success(), "not for Sonic Pi 6: {}", out(&v6));

    let changed = tool_with(&dir.0, &["check", "--version", "5.1"], &text.replace("SPL1.ey", "SPL1.eY"));
    assert!(!changed.status.success(), "a changed licence is not genuine");

    assert!(tool(&dir.0, &["revoke", &id, "--date", "2026-12-01"]).status.success());
    let after = tool_with(&dir.0, &["check", "--version", "5.1"], &text);
    assert!(!after.status.success() && (out(&after) + &err(&after)).contains("revoked"), "revoked: {}", out(&after));

    let none = tool_with(&dir.0, &["check", "--version", "5.1"], "no licence here");
    assert!(!none.status.success());
}

#[test]
fn it_works_only_in_a_folder_it_is_given() {
    let o = Command::new(env!("CARGO_BIN_EXE_sonic-pi-licence")).arg("list").env_remove("SONIC_PI_LICENCE_DIR").output().unwrap();
    assert!(!o.status.success(), "with no folder named it does nothing");
    assert!(err(&o).contains("--dir"), "and says how to name one: {}", err(&o));
}
