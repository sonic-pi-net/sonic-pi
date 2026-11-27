// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
use super::*;
use crate::{public_key_of, sign, revocation_mark, Licence, PublicKey, PRODUCT};

const KEY: [u8; 32] = [7; 32];
const OTHER_KEY: [u8; 32] = [11; 32];

fn licence(id: &str, name: &str) -> Licence {
    Licence { id: id.into(), name: name.into(), email: "ada@example.com".into(), issued: "2026-11-27".into(), product: PRODUCT.into(), from: (5, 0), until: (6, 0), key: 1 }
}
fn signed(id: &str, name: &str) -> String {
    sign(&licence(id, name), &KEY).unwrap()
}
fn keys() -> Vec<PublicKey> {
    vec![public_key_of(&KEY, 1)]
}

/// A file in a folder of the test's own, neither of which is there yet.
struct Place(std::path::PathBuf);
impl Place {
    fn new(name: &str) -> Place {
        let dir = std::env::temp_dir().join(format!("sonic-pi-licence-keeper-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        Place(dir)
    }
    fn file(&self) -> std::path::PathBuf {
        self.0.join("Sonic Pi").join("plugin-licence.txt")
    }
    fn keeper(&self, revoked: &str) -> Keeper {
        Keeper::new(Some(self.file()), keys(), (5, 1), revoked)
    }
}
impl Drop for Place {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

#[test]
fn with_no_licence_kept_the_plugin_is_not_licensed_and_says_none_was_entered() {
    let place = Place::new("none");
    assert_eq!(place.keeper("").standing(), Standing::Unlicensed(Why::None));
}

#[test]
fn a_licence_entered_is_kept_and_found_again_by_another_plugin() {
    let place = Place::new("kept");
    let text = signed("SP-AAAAA-11111", "Ada Lovelace");
    let entered = place.keeper("").enter(&format!("Hi Ada,\n\nHere it is:\n\n> {}\nThanks, and my card number is 1234\n", text.replace('\n', "\n> ")));
    assert_eq!(entered, Standing::Licensed { name: "Ada Lovelace".into(), id: "SP-AAAAA-11111".into() });

    // what is kept is the licence and nothing else of what was pasted
    let kept = std::fs::read_to_string(place.file()).unwrap();
    assert_eq!(kept, text, "the licence as it was issued");
    assert!(!kept.contains("card number"));
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        assert_eq!(std::fs::metadata(place.file()).unwrap().permissions().mode() & 0o777, 0o600, "it names its owner: theirs alone to read");
    }

    // another instance, another plugin format, the next day: the same file
    assert_eq!(place.keeper("").standing(), Standing::Licensed { name: "Ada Lovelace".into(), id: "SP-AAAAA-11111".into() });
}

#[test]
fn what_is_refused_is_said_and_a_licence_already_kept_is_left_alone() {
    let place = Place::new("refused");
    let keeper = place.keeper("");
    let good = signed("SP-AAAAA-11111", "Ada Lovelace");
    assert!(matches!(keeper.enter(&good), Standing::Licensed { .. }));

    let by_another = sign(&licence("SP-BBBBB-22222", "Mallory"), &OTHER_KEY).unwrap();
    let for_six = sign(&Licence { from: (6, 0), until: (7, 0), ..licence("SP-CCCCC-33333", "Grace") }, &KEY).unwrap();
    let for_another = sign(&Licence { product: "something-else".into(), ..licence("SP-DDDDD-44444", "Grace") }, &KEY).unwrap();
    let cut_short = &good[..good.len() / 2];
    for (text, why) in [
        ("hello, is this where the licence goes?", Why::NotALicence),
        (cut_short, Why::Damaged),
        (by_another.as_str(), Why::NotGenuine),
        (for_six.as_str(), Why::Version { from: (6, 0), until: (7, 0) }),
        (for_another.as_str(), Why::WrongProduct),
    ] {
        assert_eq!(keeper.enter(text), Standing::Unlicensed(why.clone()), "{why:?}");
        assert_eq!(std::fs::read_to_string(place.file()).unwrap(), good, "the licence kept is still the good one after {why:?}");
        assert!(matches!(keeper.standing(), Standing::Licensed { .. }), "and the plugin is still licensed");
    }
}

#[test]
fn a_licence_revoked_in_a_later_release_stops_standing_and_cannot_be_entered() {
    let place = Place::new("revoked");
    let text = signed("SP-AAAAA-11111", "Ada Lovelace");
    assert!(matches!(place.keeper("").enter(&text), Standing::Licensed { .. }));
    // the next release carries its mark, among others and a comment
    let list = format!("# revoked licences' marks\n{}\n\n{}\n", revocation_mark("SP-ZZZZZ-99999"), revocation_mark("SP-AAAAA-11111"));
    let later = place.keeper(&list);
    assert_eq!(later.standing(), Standing::Unlicensed(Why::Revoked));
    assert_eq!(later.enter(&text), Standing::Unlicensed(Why::Revoked));
    assert!(matches!(later.enter(&signed("SP-EEEEE-55555", "Ada Lovelace")), Standing::Licensed { .. }), "a new licence takes its place");
}

#[test]
fn a_kept_file_that_is_no_licence_is_not_a_licence() {
    let place = Place::new("junk");
    std::fs::create_dir_all(place.file().parent().unwrap()).unwrap();
    std::fs::write(place.file(), "something else was saved here").unwrap();
    assert_eq!(place.keeper("").standing(), Standing::Unlicensed(Why::NotALicence));
}

#[test]
fn with_nowhere_to_keep_it_a_licence_stands_for_the_session_only() {
    let keeper = Keeper::new(None, keys(), (5, 1), "");
    assert_eq!(keeper.standing(), Standing::Unlicensed(Why::None));
    assert!(matches!(keeper.enter(&signed("SP-AAAAA-11111", "Ada Lovelace")), Standing::Licensed { .. }), "it is still judged");
}

#[test]
fn a_standing_is_said_to_the_page_as_json() {
    let licensed: serde_json::Value = serde_json::from_str(&Standing::Licensed { name: "Zoë \"Z\" Müller".into(), id: "SP-AAAAA-11111".into() }.to_json()).unwrap();
    assert_eq!(licensed, serde_json::json!({ "licensed": true, "name": "Zoë \"Z\" Müller", "id": "SP-AAAAA-11111" }));
    for (why, word) in [(Why::None, "none"), (Why::NotALicence, "not-a-licence"), (Why::Damaged, "damaged"), (Why::Unsupported, "unsupported"), (Why::NotGenuine, "not-genuine"), (Why::WrongProduct, "wrong-product"), (Why::Revoked, "revoked")] {
        let said: serde_json::Value = serde_json::from_str(&Standing::Unlicensed(why).to_json()).unwrap();
        assert_eq!(said, serde_json::json!({ "licensed": false, "why": word }));
    }
    let version: serde_json::Value = serde_json::from_str(&Standing::Unlicensed(Why::Version { from: (6, 0), until: (7, 0) }).to_json()).unwrap();
    assert_eq!(version, serde_json::json!({ "licensed": false, "why": "version", "from": "6.0", "until": "7.0" }));
}

#[test]
fn the_file_is_the_users_own_wherever_the_plugin_runs() {
    let home = std::path::Path::new("/Users/ada");
    assert_eq!(file_in(Some("/somewhere/else.txt"), "macos", Some(home), None), Some("/somewhere/else.txt".into()), "a file named outright (a test's)");
    assert_eq!(file_in(None, "macos", Some(home), None), Some("/Users/ada/Library/Application Support/Sonic Pi/plugin-licence.txt".into()));
    assert_eq!(file_in(None, "windows", None, Some(std::path::Path::new("C:\\Users\\ada\\AppData\\Roaming"))), Some(std::path::Path::new("C:\\Users\\ada\\AppData\\Roaming").join("Sonic Pi").join("plugin-licence.txt")));
    assert_eq!(file_in(None, "linux", Some(home), None), Some("/Users/ada/.config/sonic-pi/plugin-licence.txt".into()));
    assert_eq!(file_in(None, "macos", None, None), None, "no home: nowhere");
}

#[test]
fn whose_a_licence_is_can_be_asked_without_keeping_it() {
    let place = Place::new("whose");
    let keeper = place.keeper("");
    assert_eq!(keeper.whose(&format!("some text\n{}", signed("SP-AAAAA-11111", "Ada Lovelace"))), Some("Ada Lovelace".to_string()));
    assert_eq!(keeper.whose("a shopping list"), None);
    assert_eq!(keeper.whose(&sign(&licence("SP-BBBBB-22222", "Mallory"), &OTHER_KEY).unwrap()), None, "not one this plugin takes");
    assert!(!place.file().exists(), "nothing was kept");
    assert_eq!(keeper.standing(), Standing::Unlicensed(Why::None));
}

