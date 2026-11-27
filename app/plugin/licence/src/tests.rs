// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
use super::*;

const KEY_ONE: [u8; 32] = [7; 32];
const KEY_TWO: [u8; 32] = [9; 32];

fn licence(name: &str) -> Licence {
    Licence { id: "SP-ABCDE-12345".into(), name: name.into(), email: "buyer@example.com".into(), issued: "2026-11-26".into(),
              product: PRODUCT.into(), from: (5, 0), until: (6, 0), key: 1 }
}
fn keys() -> Vec<PublicKey> { vec![public_key_of(&KEY_ONE, 1), public_key_of(&KEY_TWO, 2)] }

#[test]
fn a_signed_licence_reads_back_as_it_was_issued() {
    let l = licence("Jane Smith");
    assert_eq!(read(&sign(&l, &KEY_ONE).unwrap(), &keys()), Ok(l));
}

#[test]
fn a_licence_survives_what_email_does_to_text() {
    let l = licence("Jane Smith");
    let text = sign(&l, &KEY_ONE).unwrap();
    assert!(text.starts_with("-----BEGIN SONIC PI LICENCE-----\n"), "{text}");
    assert!(text.lines().all(|line| line.len() <= 64), "lines of at most 64 characters: {text}");
    let windows = text.replace('\n', "\r\n");
    let quoted: String = text.lines().map(|line| format!("> {line}  \n")).collect();
    let rewrapped: String = text.replace('\n', "").chars().collect::<Vec<_>>().chunks(31).map(|c| c.iter().collect::<String>() + "\n\n").collect();
    let surrounded = format!("Hi Jane,\n\nHere is your licence:\n\n{text}\n\nThanks!\n");
    let bare = text.lines().filter(|line| !line.starts_with("-----")).collect::<String>();
    for (how, changed) in [("CRLF", windows), ("quoted", quoted), ("rewrapped", rewrapped), ("in a message", surrounded), ("without its first and last lines", bare)] {
        assert_eq!(read(&changed, &keys()), Ok(l.clone()), "{how}");
    }
}

#[test]
fn names_in_any_script_are_kept() {
    for name in ["Zoë Müller", "山田 太郎", "Seán Ó Briain", "Nguyễn Văn A"] {
        let l = licence(name);
        assert_eq!(read(&sign(&l, &KEY_ONE).unwrap(), &keys()).map(|l| l.name), Ok(name.to_string()));
    }
}

/// The text with its first field's bytes changed: the name in the signed part.
fn with_the_name_changed(text: &str) -> String {
    use base64::Engine;
    let token: String = text.lines().filter(|line| !line.starts_with("-----")).collect();
    let mut parts = token.split('.');
    let (tag, payload, signature) = (parts.next().unwrap(), parts.next().unwrap(), parts.next().unwrap());
    let engine = base64::engine::general_purpose::STANDARD_NO_PAD;
    let changed = String::from_utf8(engine.decode(payload).unwrap()).unwrap().replace("Jane Smith", "John Smith");
    assert!(changed.contains("John Smith"));
    format!("{tag}.{}.{signature}", engine.encode(changed))
}

#[test]
fn a_licence_whose_name_was_changed_is_refused() {
    let text = sign(&licence("Jane Smith"), &KEY_ONE).unwrap();
    assert_eq!(read(&with_the_name_changed(&text), &keys()), Err(ReadError::BadSignature));
}

#[test]
fn a_licence_signed_with_a_key_the_plugin_does_not_hold_is_refused() {
    let stranger: [u8; 32] = [42; 32];
    // signed by a stranger but claiming key 1
    assert_eq!(read(&sign(&licence("Jane Smith"), &stranger).unwrap(), &keys()), Err(ReadError::BadSignature));
    // claiming a key number the plugin has never heard of
    let mut l = licence("Jane Smith"); l.key = 3;
    assert_eq!(read(&sign(&l, &stranger).unwrap(), &keys()), Err(ReadError::UnknownKey(3)));
}

#[test]
fn the_spare_key_signs_licences_the_plugin_accepts() {
    let mut l = licence("Jane Smith"); l.key = 2;
    assert_eq!(read(&sign(&l, &KEY_TWO).unwrap(), &keys()), Ok(l.clone()));
    // and one key's licence is not accepted under the other's number
    assert_eq!(read(&sign(&l, &KEY_ONE).unwrap(), &keys()), Err(ReadError::BadSignature));
}

#[test]
fn text_that_is_not_a_whole_licence_is_refused_without_a_panic() {
    let text = sign(&licence("Jane Smith"), &KEY_ONE).unwrap();
    let token: String = text.lines().filter(|line| !line.starts_with("-----")).collect();
    assert_eq!(read("", &keys()), Err(ReadError::NotALicence));
    assert_eq!(read("hello, this is my licence", &keys()), Err(ReadError::NotALicence));
    for cut in [5, 20, token.len() / 2, token.len() - 3] {
        assert!(matches!(read(&token[..cut], &keys()), Err(ReadError::Damaged | ReadError::BadSignature)), "cut at {cut}");
    }
    assert_eq!(read("SPL1.!!!.???", &keys()), Err(ReadError::Damaged));
    assert_eq!(read(&token.replacen("SPL1.", "SPL9.", 1), &keys()), Err(ReadError::NotALicence));
}

#[test]
fn a_licence_covers_its_product_and_its_versions_only() {
    let l = licence("Jane Smith");   // v5: from 5.0, up to but not 6.0
    for version in [(5, 0), (5, 1), (5, 99)] { assert_eq!(l.covers(PRODUCT, version, &[]), Ok(()), "{version:?}"); }
    for version in [(4, 9), (6, 0), (6, 1), (7, 0)] {
        assert_eq!(l.covers(PRODUCT, version, &[]), Err(Refusal::Version { from: (5, 0), until: (6, 0) }), "{version:?}");
    }
    assert_eq!(l.covers("another-product", (5, 1), &[]), Err(Refusal::WrongProduct));
}

#[test]
fn a_revoked_licence_is_refused_by_its_mark_and_the_list_names_no_licence() {
    let l = licence("Jane Smith");
    let mark = revocation_mark("SP-ABCDE-12345");
    // what a list holds: 64 hex characters with nothing of the number in them
    assert!(mark.len() == 64 && mark.chars().all(|c| c.is_ascii_hexdigit() && !c.is_ascii_uppercase()), "{mark}");
    assert!(!mark.to_uppercase().contains("ABCDE") && !mark.contains("12345"), "{mark}");
    // however the number is typed, its mark is the same; another number's is another
    assert_eq!(revocation_mark("  sp-abcde-12345 "), mark);
    let other = revocation_mark("SP-ZZZZZ-00000");
    assert_ne!(other, mark);

    assert_eq!(l.covers(PRODUCT, (5, 1), &[&other, &mark]), Err(Refusal::Revoked));
    assert_eq!(l.covers(PRODUCT, (5, 1), &[&other]), Ok(()));
    // a list as it sits in a file: space around a mark and its case do not hide it
    assert_eq!(l.covers(PRODUCT, (5, 1), &[&format!("  {}  ", mark.to_uppercase())]), Err(Refusal::Revoked));
    // the list is of marks only: a licence's number written in it revokes nothing
    assert_eq!(l.covers(PRODUCT, (5, 1), &["SP-ABCDE-12345"]), Ok(()));
}

#[test]
fn a_licence_is_not_issued_without_a_name_an_address_and_a_range() {
    let mut l = licence("  "); assert_eq!(sign(&l, &KEY_ONE), Err(IssueError::Name));
    l = licence("Jane\nSmith"); assert_eq!(sign(&l, &KEY_ONE), Err(IssueError::Name));
    l = licence(&"x".repeat(201)); assert_eq!(sign(&l, &KEY_ONE), Err(IssueError::Name));
    l = licence("Jane Smith"); l.email = "not an address".into(); assert_eq!(sign(&l, &KEY_ONE), Err(IssueError::Email));
    l = licence("Jane Smith"); l.until = (5, 0); assert_eq!(sign(&l, &KEY_ONE), Err(IssueError::Range));
    // a name is kept as typed apart from the space around it
    l = licence("  Jane Smith "); assert_eq!(read(&sign(&l, &KEY_ONE).unwrap(), &keys()).map(|l| l.name), Ok("Jane Smith".to_string()));
}

#[test]
fn licence_numbers_are_short_distinct_and_easy_to_read_out() {
    let ids: Vec<String> = (0..200).map(|_| new_id()).collect();
    for id in &ids {
        assert_eq!(id.len(), 14, "{id}");
        let b = id.as_bytes();
        assert!(id.starts_with("SP-") && b[8] == b'-', "{id}");
        // no I, L, O or U: letters mistaken for digits or for each other
        assert!(id[3..].chars().all(|c| c == '-' || "0123456789ABCDEFGHJKMNPQRSTVWXYZ".contains(c)), "{id}");
    }
    let mut unique = ids.clone(); unique.sort(); unique.dedup();
    assert_eq!(unique.len(), ids.len());
}

#[test]
fn a_release_version_gives_the_numbers_a_licence_is_checked_against() {
    assert_eq!(version_of("5.1.0-beta1"), Some((5, 1)));
    assert_eq!(version_of("5.0.0"), Some((5, 0)));
    assert_eq!(version_of("v6.2"), Some((6, 2)));
    assert_eq!(version_of("5"), Some((5, 0)));
    assert_eq!(version_of("beta"), None);
    assert_eq!(version_of(""), None);
}
