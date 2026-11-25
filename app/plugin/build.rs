// SPDX-License-Identifier: AGPL-3.0-or-later
// Copyright (c) 2026 Sam Aaron
//! The C and C++ beside the Rust: the VST3 shell, the Audio Unit's, the AppKit (or Win32) glue, and
//! SuperSonic's front, compiled with exactly the flags Clockwork was (its
//! layout constants are compile-time, and a front built with others reads
//! the engine's memory wrong).
use std::path::{Path, PathBuf};

/// Building for Windows with MSVC: cl's flags, not clang's, and Windows' libraries in place of Apple's frameworks.
fn msvc() -> bool {
    std::env::var("CARGO_CFG_TARGET_ENV").as_deref() == Ok("msvc")
}

fn macos() -> bool {
    std::env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("macos")
}

/// Only the entry points are exported (clang's -fvisibility=hidden); MSVC exports nothing it is not told to.
fn hidden(b: &mut cc::Build) -> &mut cc::Build {
    if !msvc() {
        b.flag("-fvisibility=hidden");
    }
    b
}

/// A C standard: cl has no gnu99 (or c99), and takes C11 for what it is asked to compile from C99 on
/// (sp_host.c's _Thread_local is C11's, which clang and gcc allow in gnu99).
fn c_std<'a>(b: &'a mut cc::Build, std: &str) -> &'a mut cc::Build {
    b.std(if msvc() { "c11" } else { std })
}

fn main() {
    for file in ["vst3/shell.cpp", "au/shell.mm", "core/plugin.cpp", "core/plugin.h", "core/mac.mm", "core/win.cpp"] {
        println!("cargo:rerun-if-changed={file}");
    }
    println!("cargo:rerun-if-env-changed=VST3_PLUGINTERFACES_INCLUDE_DIR");

    let manifest = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let vst3 = std::env::var("VST3_PLUGINTERFACES_INCLUDE_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| manifest.join("target/vst3_pluginterfaces"));
    assert!(
        vst3.join("pluginterfaces/base/funknown.h").is_file(),
        "no VST3 pluginterfaces at {}: scripts/build.sh fetches them, or set VST3_PLUGINTERFACES_INCLUDE_DIR",
        vst3.display()
    );

    let mut shell = cc::Build::new();
    hidden(&mut shell)
        .cpp(true)
        .std("c++17")
        .include(&vst3)
        .include("core")
        // the version a host is told is this crate's (Cargo.toml), as the bundle's Info.plist is (scripts/build.sh)
        .define("SONIC_PI_PLUGIN_VERSION", Some(format!("\"{}\"", std::env::var("CARGO_PKG_VERSION").unwrap()).as_str()))
        .file("vst3/shell.cpp")
        .file("core/plugin.cpp")
        .compile("sp_shell");

    if macos() {
        cc::Build::new()
            .cpp(true)
            .std("c++17")
            .flag("-fobjc-arc")
            .flag("-fvisibility=hidden")
            .file("core/mac.mm")
            .compile("sp_mac");
        // the Audio Unit, for the hosts that take no VST3: compiled without ARC (it makes its classes by hand)
        cc::Build::new()
            .cpp(true)
            .std("c++17")
            .flag("-fvisibility=hidden")
            .include("core")
            .file("au/shell.mm")
            .compile("sp_au");
        println!("cargo:rustc-link-lib=framework=AppKit");
        println!("cargo:rustc-link-lib=framework=AudioToolbox");
    } else {
        cc::Build::new().cpp(true).std("c++17").file("core/win.cpp").compile("sp_win");
        println!("cargo:rustc-link-lib=dylib=user32");
    }

    let flags = front();
    runtime(&flags);
}

/// SuperSonic's front (front/), behind its C API: the file verbs, answered by
/// reading and writing the files on the front's own thread. Returns the
/// flags Clockwork was compiled with, for everything else that reads its
/// headers.
fn front() -> Vec<String> {
    let supersonic = std::env::var_os("CLOCKWORK_SYS_SOURCE")
        .map(PathBuf::from)
        .expect("CLOCKWORK_SYS_SOURCE names SuperSonic's tree (.cargo/config.toml)");
    let build = PathBuf::from(std::env::var("DEP_CLOCKWORK_BUILD_DIR").expect("clockwork-sys says where it built"));
    let flags = clockwork_flags(&build);
    let front = supersonic.join("front");
    let sources = ["supersonic_commands.cpp", "supersonic_commands_api.cpp", "Recorder.cpp"];
    for s in sources {
        println!("cargo:rerun-if-changed={}", front.join(s).display());
    }
    let version = std::fs::read_to_string(supersonic.join("CMakeLists.txt"))
        .ok()
        .and_then(|text| {
            let at = text.find("project(SuperSonic")?;
            let rest = &text[at..];
            let v = rest.find("VERSION ")? + "VERSION ".len();
            Some(rest[v..].split_whitespace().next()?.to_owned())
        })
        .unwrap_or_else(|| "unknown".into());

    let mut b = cc::Build::new();
    hidden(&mut b)
        .cpp(true)
        .std("c++20")
        .define("SUPERSONIC_VERSION_STRING", format!("\"{version}\"").as_str())
        .include(&front)
        .include(supersonic.join("dsp/scsynth"))
        .include(supersonic.join("clockwork/src/comms"));
    for flag in &flags {
        b.flag(flag);
    }
    for s in sources {
        b.file(front.join(s));
    }
    b.compile("supersonic_front");
    let mut flags = flags;
    flags.push(format!("-I{}", front.display()));
    flags.push(format!("-I{}", supersonic.join("clockwork/src").display()));
    flags.push(format!("-DSUPERSONIC_VERSION_STRING=\"{version}\""));
    flags
}

/// The language: mruby and the runtime's native bytecode as the web's native
/// build makes them (app/web/scripts/build-runtime.sh --native), the seam the
/// web shares and the native host over it (app/web/runtime/host), and this
/// plugin's instance (core/instance.cpp) composing that with the engine.
fn runtime(clockwork: &[String]) {
    println!("cargo:rerun-if-changed=core/instance.cpp");
    println!("cargo:rerun-if-changed=core/instance.h");
    println!("cargo:rerun-if-changed=core/sound_files.cpp");
    println!("cargo:rerun-if-env-changed=SONIC_PI_WEB_ROOT");
    let manifest = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let web = std::env::var("SONIC_PI_WEB_ROOT")
        .map(PathBuf::from)
        .unwrap_or_else(|_| manifest.join("../web"));
    // SONIC_PI_MRUBY: another build of mruby than the web's usual one (scripts/build.sh --universal gives one that
    // holds both of a Mac's architectures)
    println!("cargo:rerun-if-env-changed=SONIC_PI_MRUBY");
    let mruby = std::env::var("SONIC_PI_MRUBY").map(PathBuf::from).unwrap_or_else(|_| web.join("build/mruby/host"));
    let irep = web.join("build/runtime/runtime_native_irep.c");
    // MSVC's mruby (its visualcpp toolchain) is libmruby.lib
    let libmruby = mruby.join(if msvc() { "lib/libmruby.lib" } else { "lib/libmruby.a" });
    assert!(
        libmruby.is_file() && irep.is_file(),
        "no native runtime under {}: run scripts/build-runtime.sh --native there",
        web.display()
    );
    let host = web.join("runtime/host");
    let seam = host.join("sp_host.c");
    for f in [seam.clone(), irep.clone(), libmruby.clone()] {
        println!("cargo:rerun-if-changed={}", f.display());
    }

    // mruby's own defines decide its object layout: everything that reads its headers takes them
    let flags_mak = std::fs::read_to_string(mruby.join("lib/libmruby.flags.mak")).expect("libmruby.flags.mak");
    let cflags = flags_mak
        .lines()
        .find_map(|l| l.strip_prefix("MRUBY_CFLAGS = "))
        .expect("MRUBY_CFLAGS in libmruby.flags.mak");
    // cl's are /D, which cc passes on as -D as it does its own
    let defines: Vec<String> = cflags
        .split_whitespace()
        .filter_map(|w| w.strip_prefix("-D").or_else(|| w.strip_prefix("/D")))
        .map(|d| format!("-D{d}"))
        .collect();
    let mruby_include = mruby.join("include");

    let mut c = cc::Build::new();
    c_std(hidden(&mut c), "gnu99").include(&mruby_include).warnings(false);
    for d in &defines {
        c.flag(d);
    }
    c.file(&seam).file(&irep).compile("sp_runtime");

    // aubio's onset detector, as Sonic Pi vendors it: its own file readers (io/) left out
    let aubio = web.join("../external/aubio-0.4.9/src");
    let mut a = cc::Build::new();
    c_std(hidden(&mut a), "c99").include(&aubio).warnings(false);
    if msvc() {
        a.define("_CRT_SECURE_NO_WARNINGS", "1");
    }
    for d in ["HAVE_STDLIB_H", "HAVE_STDIO_H", "HAVE_MATH_H", "HAVE_STRING_H", "HAVE_LIMITS_H", "HAVE_STDARG_H", "HAVE_ERRNO_H", "HAVE_C99_VARARGS_MACROS"] {
        a.define(d, "1");
    }
    let mut stack = vec![aubio.clone()];
    while let Some(dir) = stack.pop() {
        for e in std::fs::read_dir(&dir).expect("aubio's sources").flatten() {
            let p = e.path();
            if p.is_dir() {
                if p.file_name().is_some_and(|n| n != "io") {
                    stack.push(p);
                }
            } else if p.extension().is_some_and(|x| x == "c") && p.file_name().is_some_and(|n| n != "windll.c") {
                a.file(p);
            }
        }
    }
    a.compile("aubio_onset");

    let mut x = cc::Build::new();
    hidden(&mut x)
        .cpp(true)
        .std("c++20")
        .include(&host)
        .include(&aubio)
        .include(&mruby_include);
    for f in clockwork.iter().chain(&defines) {
        x.flag(f);
    }
    for f in ["runtime_host.cpp", "files.cpp", "osc_codec.cpp"] {
        println!("cargo:rerun-if-changed={}", host.join(f).display());
        x.file(host.join(f));
    }
    // oscpack, which the front and the codec read and write OSC with
    let supersonic = PathBuf::from(std::env::var("CLOCKWORK_SYS_SOURCE").unwrap());
    let oscpack = supersonic.join("clockwork/src/vendor/oscpack/osc");
    for f in ["OscOutboundPacketStream.cpp", "OscReceivedElements.cpp", "OscTypes.cpp"] {
        x.file(oscpack.join(f));
    }
    x.file("core/sound_files.cpp").file("core/instance.cpp").compile("sp_instance");

    println!("cargo:rustc-link-search=native={}", mruby.join("lib").display());
    if msvc() {
        println!("cargo:rustc-link-lib=static=libmruby");
    } else {
        println!("cargo:rustc-link-lib=static=mruby");
    }
    // aubio's vector maths, on Apple platforms
    if macos() {
        println!("cargo:rustc-link-lib=framework=Accelerate");
    }
}

/// The -D and -I flags Clockwork's own sources were compiled with, read from
/// the build's compile_commands.json (CMAKE_EXPORT_COMPILE_COMMANDS).
fn clockwork_flags(build: &Path) -> Vec<String> {
    let path = build.join("compile_commands.json");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    let commands: serde_json::Value = serde_json::from_str(&text).expect("compile_commands.json");
    let entry = commands
        .as_array()
        .and_then(|all| {
            all.iter().find(|e| {
                e["file"].as_str().is_some_and(|f| f.ends_with("src/embed/clockwork_embed.cpp"))
            })
        })
        .expect("Clockwork's embed source in compile_commands.json");
    let words: Vec<String> = match entry["arguments"].as_array() {
        Some(args) => args.iter().filter_map(|a| a.as_str().map(str::to_owned)).collect(),
        None => split_command(entry["command"].as_str().unwrap_or(""), msvc()),
    };
    let mut flags = Vec::new();
    let mut words = words.into_iter();
    while let Some(w) = words.next() {
        if w.starts_with("-D") || w.starts_with("-I") {
            flags.push(w);
        } else if let Some(rest) = w.strip_prefix("/D").or_else(|| w.strip_prefix("/I")) {
            // cl's own spelling, which CMake uses for some (/DWIN32, /DNDEBUG)
            flags.push(format!("-{}{rest}", &w[1..2]));
        } else if let Some(dir) = w.strip_prefix("-external:I").or_else(|| w.strip_prefix("/external:I")) {
            // cl's -isystem
            flags.push(format!("-I{dir}"));
        } else if w == "-isystem" {
            if let Some(dir) = words.next() {
                flags.push("-isystem".into());
                flags.push(dir);
            }
        }
    }
    flags
}

/// A shell command line as the words a shell would make of it: spaces split,
/// double quotes group, a backslash takes the next character as it is. On
/// Windows (`windows`) a backslash is a path's separator, and only a quote
/// after one is taken as it is.
fn split_command(line: &str, windows: bool) -> Vec<String> {
    let mut words = Vec::new();
    let mut word = String::new();
    let mut started = false;
    let mut quoted = false;
    let mut chars = line.chars();
    while let Some(c) = chars.next() {
        match c {
            '\\' if windows => {
                if chars.clone().next() == Some('"') {
                    word.push('"');
                    chars.next();
                } else {
                    word.push('\\');
                }
                started = true;
            }
            '\\' => {
                if let Some(n) = chars.next() {
                    word.push(n);
                    started = true;
                }
            }
            '"' => {
                quoted = !quoted;
                started = true;
            }
            c if c.is_whitespace() && !quoted => {
                if started {
                    words.push(std::mem::take(&mut word));
                    started = false;
                }
            }
            c => {
                word.push(c);
                started = true;
            }
        }
    }
    if started {
        words.push(word);
    }
    words
}
