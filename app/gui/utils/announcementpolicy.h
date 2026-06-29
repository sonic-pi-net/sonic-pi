#ifndef ANNOUNCEMENTPOLICY_H
#define ANNOUNCEMENTPOLICY_H

namespace SonicPi {

// Categories of screen-reader announcement, so a user can choose which ones
// are spoken — mirroring how DAWs (e.g. Logic Pro) let you pick which transport
// shortcuts are voiced. The motivating case: "Run started" was announced the
// instant audio began, so VoiceOver ducked the first beat. Transport speech can
// now be silenced independently, while errors — the most important feedback —
// always speak.
enum class Announcement {
    General,      // unclassified; always speaks
    Transport,    // Run started / Stopped
    Navigation,   // completion-popup suggestions
    Error,        // syntax / runtime errors
};

// Pure decision object: given the user's preferences, should a given category
// be spoken? Held by MainWindow and consulted by announce(). Kept dependency-free
// so it can be unit-tested without a QApplication.
struct AnnouncementPolicy {
    bool speakTransport = true;

    bool shouldSpeak(Announcement a) const {
        switch (a) {
            case Announcement::Transport:  return speakTransport;
            case Announcement::Navigation: return true;
            case Announcement::General:    return true;
            case Announcement::Error:      return true;   // errors always speak
        }
        return true;
    }
};

} // namespace SonicPi

#endif // ANNOUNCEMENTPOLICY_H
