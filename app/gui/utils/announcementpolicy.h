#ifndef ANNOUNCEMENTPOLICY_H
#define ANNOUNCEMENTPOLICY_H

namespace SonicPi {

// Categories of screen-reader announcement, so a user can silence some (e.g.
// transport speech, which otherwise ducks the start of the audio) while always
// hearing errors.
enum class Announcement {
    General,      // unclassified; always speaks
    Transport,    // Run started / Stopped
    Navigation,   // completion-popup suggestions
    Error,        // syntax / runtime errors
};

// Given the user's preferences, should a category be spoken? Consulted by
// MainWindow::announce(); dependency-free so it's unit-testable without a QApplication.
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
