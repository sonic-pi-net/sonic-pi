// The MIT License (MIT)
//
// Copyright (c) Itay Grudev 2015 - 2018
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

#ifndef SINGLE_APPLICATION_H
#define SINGLE_APPLICATION_H

#include <QtCore/QtGlobal>
#include <QtNetwork/QLocalSocket>

#ifndef QAPPLICATION_CLASS
  #define QAPPLICATION_CLASS QCoreApplication
#endif

#include QT_STRINGIFY(QAPPLICATION_CLASS)

class SingleApplicationPrivate;

/**
 * @brief Handles multiple instances of the same
 * Application
 * @see QCoreApplication
 */
class SingleApplication : public QAPPLICATION_CLASS
{
    Q_OBJECT

    using app_t = QAPPLICATION_CLASS;

public:
    /**
     * @brief Mode of operation of `SingleApplication`.
     * Whether the block should be user-wide or system-wide and whether the
     * primary instance should be notified when a secondary instance had been
     * started.
     * @note Operating system can restrict the shared memory blocks to the same
     * user, in which case the User/System modes will have no effect and the
     * block will be user wide.
     */
    enum Mode {
        /** The `SingleApplication` block should apply user wide
         * (this adds user specific data to the key used for the shared memory and server name)
         * */
        User = 1 << 0,
        /**
         * The `SingleApplication` block applies system-wide.
         */
        System = 1 << 1,
        /**
         * Whether to trigger `instanceStarted()` even whenever secondary instances are started
         */
        SecondaryNotification = 1 << 2,
        /**
         * Excludes the application version from the server name (and memory block) hash
         */
        ExcludeAppVersion = 1 << 3,
        /**
         * Excludes the application path from the server name (and memory block) hash
         */
        ExcludeAppPath = 1 << 4
    };
    Q_DECLARE_FLAGS(Options, Mode)

    /**
     * @brief Intitializes a `SingleApplication` instance with argc command line
     * arguments in argv
     * @arg argc - Number of arguments in argv
     * @arg argv - Supplied command line arguments
     * @arg allowSecondary - Whether to start the instance as secondary
     * if there is already a primary instance.
     * @arg mode - Whether for the `SingleApplication` block to be applied
     * User wide or System wide.
     * @arg timeout - Timeout to wait in milliseconds.
     * @note argc and argv may be changed as Qt removes arguments that it
     * recognizes
     * @note `Mode::SecondaryNotification` only works if set on both the primary
     * instance and the secondary instance.
     * @note The timeout is just a hint for the maximum time of blocking
     * operations. It does not guarantee that the `SingleApplication`
     * initialisation will be completed in given time, though is a good hint.
     * Usually 4*timeout would be the worst case (fail) scenario.
     * @see See the corresponding `QAPPLICATION_CLASS` constructor for reference
     */
    explicit SingleApplication( int &argc, char *argv[], bool allowSecondary = false, Options options = Mode::User, int timeout = 1000, const QString &userData = {} );
    ~SingleApplication() override;

    /**
     * @brief Checks if the instance is primary instance
     * @returns `true` if the instance is primary
     */
    bool isPrimary() const;

    /**
     * @brief Checks if the instance is a secondary instance
     * @returns `true` if the instance is secondary
     */
    bool isSecondary() const;

    /**
     * @brief Returns a unique identifier for the current instance
     * @returns instance id
     */
    quint32 instanceId() const;

    /**
     * @brief Returns the process ID (PID) of the primary instance
     * @returns pid
     */
    qint64 primaryPid() const;

    /**
     * @brief Returns the username of the user running the primary instance
     * @returns user name
     */
    QString primaryUser() const;

    /**
     * @brief Returns the username of the current user
     * @returns user name
     */
    QString currentUser() const;

    /**
     * @brief Mode of operation of sendMessage.
     */
    enum SendMode {
        NonBlocking,  /** Do not wait for the primary instance termination and return immediately */
        BlockUntilPrimaryExit,  /** Wait until the primary instance is terminated */
    };

    /**
     * @brief Sends a message to the primary instance
     * @param message data to send
     * @param timeout timeout for connecting
     * @param sendMode - Mode of operation
     * @returns `true` on success
     * @note sendMessage() will return false if invoked from the primary instance
     */
    bool sendMessage( const QByteArray &message, int timeout = 100, SendMode sendMode = NonBlocking );

    /**
     * @brief Get the set user data.
     * @returns user data
     */
    QStringList userData() const;

Q_SIGNALS:
    /**
     * @brief Triggered whenever a new instance had been started,
     * except for secondary instances if the `Mode::SecondaryNotification` flag is not specified
     */
    void instanceStarted();

    /**
     * @brief Triggered whenever there is a message received from a secondary instance
     */
    void receivedMessage( quint32 instanceId, QByteArray message );

private:
    SingleApplicationPrivate *d_ptr;
    Q_DECLARE_PRIVATE(SingleApplication)
    void abortSafely();
};

Q_DECLARE_OPERATORS_FOR_FLAGS(SingleApplication::Options)

#endif // SINGLE_APPLICATION_H
