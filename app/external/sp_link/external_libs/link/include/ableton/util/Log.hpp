// Copyright: 2014, Ableton AG, Berlin, all rights reserved

#pragma once

#include <ableton/util/Injected.hpp>
#include <iostream>
#include <string>

namespace ableton
{
namespace util
{

// Null object for the Log concept
struct NullLog
{
  template <typename T>
  friend const NullLog& operator<<(const NullLog& log, const T&)
  {
    return log;
  }

  friend const NullLog& debug(const NullLog& log)
  {
    return log;
  }

  friend const NullLog& info(const NullLog& log)
  {
    return log;
  }

  friend const NullLog& warning(const NullLog& log)
  {
    return log;
  }

  friend const NullLog& error(const NullLog& log)
  {
    return log;
  }

  friend NullLog channel(const NullLog&, std::string)
  {
    return {};
  }
};

// std streams-based log
struct StdLog
{
  StdLog(std::string channelName = "")
    : mChannelName(std::move(channelName))
  {
  }

  // Stream type used by std log to prepend the channel name to log messages
  struct StdLogStream
  {
    StdLogStream(std::ostream& ioStream, const std::string& channelName)
      : mpIoStream(&ioStream)
      , mChannelName(channelName)
    {
      ioStream << "[" << mChannelName << "] ";
    }

    StdLogStream(StdLogStream&& rhs)
      : mpIoStream(rhs.mpIoStream)
      , mChannelName(rhs.mChannelName)
    {
      rhs.mpIoStream = nullptr;
    }

    ~StdLogStream()
    {
      if (mpIoStream)
      {
        (*mpIoStream) << "\n";
      }
    }

    template <typename T>
    std::ostream& operator<<(const T& rhs)
    {
      (*mpIoStream) << rhs;
      return *mpIoStream;
    }

    std::ostream* mpIoStream;
    const std::string& mChannelName;
  };

  friend StdLogStream debug(const StdLog& log)
  {
    return {std::clog, log.mChannelName};
  }

  friend StdLogStream info(const StdLog& log)
  {
    return {std::clog, log.mChannelName};
  }

  friend StdLogStream warning(const StdLog& log)
  {
    return {std::clog, log.mChannelName};
  }

  friend StdLogStream error(const StdLog& log)
  {
    return {std::cerr, log.mChannelName};
  }

  friend StdLog channel(const StdLog& log, const std::string& channelName)
  {
    auto compositeName =
      log.mChannelName.empty() ? channelName : log.mChannelName + "::" + channelName;
    return {std::move(compositeName)};
  }

  std::string mChannelName;
};

// Log adapter that adds timestamps
template <typename Log>
struct Timestamped
{
  using InnerLog = typename util::Injected<Log>::type;

  Timestamped() = default;

  Timestamped(util::Injected<Log> log)
    : mLog(std::move(log))
  {
  }

  util::Injected<Log> mLog;

  friend decltype(debug(std::declval<InnerLog>())) debug(const Timestamped& log)
  {
    return log.logTimestamp(debug(*log.mLog));
  }

  friend decltype(info(std::declval<InnerLog>())) info(const Timestamped& log)
  {
    return log.logTimestamp(info(*log.mLog));
  }

  friend decltype(warning(std::declval<InnerLog>())) warning(const Timestamped& log)
  {
    return log.logTimestamp(warning(*log.mLog));
  }

  friend decltype(error(std::declval<InnerLog>())) error(const Timestamped& log)
  {
    return log.logTimestamp(error(*log.mLog));
  }

  friend Timestamped channel(const Timestamped& log, const std::string& channelName)
  {
    return {channel(*log.mLog, channelName)};
  }

  template <typename Stream>
  Stream logTimestamp(Stream&& streamRef) const
  {
    using namespace std::chrono;
    Stream stream = std::forward<Stream>(streamRef);
    stream << "|"
           << duration_cast<milliseconds>(system_clock::now().time_since_epoch()).count()
           << "ms| ";
    return stream;
  }
};

} // namespace util
} // namespace ableton
