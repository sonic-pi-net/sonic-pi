/* Copyright 2016, Ableton AG, Berlin. All rights reserved.
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  If you would like to incorporate Link into a proprietary software application,
 *  please contact <link-devs@ableton.com>.
 */

#include "AudioPlatform.hpp"

#include <algorithm>
#include <atomic>
#include <chrono>
#include <iomanip>
#include <iostream>
#include <thread>
#if defined(LINK_PLATFORM_UNIX)
#include <termios.h>
#endif

namespace
{

struct State
{
  std::atomic<bool> running;
  ableton::Link link;
  ableton::linkaudio::AudioPlatform audioPlatform;

  State()
    : running(true)
    , link(120.)
    , audioPlatform(link)
  {
  }
};

void disableBufferedInput()
{
#if defined(LINK_PLATFORM_UNIX)
  termios t;
  tcgetattr(STDIN_FILENO, &t);
  t.c_lflag &= static_cast<unsigned long>(~ICANON);
  tcsetattr(STDIN_FILENO, TCSANOW, &t);
#endif
}

void enableBufferedInput()
{
#if defined(LINK_PLATFORM_UNIX)
  termios t;
  tcgetattr(STDIN_FILENO, &t);
  t.c_lflag |= ICANON;
  tcsetattr(STDIN_FILENO, TCSANOW, &t);
#endif
}

void clearLine()
{
  std::cout << "   \r" << std::flush;
  std::cout.fill(' ');
}

void printHelp()
{
  std::cout << std::endl << " < L I N K  H U T >" << std::endl << std::endl;
  std::cout << "usage:" << std::endl;
  std::cout << "  enable / disable Link: a" << std::endl;
  std::cout << "  start / stop: space" << std::endl;
  std::cout << "  decrease / increase tempo: w / e" << std::endl;
  std::cout << "  decrease / increase quantum: r / t" << std::endl;
  std::cout << "  enable / disable start stop sync: s" << std::endl;
  std::cout << "  quit: q" << std::endl << std::endl;
}

void printStateHeader()
{
  std::cout
    << "enabled | num peers | quantum | start stop sync | tempo   | beats   | metro"
    << std::endl;
}

void printState(const std::chrono::microseconds time,
  const ableton::Link::SessionState sessionState,
  const bool linkEnabled,
  const std::size_t numPeers,
  const double quantum,
  const bool startStopSyncOn)
{
  using namespace std;
  const auto enabled = linkEnabled ? "yes" : "no";
  const auto beats = sessionState.beatAtTime(time, quantum);
  const auto phase = sessionState.phaseAtTime(time, quantum);
  const auto startStop = startStopSyncOn ? "yes" : "no";
  const auto isPlaying = sessionState.isPlaying() ? "[playing]" : "[stopped]";
  cout << defaultfloat << left << setw(7) << enabled << " | " << setw(9) << numPeers
       << " | " << setw(7) << quantum << " | " << setw(3) << startStop << " " << setw(11)
       << isPlaying << " | " << fixed << setw(7) << sessionState.tempo() << " | " << fixed
       << setprecision(2) << setw(7) << beats << " | ";
  for (int i = 0; i < ceil(quantum); ++i)
  {
    if (i < phase)
    {
      std::cout << 'X';
    }
    else
    {
      std::cout << 'O';
    }
  }
  clearLine();
}

void input(State& state)
{
  char in;

#if defined(LINK_PLATFORM_WINDOWS)
  HANDLE stdinHandle = GetStdHandle(STD_INPUT_HANDLE);
  DWORD numCharsRead;
  INPUT_RECORD inputRecord;
  do
  {
    ReadConsoleInput(stdinHandle, &inputRecord, 1, &numCharsRead);
  } while ((inputRecord.EventType != KEY_EVENT) || inputRecord.Event.KeyEvent.bKeyDown);
  in = inputRecord.Event.KeyEvent.uChar.AsciiChar;
#elif defined(LINK_PLATFORM_UNIX)
  in = static_cast<char>(std::cin.get());
#endif

  const auto tempo = state.link.captureAppSessionState().tempo();
  auto& engine = state.audioPlatform.mEngine;

  switch (in)
  {
  case 'q':
    state.running = false;
    clearLine();
    return;
  case 'a':
    state.link.enable(!state.link.isEnabled());
    break;
  case 'w':
    engine.setTempo(tempo - 1);
    break;
  case 'e':
    engine.setTempo(tempo + 1);
    break;
  case 'r':
    engine.setQuantum(engine.quantum() - 1);
    break;
  case 't':
    engine.setQuantum(std::max(1., engine.quantum() + 1));
    break;
  case 's':
    engine.setStartStopSyncEnabled(!engine.isStartStopSyncEnabled());
    break;
  case ' ':
    if (engine.isPlaying())
    {
      engine.stopPlaying();
    }
    else
    {
      engine.startPlaying();
    }
    break;
  }

  input(state);
}

} // namespace

int main(int, char**)
{
  State state;
  printHelp();
  printStateHeader();
  std::thread thread(input, std::ref(state));
  disableBufferedInput();

  while (state.running)
  {
    const auto time = state.link.clock().micros();
    auto sessionState = state.link.captureAppSessionState();
    printState(time, sessionState, state.link.isEnabled(), state.link.numPeers(),
      state.audioPlatform.mEngine.quantum(),
      state.audioPlatform.mEngine.isStartStopSyncEnabled());
    std::this_thread::sleep_for(std::chrono::milliseconds(10));
  }

  enableBufferedInput();
  thread.join();
  return 0;
}
