/* Copyright 2021, Ableton AG, Berlin. All rights reserved.
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

#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#if defined(LINK_PLATFORM_UNIX)
#include <sys/select.h>
#include <termios.h>
#include <unistd.h>
#elif defined(LINK_PLATFORM_WINDOWS)
#pragma warning(push, 0)
#pragma warning(disable : 4255) // 'no function prototype given' in winuser.h
#pragma warning(disable : 4668) // undefined preprocessor macro in winioctl.h
#pragma warning(disable : 5105) // "/wd5105" # "macro expansion producing 'defined' has
                                // undefined behavior" in winbase.h
#include <windows.h>
#pragma warning(pop)
#pragma warning(disable : 4100) // unreferenced formal parameter in main
#endif

#include <abl_link.h>

typedef struct state
{
  abl_link link;
  abl_link_session_state session_state;
  bool running;
  double quantum;
} state;

struct state *new_state(void)
{
  struct state *s = malloc(sizeof(state));
  s->link = abl_link_create(120);
  s->session_state = abl_link_create_session_state();
  s->running = true;
  s->quantum = 4;
  return s;
}

void delete_state(state *state)
{
  abl_link_destroy_session_state(state->session_state);
  abl_link_destroy(state->link);
  free(state);
}

void disable_buffered_input(void)
{
#if defined(LINK_PLATFORM_UNIX)
  struct termios t;
  tcgetattr(STDIN_FILENO, &t);
  t.c_lflag &= ~ICANON;
  tcsetattr(STDIN_FILENO, TCSANOW, &t);
#endif
}

void enable_buffered_input(void)
{
#if defined(LINK_PLATFORM_UNIX)
  struct termios t;
  tcgetattr(STDIN_FILENO, &t);
  t.c_lflag |= ICANON;
  tcsetattr(STDIN_FILENO, TCSANOW, &t);
#endif
}

bool wait_for_input(void)
{
#if defined(LINK_PLATFORM_UNIX)
  fd_set selectset;
  struct timeval timeout = {0, 50000};
  int ret;
  FD_ZERO(&selectset);
  FD_SET(0, &selectset);
  ret = select(1, &selectset, NULL, NULL, &timeout);
  if (ret > 0)
  {
    return true;
  }
#elif (LINK_PLATFORM_WINDOWS)
  HANDLE handle = GetStdHandle(STD_INPUT_HANDLE);
  if (WaitForSingleObject(handle, 50) == WAIT_OBJECT_0)
  {
    return true;
  }
#else
#error "Missing implementation"
#endif
  return false;
}

void clear_line(void)
{
  printf("   \r");
  fflush(stdout);
}

void clear_input(void)
{
#if defined(LINK_PLATFORM_WINDOWS)
  {
    HANDLE handle = GetStdHandle(STD_INPUT_HANDLE);
    INPUT_RECORD r[512];
    DWORD read;
    ReadConsoleInput(handle, r, 512, &read);
  }
#endif
}

void print_help(void)
{
  printf("\n\n < L I N K  H U T >\n\n");
  printf("usage:\n");
  printf("  enable / disable Link: a\n");
  printf("  start / stop: space\n");
  printf("  decrease / increase tempo: w / e\n");
  printf("  decrease / increase quantum: r / t\n");
  printf("  enable / disable start stop sync: s\n");
  printf("  quit: q\n");
}

void print_state_header(void)
{
  printf(
    "\nenabled | num peers | quantum | start stop sync | tempo   | beats   | metro\n");
}

void print_state(state *state)
{
  abl_link_capture_app_session_state(state->link, state->session_state);
  const uint64_t time = abl_link_clock_micros(state->link);
  const char *enabled = abl_link_is_enabled(state->link) ? "yes" : "no";
  const uint64_t num_peers = abl_link_num_peers(state->link);
  const char *start_stop =
    abl_link_is_start_stop_sync_enabled(state->link) ? "yes" : " no";
  const char *playing =
    abl_link_is_playing(state->session_state) ? "[playing]" : "[stopped]";
  const double tempo = abl_link_tempo(state->session_state);
  const double beats = abl_link_beat_at_time(state->session_state, time, state->quantum);
  const double phase = abl_link_phase_at_time(state->session_state, time, state->quantum);
  printf("%7s | ", enabled);
  printf("%9" PRIu64 " | ", num_peers);
  printf("%7.f | ", state->quantum);
  printf("%3s %11s | ", start_stop, playing);
  printf("%7.2f | ", tempo);
  printf("%7.2f | ", beats);
  for (int i = 0; i < ceil(state->quantum); ++i)
  {
    if (i < phase)
    {
      printf("X");
    }
    else
    {
      printf("O");
    }
  }
}

void input(state *state)
{
  char in;

#if defined(LINK_PLATFORM_UNIX)
  in = (char)fgetc(stdin);
#elif defined(LINK_PLATFORM_WINDOWS)
  HANDLE stdinHandle = GetStdHandle(STD_INPUT_HANDLE);
  DWORD numCharsRead;
  INPUT_RECORD inputRecord;
  do
  {
    ReadConsoleInput(stdinHandle, &inputRecord, 1, &numCharsRead);
  } while ((inputRecord.EventType != KEY_EVENT) || inputRecord.Event.KeyEvent.bKeyDown);
  in = inputRecord.Event.KeyEvent.uChar.AsciiChar;
#else
#error "Missing implementation"
#endif

  abl_link_capture_app_session_state(state->link, state->session_state);
  const double tempo = abl_link_tempo(state->session_state);
  const uint64_t timestamp = abl_link_clock_micros(state->link);
  const bool enabled = abl_link_is_enabled(state->link);
  switch (in)
  {
  case 'q':
    state->running = false;
    clear_line();
    return;
  case 'a':
    abl_link_enable(state->link, !enabled);
    break;
  case 'w':
    abl_link_set_tempo(state->session_state, tempo - 1, timestamp);
    break;
  case 'e':
    abl_link_set_tempo(state->session_state, tempo + 1, timestamp);
    break;
  case 'r':
    state->quantum -= 1;
    break;
  case 't':
    state->quantum += 1;
    break;
  case 's':
    abl_link_enable_start_stop_sync(
      state->link, !abl_link_is_start_stop_sync_enabled(state->link));
    break;
  case ' ':
    if (abl_link_is_playing(state->session_state))
    {
      abl_link_set_is_playing(
        state->session_state, false, abl_link_clock_micros(state->link));
    }
    else
    {
      abl_link_set_is_playing_and_request_beat_at_time(state->session_state, true,
        abl_link_clock_micros(state->link), 0, state->quantum);
    }
    break;
  }
  abl_link_commit_app_session_state(state->link, state->session_state);
}

int main(int nargs, char **args)
{
  state *state = new_state();

  print_help();
  print_state_header();
  disable_buffered_input();
  clear_input();

  while (state->running)
  {
    clear_line();
    if (wait_for_input())
    {
      input(state);
    }
    else
    {
      print_state(state);
    }
  }

  enable_buffered_input();
  delete_state(state);
  return 0;
}
