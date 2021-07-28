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

#include <ableton/link/Controller.hpp>
#include <ableton/link/Tempo.hpp>
#include <ableton/platforms/stl/Random.hpp>
#include <ableton/test/CatchWrapper.hpp>
#include <ableton/util/Log.hpp>
#include <ableton/util/test/Timer.hpp>

namespace ableton
{
namespace link
{

using namespace std::chrono;

static bool operator==(const IncomingClientState& lhs, const ClientState& rhs)
{
  return static_cast<bool>(lhs.timeline) && static_cast<bool>(lhs.startStopState)
         && std::tie(*lhs.timeline, *lhs.startStopState)
              == std::tie(rhs.timeline, rhs.startStopState);
}

namespace
{

struct MockClock
{
  template <typename T, typename Rep>
  void advance(std::chrono::duration<T, Rep> duration)
  {
    now() += duration;
  }

  microseconds micros() const
  {
    return now();
  }

private:
  static microseconds& now()
  {
    static microseconds now{1};
    return now;
  }
};

struct MockIoContext
{
  template <std::size_t BufferSize>
  struct Socket
  {
    std::size_t send(
      const uint8_t* const, const size_t numBytes, const asio::ip::udp::endpoint&)
    {
      return numBytes;
    }

    template <typename Handler>
    void receive(Handler)
    {
    }

    asio::ip::udp::endpoint endpoint() const
    {
      return {};
    }
  };

  void stop()
  {
  }

  template <std::size_t BufferSize>
  Socket<BufferSize> openUnicastSocket(const asio::ip::address_v4&)
  {
    return {};
  }

  template <std::size_t BufferSize>
  Socket<BufferSize> openMulticastSocket(const asio::ip::address_v4&)
  {
    return {};
  }

  std::vector<asio::ip::address> scanNetworkInterfaces()
  {
    return {};
  }

  using Timer = util::test::Timer;

  Timer makeTimer()
  {
    return {};
  }

  template <typename Callback, typename Duration>
  struct LockFreeCallbackDispatcher
  {
    LockFreeCallbackDispatcher(Callback callback, Duration)
      : mCallback(std::move(callback))
    {
    }

    void invoke()
    {
      mCallback();
    }

    Callback mCallback;
  };

  using Log = util::NullLog;

  Log log() const
  {
    return {};
  }

  template <typename Handler>
  void async(Handler handler) const
  {
    handler();
  }

  MockIoContext clone() const
  {
    return {};
  }

  template <typename ExceptionHandler>
  MockIoContext clone(ExceptionHandler) const
  {
    return {};
  }
};

using MockController = Controller<PeerCountCallback,
  TempoCallback,
  StartStopStateCallback,
  MockClock,
  platforms::stl::Random,
  MockIoContext>;

const auto kAnyBeatTime = Beats{5.};

const auto kAnyTime = std::chrono::microseconds{6};

struct TempoClientCallback
{
  void operator()(const Tempo bpm)
  {
    tempos.push_back(bpm);
  }

  std::vector<Tempo> tempos;
};

struct StartStopStateClientCallback
{
  void operator()(const bool isPlaying)
  {
    startStopStates.push_back(isPlaying);
  }

  std::vector<bool> startStopStates;
};

template <typename SetClientStateFunctionT, typename GetClientStateFunctionT>
void testSetAndGetClientState(
  SetClientStateFunctionT setClientState, GetClientStateFunctionT getClientState)
{
  using namespace std::chrono;

  auto clock = MockClock{};
  MockController controller(Tempo{100.0}, [](std::size_t) {}, [](Tempo) {}, [](bool) {},
    clock, util::injectVal(MockIoContext{}));

  clock.advance(microseconds{1});
  const auto initialTimeline =
    Optional<Timeline>{Timeline{Tempo{60.}, Beats{0.}, kAnyTime}};
  const auto initialStartStopState =
    Optional<ClientStartStopState>{ClientStartStopState{false, kAnyTime, clock.micros()}};
  const auto initialClientState =
    IncomingClientState{initialTimeline, initialStartStopState, clock.micros()};

  setClientState(controller, initialClientState);

  SECTION("Client state is correct after initial set")
  {
    CHECK(initialClientState == getClientState(controller));
  }

  SECTION("Set outdated start stop state (timestamp unchanged)")
  {
    // Set client state with a StartStopState having the same timestamp as the current
    // StartStopState - don't advance clock
    const auto outdatedStartStopState = Optional<ClientStartStopState>{
      ClientStartStopState{false, kAnyTime, clock.micros()}};
    setClientState(controller,
      IncomingClientState{Optional<Timeline>{}, outdatedStartStopState, clock.micros()});
    CHECK(initialClientState == getClientState(controller));
  }

  clock.advance(microseconds{1});

  SECTION("Set outdated start stop state (timestamp in past)")
  {
    const auto outdatedStartStopState = Optional<ClientStartStopState>{
      ClientStartStopState{false, kAnyTime, microseconds{0}}};
    setClientState(controller,
      IncomingClientState{Optional<Timeline>{}, outdatedStartStopState, clock.micros()});
    CHECK(initialClientState == getClientState(controller));
  }

  SECTION("Set empty client state")
  {
    setClientState(controller, IncomingClientState{Optional<Timeline>{},
                                 Optional<ClientStartStopState>{}, clock.micros()});
    CHECK(initialClientState == getClientState(controller));
  }

  SECTION("Set client state with new Timeline and StartStopState")
  {
    const auto expectedTimeline =
      Optional<Timeline>{Timeline{Tempo{80.}, Beats{1.}, kAnyTime}};
    const auto expectedStartStopState = Optional<ClientStartStopState>{
      ClientStartStopState{false, kAnyTime, clock.micros()}};
    const auto expectedClientState =
      IncomingClientState{expectedTimeline, expectedStartStopState, clock.micros()};
    setClientState(controller, expectedClientState);
    CHECK(expectedClientState == getClientState(controller));
  }
}


template <typename SetClientStateFunctionT>
void testCallbackInvocation(SetClientStateFunctionT setClientState)
{
  using namespace std::chrono;

  auto clock = MockClock{};
  auto tempoCallback = TempoClientCallback{};
  auto startStopStateCallback = StartStopStateClientCallback{};
  MockController controller(Tempo{100.0}, [](std::size_t) {}, std::ref(tempoCallback),
    std::ref(startStopStateCallback), clock, util::injectVal(MockIoContext{}));

  clock.advance(microseconds{1});

  const auto initialTempo = Tempo{50.};
  const auto initialIsPlaying = true;

  const auto initialTimeline =
    Optional<Timeline>{Timeline{initialTempo, Beats{0.}, kAnyTime}};
  const auto initialStartStopState = Optional<ClientStartStopState>{
    ClientStartStopState{initialIsPlaying, kAnyTime, clock.micros()}};
  setClientState(controller, {initialTimeline, initialStartStopState, clock.micros()});

  SECTION("Callbacks are called when setting new client state")
  {
    CHECK(std::vector<Tempo>{initialTempo} == tempoCallback.tempos);
    CHECK(std::vector<bool>{initialIsPlaying} == startStopStateCallback.startStopStates);

    clock.advance(microseconds{1});
    tempoCallback.tempos = {};
    startStopStateCallback.startStopStates = {};

    SECTION("Callbacks mustn't be called if Tempo and isPlaying don't change")
    {
      const auto timeline =
        Optional<Timeline>{Timeline{initialTempo, Beats{1.}, kAnyTime}};
      const auto startStopState = Optional<ClientStartStopState>{
        ClientStartStopState{initialIsPlaying, kAnyTime, clock.micros()}};
      setClientState(controller, {timeline, startStopState, clock.micros()});
      CHECK(tempoCallback.tempos.empty());
      CHECK(startStopStateCallback.startStopStates.empty());
    }
  }
}

} // namespace


TEST_CASE("Controller | ConstructOptimistically", "[Controller]")
{
  MockController controller(Tempo{100.0}, [](std::size_t) {}, [](Tempo) {}, [](bool) {},
    MockClock{}, util::injectVal(MockIoContext{}));

  CHECK(!controller.isEnabled());
  CHECK(!controller.isStartStopSyncEnabled());
  CHECK(0 == controller.numPeers());
  const auto tl = controller.clientState().timeline;
  CHECK(Tempo{100.0} == tl.tempo);
}

TEST_CASE("Controller | ConstructWithInvalidTempo", "[Controller]")
{
  MockController controllerLowTempo(Tempo{1.0}, [](std::size_t) {}, [](Tempo) {},
    [](bool) {}, MockClock{}, util::injectVal(MockIoContext{}));
  const auto tlLow = controllerLowTempo.clientState().timeline;
  CHECK(Tempo{20.0} == tlLow.tempo);

  MockController controllerHighTempo(Tempo{100000.0}, [](std::size_t) {}, [](Tempo) {},
    [](bool) {}, MockClock{}, util::injectVal(MockIoContext{}));
  const auto tlHigh = controllerHighTempo.clientState().timeline;
  CHECK(Tempo{999.0} == tlHigh.tempo);
}

TEST_CASE("Controller | EnableDisable", "[Controller]")
{
  MockController controller(Tempo{100.0}, [](std::size_t) {}, [](Tempo) {}, [](bool) {},
    MockClock{}, util::injectVal(MockIoContext{}));

  controller.enable(true);
  CHECK(controller.isEnabled());
  controller.enable(false);
  CHECK(!controller.isEnabled());
}

TEST_CASE("Controller | EnableDisableStartStopSync", "[Controller]")
{
  MockController controller(Tempo{100.0}, [](std::size_t) {}, [](Tempo) {}, [](bool) {},
    MockClock{}, util::injectVal(MockIoContext{}));

  controller.enableStartStopSync(true);
  CHECK(controller.isStartStopSyncEnabled());
  controller.enableStartStopSync(false);
  CHECK(!controller.isStartStopSyncEnabled());
}

TEST_CASE("Controller | SetAndGetClientStateThreadSafe", "[Controller]")
{
  testSetAndGetClientState(
    [](MockController& controller, IncomingClientState clientState) {
      controller.setClientState(clientState);
    },
    [](MockController& controller) { return controller.clientState(); });
}

TEST_CASE("Controller | SetAndGetClientStateRealtimeSafe", "[Controller]")
{
  testSetAndGetClientState(
    [](MockController& controller, IncomingClientState clientState) {
      controller.setClientStateRtSafe(clientState);
    },
    [](MockController& controller) { return controller.clientStateRtSafe(); });
}

TEST_CASE("Controller | SetClientStateRealtimeSafeAndGetItThreadSafe", "[Controller]")
{
  testSetAndGetClientState(
    [](MockController& controller, IncomingClientState clientState) {
      controller.setClientStateRtSafe(clientState);
    },
    [](MockController& controller) { return controller.clientState(); });
}

TEST_CASE("Controller | SetClientStateThreadSafeAndGetItRealtimeSafe", "[Controller]")
{
  testSetAndGetClientState(
    [](MockController& controller, IncomingClientState clientState) {
      controller.setClientState(clientState);
    },
    [](MockController& controller) {
      MockClock{}.advance(seconds{2});
      return controller.clientStateRtSafe();
    });
}

TEST_CASE("Controller | CallbacksCalledBySettingClientStateThreadSafe", "[Controller]")
{
  testCallbackInvocation([](MockController& controller, IncomingClientState clientState) {
    controller.setClientState(clientState);
  });
}

TEST_CASE("Controller | CallbacksCalledBySettingClientStateRealtimeSafe", "[Controller]")
{
  testCallbackInvocation([](MockController& controller, IncomingClientState clientState) {
    controller.setClientStateRtSafe(clientState);
  });
}

TEST_CASE("Controller | GetClientStateRtSafeGracePeriod", "[Controller]")
{
  using namespace std::chrono;

  auto clock = MockClock{};
  auto tempoCallback = TempoClientCallback{};
  auto startStopStateCallback = StartStopStateClientCallback{};
  MockController controller(Tempo{100.0}, [](std::size_t) {}, std::ref(tempoCallback),
    std::ref(startStopStateCallback), clock, util::injectVal(MockIoContext{}));
  controller.enable(true);

  clock.advance(microseconds{1});
  const auto initialTimeline =
    Optional<Timeline>{Timeline{Tempo{50.}, Beats{0.}, clock.micros()}};
  const auto initialStartStopState =
    Optional<ClientStartStopState>{ClientStartStopState{true, kAnyTime, clock.micros()}};
  const auto initialState =
    IncomingClientState{initialTimeline, initialStartStopState, clock.micros()};

  controller.setClientStateRtSafe(
    {initialTimeline, initialStartStopState, clock.micros()});
  REQUIRE(initialState == controller.clientState());
  REQUIRE(initialState == controller.clientStateRtSafe());

  clock.advance(microseconds{1});
  const auto newTimeline =
    Optional<Timeline>{Timeline{Tempo{70.}, Beats{1.}, clock.micros()}};
  const auto newStartStopState =
    Optional<ClientStartStopState>{ClientStartStopState{false, kAnyTime, clock.micros()}};
  const auto newState =
    IncomingClientState{newTimeline, newStartStopState, clock.micros()};

  controller.setClientState({newTimeline, newStartStopState, clock.micros()});
  clock.advance(milliseconds{500});
  CHECK(newState == controller.clientState());
  CHECK(initialState == controller.clientStateRtSafe());

  clock.advance(milliseconds{500});
  CHECK(newState == controller.clientState());
  CHECK(newState == controller.clientStateRtSafe());
}

} // namespace link
} // namespace ableton
