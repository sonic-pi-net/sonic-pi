#ifdef TRACY_ENABLE
#include <tracy/Tracy.hpp>
#define SP_Plot(a, b) TracyPlot(a, b)
#define SP_Lockable(a, b) TracyLockable(a, b)
#define SP_LockableBase(a) LockableBase(a)
#define SP_ZoneScoped ZoneScoped
#define SP_ZoneScopedN(a) ZoneScopedN(a)
#define SP_FrameMark FrameMark
#else
#define SP_Plot(a, b)
#define SP_Lockable(a, b) a b
#define SP_LockableBase(a) a
#define SP_ZoneScoped
#define SP_ZoneScopedN(a)
#define SP_FrameMark
#endif