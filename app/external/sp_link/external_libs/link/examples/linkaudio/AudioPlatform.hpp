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

#pragma once

#if defined(LINKHUT_AUDIO_PLATFORM_ASIO)
#include "AudioPlatform_Asio.hpp"
#endif

#if defined(LINKHUT_AUDIO_PLATFORM_COREAUDIO)
#include "AudioPlatform_CoreAudio.hpp"
#endif

#if defined(LINKHUT_AUDIO_PLATFORM_DUMMY)
#include "AudioPlatform_Dummy.hpp"
#endif

#if defined(LINKHUT_AUDIO_PLATFORM_JACK)
#include "AudioPlatform_Jack.hpp"
#endif

#if defined(LINKHUT_AUDIO_PLATFORM_PORTAUDIO)
#include "AudioPlatform_Portaudio.hpp"
#endif

#if defined(LINKHUT_AUDIO_PLATFORM_WASAPI)
#include "AudioPlatform_Wasapi.hpp"
#endif
