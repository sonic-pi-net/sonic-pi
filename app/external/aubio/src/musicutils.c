/*
  Copyright (C) 2018 Paul Brossier <piem@aubio.org>

  This file is part of aubio.

  aubio is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  aubio is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with aubio.  If not, see <http://www.gnu.org/licenses/>.

*/

#include "aubio_priv.h"
#include "musicutils.h"

smpl_t
aubio_hztomel (smpl_t freq)
{
  const smpl_t lin_space = 3./200.;
  const smpl_t split_hz = 1000.;
  const smpl_t split_mel = split_hz * lin_space;
  const smpl_t log_space = 27./LOG(6400/1000.);
  if (freq < 0) {
    AUBIO_WRN("hztomel: input frequency should be >= 0\n");
    return 0;
  }
  if (freq < split_hz)
  {
    return freq * lin_space;
  } else {
    return split_mel + log_space * LOG (freq / split_hz);
  }

}

smpl_t
aubio_meltohz (smpl_t mel)
{
  const smpl_t lin_space = 200./3.;
  const smpl_t split_hz = 1000.;
  const smpl_t split_mel = split_hz / lin_space;
  const smpl_t logSpacing = POW(6400/1000., 1/27.);
  if (mel < 0) {
    AUBIO_WRN("meltohz: input mel should be >= 0\n");
    return 0;
  }
  if (mel < split_mel) {
    return lin_space * mel;
  } else {
    return split_hz * POW(logSpacing, mel - split_mel);
  }
}

smpl_t
aubio_hztomel_htk (smpl_t freq)
{
  const smpl_t split_hz = 700.;
  const smpl_t log_space = 1127.;
  if (freq < 0) {
    AUBIO_WRN("hztomel_htk: input frequency should be >= 0\n");
    return 0;
  }
  return log_space * LOG (1 + freq / split_hz);
}

smpl_t
aubio_meltohz_htk (smpl_t mel)
{
  const smpl_t split_hz = 700.;
  const smpl_t log_space = 1./1127.;
  if (mel < 0) {
    AUBIO_WRN("meltohz_htk: input frequency should be >= 0\n");
    return 0;
  }
  return split_hz * ( EXP ( mel * log_space) - 1.);
}

