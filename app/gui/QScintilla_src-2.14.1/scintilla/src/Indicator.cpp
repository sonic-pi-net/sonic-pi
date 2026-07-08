// Scintilla source code edit control
/** @file Indicator.cxx
 ** Defines the style of indicators which are text decorations such as underlining.
 **/
// Copyright 1998-2001 by Neil Hodgson <neilh@scintilla.org>
// The License.txt file describes the conditions under which this software may be distributed.

#include <cmath>

#include <stdexcept>
#include <vector>
#include <map>
#include <algorithm>
#include <memory>

#include "Platform.h"

#include "Scintilla.h"
#include "StringCopy.h"
#include "IntegerRectangle.h"
#include "Indicator.h"
#include "XPM.h"

using namespace Scintilla;

static PRectangle PixelGridAlign(const PRectangle &rc) {
	// Move left and right side to nearest pixel to avoid blurry visuals
	return PRectangle(round(rc.left), floor(rc.top),
		round(rc.right), floor(rc.bottom));
}

void Indicator::Draw(Surface *surface, const PRectangle &rc, const PRectangle &rcLine, const PRectangle &rcCharacter, DrawState drawState, int value) const {
	StyleAndColour sacDraw = sacNormal;
	if (Flags() & SC_INDICFLAG_VALUEFORE) {
		sacDraw.fore = ColourDesired(value & SC_INDICVALUEMASK);
	}
	if (drawState == drawHover) {
		sacDraw = sacHover;
	}
	const IntegerRectangle irc(rc);
	surface->PenColour(sacDraw.fore);
	const int ymid = (irc.bottom + irc.top) / 2;
	if (sacDraw.style == INDIC_SQUIGGLE) {
		// Sonic Pi patch: Scintilla's stock squiggle is a cramped 2px pixel wave
		// hugging the baseline. Draw instead the same smooth zig-zag the GUI's
		// error card paints (widgets/sonicpierrorcodeline.h) so the editor and
		// the card match: thicker stroke, taller amplitude, longer wavelength,
		// antialiased via an RGBA image, and sat a little below the baseline.
		// The editor grants room with SCI_SETEXTRADESCENT; the drawing clamps
		// to the line box regardless. Tune the look with the constants below
		// (keep them in step with sonicpierrorcodeline.h).
		const IntegerRectangle ircSquiggle(PixelGridAlign(rc));
		const int width = std::min(4000, ircSquiggle.right - ircSquiggle.left);
		if (width > 0) {
			const float amp = 3.0f;      // peak height above/below the centre line
			const float half = 5.0f;     // half a wavelength
			const float stroke = 2.0f;   // perpendicular line thickness
			const int gap = 6;           // px between baseline (rc.top) and wave top
			const int height = static_cast<int>(2 * amp + stroke) + 2;

			// Vertical distance -> perpendicular distance on the diagonals, so
			// the stroke reads as `stroke` px thick regardless of slope.
			const float slope = (2.0f * amp) / half;
			const float perp = 1.0f / std::sqrt(1.0f + slope * slope);

			RGBAImage image(width, height, 1.0, nullptr);
			const float yMid = height / 2.0f;
			const int period = static_cast<int>(2 * half);
			for (int x = 0; x < width; x++) {
				// Phase from the run's start and rising first, so a marked token
				// always begins its wave identically to the error card's
				// (moveTo(x0, y + amp) then up).
				const float px = static_cast<float>(x % period) + 0.5f;
				const float yc = (px < half)
					? amp - (2.0f * amp) * (px / half)
					: -amp + (2.0f * amp) * ((px - half) / half);
				const float rowCentre = yMid + yc;
				for (int y = 0; y < height; y++) {
					const float d = std::abs((y + 0.5f) - rowCentre) * perp;
					const float a = (stroke / 2.0f) + 0.5f - d;
					if (a > 0.0f)
						image.SetPixel(x, y, sacDraw.fore,
							static_cast<int>(std::min(1.0f, a) * 0xff));
				}
			}

			PRectangle rcSquiggle(static_cast<XYPOSITION>(ircSquiggle.left),
				rc.top + gap,
				static_cast<XYPOSITION>(ircSquiggle.left + width),
				rc.top + gap + height);
			// Never draw past the bottom of the line box.
			if (rcSquiggle.bottom > rcLine.bottom) {
				const XYPOSITION dy = rcSquiggle.bottom - rcLine.bottom;
				rcSquiggle.top -= dy;
				rcSquiggle.bottom -= dy;
			}
			surface->DrawRGBAImage(rcSquiggle, image.GetWidth(), image.GetHeight(), image.Pixels());
		}
	} else if (sacDraw.style == INDIC_SQUIGGLEPIXMAP) {
		const PRectangle rcSquiggle = PixelGridAlign(rc);

		const int width = std::min(4000, static_cast<int>(rcSquiggle.Width()));
		RGBAImage image(width, 3, 1.0, nullptr);
		enum { alphaFull = 0xff, alphaSide = 0x2f, alphaSide2=0x5f };
		for (int x = 0; x < width; x++) {
			if (x%2) {
				// Two halfway columns have a full pixel in middle flanked by light pixels
				image.SetPixel(x, 0, sacDraw.fore, alphaSide);
				image.SetPixel(x, 1, sacDraw.fore, alphaFull);
				image.SetPixel(x, 2, sacDraw.fore, alphaSide);
			} else {
				// Extreme columns have a full pixel at bottom or top and a mid-tone pixel in centre
				image.SetPixel(x, (x % 4) ? 0 : 2, sacDraw.fore, alphaFull);
				image.SetPixel(x, 1, sacDraw.fore, alphaSide2);
			}
		}
		surface->DrawRGBAImage(rcSquiggle, image.GetWidth(), image.GetHeight(), image.Pixels());
	} else if (sacDraw.style == INDIC_SQUIGGLELOW) {
		surface->MoveTo(irc.left, irc.top);
		int x = irc.left + 3;
		int y = 0;
		while (x < rc.right) {
			surface->LineTo(x - 1, irc.top + y);
			y = 1 - y;
			surface->LineTo(x, irc.top + y);
			x += 3;
		}
		surface->LineTo(irc.right, irc.top + y);	// Finish the line
	} else if (sacDraw.style == INDIC_TT) {
		surface->MoveTo(irc.left, ymid);
		int x = irc.left + 5;
		while (x < rc.right) {
			surface->LineTo(x, ymid);
			surface->MoveTo(x-3, ymid);
			surface->LineTo(x-3, ymid+2);
			x++;
			surface->MoveTo(x, ymid);
			x += 5;
		}
		surface->LineTo(irc.right, ymid);	// Finish the line
		if (x - 3 <= rc.right) {
			surface->MoveTo(x-3, ymid);
			surface->LineTo(x-3, ymid+2);
		}
	} else if (sacDraw.style == INDIC_DIAGONAL) {
		int x = irc.left;
		while (x < rc.right) {
			surface->MoveTo(x, irc.top + 2);
			int endX = x+3;
			int endY = irc.top - 1;
			if (endX > rc.right) {
				endY += endX - irc.right;
				endX = irc.right;
			}
			surface->LineTo(endX, endY);
			x += 4;
		}
	} else if (sacDraw.style == INDIC_STRIKE) {
		surface->MoveTo(irc.left, irc.top - 4);
		surface->LineTo(irc.right, irc.top - 4);
	} else if ((sacDraw.style == INDIC_HIDDEN) || (sacDraw.style == INDIC_TEXTFORE)) {
		// Draw nothing
	} else if (sacDraw.style == INDIC_BOX) {
		surface->MoveTo(irc.left, ymid + 1);
		surface->LineTo(irc.right, ymid + 1);
		const int lineTop = static_cast<int>(rcLine.top) + 1;
		surface->LineTo(irc.right, lineTop);
		surface->LineTo(irc.left, lineTop);
		surface->LineTo(irc.left, ymid + 1);
	} else if (sacDraw.style == INDIC_ROUNDBOX ||
		sacDraw.style == INDIC_STRAIGHTBOX ||
		sacDraw.style == INDIC_FULLBOX) {
		PRectangle rcBox = rcLine;
		if (sacDraw.style != INDIC_FULLBOX)
			rcBox.top = rcLine.top + 1;
		rcBox.left = rc.left;
		rcBox.right = rc.right;
		surface->AlphaRectangle(rcBox, (sacDraw.style == INDIC_ROUNDBOX) ? 1 : 0,
			sacDraw.fore, fillAlpha, sacDraw.fore, outlineAlpha, 0);
	} else if (sacDraw.style == INDIC_GRADIENT ||
		sacDraw.style == INDIC_GRADIENTCENTRE) {
		PRectangle rcBox = rc;
		rcBox.top = rcLine.top + 1;
		rcBox.bottom = rcLine.bottom;
		const Surface::GradientOptions options = Surface::GradientOptions::topToBottom;
		const ColourAlpha start(sacNormal.fore, fillAlpha);
		const ColourAlpha end(sacNormal.fore, 0);
		std::vector<ColourStop> stops;
		switch (sacDraw.style) {
		case INDIC_GRADIENT:
			stops.push_back(ColourStop(0.0, start));
			stops.push_back(ColourStop(1.0, end));
			break;
		case INDIC_GRADIENTCENTRE:
			stops.push_back(ColourStop(0.0, end));
			stops.push_back(ColourStop(0.5, start));
			stops.push_back(ColourStop(1.0, end));
			break;
		}
		surface->GradientRectangle(rcBox, stops, options);
	} else if (sacDraw.style == INDIC_DOTBOX) {
		PRectangle rcBox = PixelGridAlign(rc);
		rcBox.top = rcLine.top + 1;
		rcBox.bottom = rcLine.bottom;
		IntegerRectangle ircBox(rcBox);
		// Cap width at 4000 to avoid large allocations when mistakes made
		const int width = std::min(ircBox.Width(), 4000);
		RGBAImage image(width, ircBox.Height(), 1.0, nullptr);
		// Draw horizontal lines top and bottom
		for (int x=0; x<width; x++) {
			for (int y = 0; y<ircBox.Height(); y += ircBox.Height() - 1) {
				image.SetPixel(x, y, sacDraw.fore, ((x + y) % 2) ? outlineAlpha : fillAlpha);
			}
		}
		// Draw vertical lines left and right
		for (int y = 1; y<ircBox.Height(); y++) {
			for (int x=0; x<width; x += width-1) {
				image.SetPixel(x, y, sacDraw.fore, ((x + y) % 2) ? outlineAlpha : fillAlpha);
			}
		}
		surface->DrawRGBAImage(rcBox, image.GetWidth(), image.GetHeight(), image.Pixels());
	} else if (sacDraw.style == INDIC_DASH) {
		int x = irc.left;
		while (x < rc.right) {
			surface->MoveTo(x, ymid);
			surface->LineTo(std::min(x + 4, irc.right), ymid);
			x += 7;
		}
	} else if (sacDraw.style == INDIC_DOTS) {
		int x = irc.left;
		while (x < irc.right) {
			const PRectangle rcDot = PRectangle::FromInts(x, ymid, x + 1, ymid + 1);
			surface->FillRectangle(rcDot, sacDraw.fore);
			x += 2;
		}
	} else if (sacDraw.style == INDIC_COMPOSITIONTHICK) {
		const PRectangle rcComposition(rc.left+1, rcLine.bottom-2, rc.right-1, rcLine.bottom);
		surface->FillRectangle(rcComposition, sacDraw.fore);
	} else if (sacDraw.style == INDIC_COMPOSITIONTHIN) {
		const PRectangle rcComposition(rc.left+1, rcLine.bottom-2, rc.right-1, rcLine.bottom-1);
		surface->FillRectangle(rcComposition, sacDraw.fore);
	} else if (sacDraw.style == INDIC_POINT || sacDraw.style == INDIC_POINTCHARACTER) {
		if (rcCharacter.Width() >= 0.1) {
			const XYPOSITION pixelHeight = floor(rc.Height() - 1.0f);	// 1 pixel onto next line if multiphase
			const XYPOSITION x = (sacDraw.style == INDIC_POINT) ? (rcCharacter.left) : ((rcCharacter.right + rcCharacter.left) / 2);
			const XYPOSITION ix = round(x);
			const XYPOSITION iy = floor(rc.top + 1.0f);
			Point pts[] = {
				Point(ix - pixelHeight, iy + pixelHeight),	// Left
				Point(ix + pixelHeight, iy + pixelHeight),	// Right
				Point(ix, iy)								// Top
			};
			surface->Polygon(pts, ELEMENTS(pts), sacDraw.fore, sacDraw.fore);
		}
	} else {	// Either INDIC_PLAIN or unknown
		surface->MoveTo(irc.left, ymid);
		surface->LineTo(irc.right, ymid);
	}
}

void Indicator::SetFlags(int attributes_) {
	attributes = attributes_;
}
