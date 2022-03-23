
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_menstyle
 *
 * Purpose:
 *
 * Author:     L. Mervart
 *
 * Created:    09-AUG-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include "menstyle.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_menstyle::t_menstyle() : QWindowsStyle() {
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_menstyle::~t_menstyle() {
}

//
////////////////////////////////////////////////////////////////////////////
int t_menstyle::pixelMetric(PixelMetric metric, const QStyleOption *option,
                            const QWidget* widget) const {
  if      (metric == PM_IndicatorWidth) {
    return 20;
  }
  else if (metric == PM_IndicatorHeight) {
    return 20;
  }
  else {
    return QWindowsStyle::pixelMetric(metric, option, widget);
  }
}

