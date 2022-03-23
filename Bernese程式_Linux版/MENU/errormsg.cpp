/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Function:   errormsg
 *
 * Purpose:    This functions prints error messages.
 *
 * Author:     L. Mervart
 *
 * Created:    27-JUL-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <iostream>
#include <qapplication.h>
#include <qmessagebox.h>
#include "errormsg.h"
#include "initmenu.h"

using namespace std;

void errormsg(const QString& message,  bool forceMonospace)
{
  if (initmenu.getLogModus() == t_initmenu::quiet) return;

  if (initmenu.getIntModus() == t_initmenu::INT) {
    QApplication::setOverrideCursor( Qt::arrowCursor );

    QMessageBox mb;
    mb.setCaption("Message");
    mb.setText(message);
    mb.setIcon(QMessageBox::Information);
    if (forceMonospace) {
      mb.setFont( initmenu.getFontSmall() );
    }
    mb.exec();

    QApplication::restoreOverrideCursor();
  }
  else {
    qWarning("%s", message.latin1());
  }
}
