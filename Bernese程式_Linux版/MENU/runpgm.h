
#ifndef RUNPGM_H
#define RUNPGM_H

#include <qwidget.h>
#include <qstring.h>
#include "canvas.h"

void runpgm(QString inpFileName, const QString& errFileName,
            t_canvas* canvas, bool backgroundAllowed,
            bool mayUseEcho = true);

void runbpe(const QString& inpFileName);

int crepro(const QString& exeProgram, const QString& inpFileName,
           bool background, bool mayUseEcho, bool detached = false);

#endif

