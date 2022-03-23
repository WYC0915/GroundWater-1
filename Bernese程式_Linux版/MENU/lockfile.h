
#ifndef LOCKFILE_H
#define LOCKFILE_H

#include <qstring.h>

bool lockFile(const QString& fileName, int maxTrials = 10,
                                     double sleepTime = 1.0);

bool unlockFile(const QString& fileName);

#endif

