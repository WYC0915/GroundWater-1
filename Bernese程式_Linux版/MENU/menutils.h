
#ifndef MENUTILS_H
#define MENUTILS_H

#include <qstring.h>
#include <qstringlist.h>

#include "menu.h"

QString  expandEnvVar(const QString& str, int* systemVarNotFound = 0);

QString  stripPath(const QString& str, bool stripExtension = false);

QString  stripExtension(const QString& str, QString* ext = 0);

QString  stripFileName(const QString& str);

void     unilineSplit(const QString& uniline, QStringList& outList);
void     cardSplit(const QString& inpStr, QStringList& outList);

QString  getKeyValFromMsg(const QString& inMsg, const QString& key);

t_irc fileCopy(const QString& source, const QString& target);

QStringList* findPgmInScript(const QString& script);

void splitCondLine(const QString& line, QStringList& condKeyName,
                   QStringList& condRelOper, QStringList& condValue,
                   QStringList& condLogOper);

bool evalLogTags(const QStringList& keyValue, const QStringList& condRelOper,
               const QStringList& condValue, const QStringList& condLogOper);

void appendDirs(QStringList& dirList, const QString& line);

QString fileToString(const QString& fileName);

void mysleep(double sleepTime);

QString  stripBnpPrefix(const QString fileName);

QByteArray compress(const QString& srcStr);
QString    uncompress(const QByteArray& srcStr);

#endif

