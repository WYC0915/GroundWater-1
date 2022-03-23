
#ifndef PCFFILE_H
#define PCFFILE_H

#include <qstring.h>
#include <qmap.h>

#include "inpfile.h"

class t_pcffile : public t_inpfile
{
  public:
    t_pcffile(const char* pcfFileName);
    ~t_pcffile();

    int     getNumberOfScripts();
    QString getOrigName(bool stripExtension);
    int     addToOptions(QMap<QString, QString>& options,
                         int iScript, bool& specialCamp);

  private:
    t_keyword* _listOfScripts;
};

#endif

