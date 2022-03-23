
#ifndef MYCHECK_H
#define MYCHECK_H

#include <qcheckbox.h>

#include "menstyle.h"

class t_mycheck : public QCheckBox
{
  public:
    t_mycheck(QWidget* parent, const char *name);
    ~t_mycheck();
  private:
    t_menstyle* _menstyle;
};

#endif

