
#ifndef MYSPINBOX_H
#define MYSPINBOX_H

#include <qspinbox.h>

class t_myspinbox : public QSpinBox
{
  public:
    t_myspinbox( int minValue, int maxValue, int step = 1,
                 QWidget* parent = 0, const char* name = 0 );
    ~t_myspinbox();

    void interpretText();
};

#endif

