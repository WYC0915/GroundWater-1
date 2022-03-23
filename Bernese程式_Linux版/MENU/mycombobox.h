
#ifndef MYCOMBOBOX_H
#define MYCOMBOBOX_H

#include <qcombobox.h>

class t_mycombobox : public QComboBox
{
  public:
    t_mycombobox( bool rw, QWidget* parent=0, const char* name=0 );
    ~t_mycombobox();

    void setEnabled(bool enabled);
};

#endif

