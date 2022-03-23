
#ifndef MENSTYLE_H
#define MENSTYLE_H

#include <qwindowsstyle.h>

class t_menstyle : public QWindowsStyle
{
  public:
    t_menstyle();
    ~t_menstyle();
    int pixelMetric( PixelMetric metric, const QStyleOption *option = 0,
                     const QWidget *widget = 0 ) const;
  private:
};

#endif



