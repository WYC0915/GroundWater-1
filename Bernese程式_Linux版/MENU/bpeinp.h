
#ifndef BPEINP_H
#define BPEINP_H

#include "inpfile.h"

class t_bpeinp : public t_inpfile {
  public:
    t_bpeinp(const QString& inpFileName, bool superBPE);
    ~t_bpeinp();

    virtual t_keyword* getKey(const QString& keyName);
    virtual QString    getKeySel0(const QString& keyName);

    void setSuperBPE(bool superBPE) {_superBPE = superBPE;}

  private:
    bool _superBPE;
};

#endif
