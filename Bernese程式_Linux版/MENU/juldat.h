
#ifndef JULDAT_H
#define JULDAT_H

class t_juldat
{
  public:
    t_juldat();
    ~t_juldat();

    void    setMJD(double MJD);
    void    setYMD(int YY, int MM, double DD);
    void    setGPS(int GPSWeek, double WeekDay);
    void    setDoY(int YY, double DoY);

    double  getMJD();
    void    getYMD(int& YY, int& MM, double& DD);
    void    getGPS(int& GPSWeek, double& WeekDay);
    void    getDoY(int& YY, double& DoY);

  private:
    double  mjd;
};

#endif
