
#include <qdialog.h>
#include <qpushbutton.h>
#include <qstringlist.h>

#include <qwt_plot.h>
#include <qwt_plot_curve.h>

class t_graphwin : public QDialog
{
  Q_OBJECT

  public:
    t_graphwin(QWidget* parent, const QString& file, int col1,
               int col2[], double scale, int nCurves,
               const QString& plotTitle, const QStringList& legend,
               const QString& xTitle, const QString& yTitle,
               double lowerLimit = 0.0, double upperLimit = 0.0,
               bool skipMarked = true, const bool* y2Axis = 0,
               const QString& y2Title = 0,
               QwtPlotCurve::CurveStyle cStyle = QwtPlotCurve::Lines);

    t_graphwin(QWidget* parent, int nPoints, int nCurves,
               double* xx, double** yy,
               const QString& plotTitle, const QStringList& legend,
               const QString& xTitle, const QString& yTitle,
               const bool* y2Axis = 0, const QString& y2Title = 0,
               QwtPlotCurve::CurveStyle cStyle = QwtPlotCurve::Lines);

    ~t_graphwin();

    static int  readData(const QString& pltFile, int col1, int col2[],
                         int nCurves, int& nPoints, double* &xx, double** &yy,
                         double scale = 1.0, double lowerLimit = 0.0,
                         double upperLimit = 0.0, bool skipMarked = true);

    void setLogarithmic(bool value){_logarithmic = value;};

  public slots:
    int exec();

  private slots:
    void slotPrint();

  private:
    static int readFile(const QString& pltFile, QStringList& list,
                        bool skipMarked);
    void initialize();
    void resizeEvent(QResizeEvent *e);

    QwtPlot*      _plot;
    QPushButton*  _cancelButton;
    QPushButton*  _printButton;
    QString       _plotTitle;
    QStringList   _legend;
    QString       _xTitle;
    QString       _yTitle;
    QString       _y2Title;
    int           _nCurves;
    int           _nPoints;
    double*       _xx;
    double**      _yy;
    bool          _ownBuffer;
    const bool*   _y2Axis;
    QwtPlotCurve::CurveStyle    _cStyle;
    QPoint        _pressPoint;
    bool          _logarithmic;
};

