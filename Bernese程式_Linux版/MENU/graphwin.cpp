
/* -------------------------------------------------------------------------
 * Celestial Mechanics
 * -------------------------------------------------------------------------
 *
 * Class:      t_graphwin
 *
 * Purpose:    This class implements a plotting widget
 *
 * Author:     L. Mervart
 *
 * Created:    12-MAR-2001
 *
 * Changes:
 *
 * -----------------------------------------------------------------------*/

#include <math.h>
#include <qprinter.h>
#include <qregexp.h>
#include <qapplication.h>

#include <qwt_legend.h>
#include <qwt_symbol.h>
#include <qwt_plot_canvas.h>
#include <qwt_plot_zoomer.h>
#include <qwt_scale_engine.h>

#include "graphwin.h"
#include "initmenu.h"
#include "menutils.h"
#include "errormsg.h"
#include "r_file.h"

// Constructor
///////////////////////////////////////////////////////////////////////////////
t_graphwin::t_graphwin(QWidget* parent, const QString& fileName, int col1,
                       int col2[], double scale, int nCurves,
                       const QString& plotTitle, const QStringList& legend,
                       const QString& xTitle, const QString& yTitle,
                       double lowerLimit, double upperLimit, bool skipMarked,
                       const bool* y2Axis, const QString& y2Title,
                       QwtPlotCurve::CurveStyle cStyle)
: QDialog(parent, "graphwin", true) {

  initialize();

  _nCurves   = nCurves;
  _nPoints   = 0;
  _xx        = 0;
  _yy        = 0;
  _plotTitle = plotTitle;
  _legend    = legend;
  if (_legend.count() == 0) {
    _legend.append("");
  }
  _xTitle    = xTitle;
  _yTitle    = yTitle;
  _y2Axis    = y2Axis;
  if (_y2Axis) {
    _y2Title = y2Title;
  }
  _cStyle    = cStyle;
  _ownBuffer = true;

  QApplication::setOverrideCursor( Qt::waitCursor );
  readData(fileName, col1, col2, _nCurves, _nPoints, _xx, _yy,
           scale, lowerLimit, upperLimit, skipMarked);
  QApplication::restoreOverrideCursor();
}

// Constructor (different version)
///////////////////////////////////////////////////////////////////////////////
t_graphwin::t_graphwin(QWidget* parent, int nPoints, int nCurves,
                       double* xx, double** yy,
                       const QString& plotTitle, const QStringList& legend,
                       const QString& xTitle, const QString& yTitle,
                       const bool* y2Axis, const QString& y2Title,
                       QwtPlotCurve::CurveStyle cStyle)
  : QDialog(parent, 0, true) {

  initialize();

  _nCurves   = nCurves;
  _nPoints   = nPoints;
  _xx        = xx;
  _yy        = yy;
  _plotTitle = plotTitle;
  _legend    = legend;
  if (_legend.count() == 0) {
    _legend.append("");
  }
  _xTitle    = xTitle;
  _yTitle    = yTitle;
  _y2Axis    = y2Axis;
  if (_y2Axis) {
    _y2Title = y2Title;
  }
  _cStyle    = cStyle;
  _ownBuffer = false;
}

// Destructor
///////////////////////////////////////////////////////////////////////////////
t_graphwin::~t_graphwin() {
  delete _plot;
  delete _cancelButton;
  delete _printButton;
  if (_ownBuffer && _xx && _yy) {
    delete[] _xx;
    for (int iC = 0; iC < _nCurves; ++iC) {
      delete[] _yy[iC];
    }
    delete[] _yy;
  }
}

// Initialization
///////////////////////////////////////////////////////////////////////////////
void t_graphwin::initialize() {

  _plot        = 0;
  _logarithmic = false;

  _cancelButton = new QPushButton("Next/Quit", this);
  _cancelButton->show();
  connect( _cancelButton, SIGNAL(clicked()), this, SLOT(reject()) );

  _printButton = new QPushButton("Print", this);
  _printButton->show();
  connect( _printButton, SIGNAL(clicked()), this, SLOT(slotPrint()) );

  this->setGeometry(50,50,700,500);
}

// Response to the Window Resize
///////////////////////////////////////////////////////////////////////////////
void t_graphwin::resizeEvent(QResizeEvent *e) {
  this->resize(e->size());
  if (_plot) {
    _plot->resize(e->size().width(), e->size().height()-40);
    _plot->move(0,0);
  }
  _cancelButton->setGeometry(40, this->height()-35,120, 30);
  _printButton->setGeometry(170, this->height()-35,120, 30);
}

// Print the Plot
///////////////////////////////////////////////////////////////////////////////
void t_graphwin::slotPrint() {
  QPrinter printer;
  if (printer.setup(0)) {
    _plot->print(printer);
  }
}

// Plot the Data
///////////////////////////////////////////////////////////////////////////////
int t_graphwin::exec() {

  if (_xx == 0 || _yy == 0) {
    return QDialog::Rejected;
  }

  QApplication::setOverrideCursor( Qt::waitCursor );

  // Allocate the QwtPlot widget
  // ---------------------------
  _plot = new QwtPlot(_plotTitle, this);
  if (!_plot) {
    errormsg("Cannot Plot Data");
    return 1;
  }

  // Define Colors
  // -------------
  QPen   redPen(QColor(255,0,0));
  QPen   greenPen(QColor(0,255,0));
  QPen   bluePen(QColor(0,0,255));
  QPen   blackPen(QColor(0,0,0));
  QBrush redBrush(QColor(255,0,0));
  QBrush greenBrush(QColor(0,255,0));
  QBrush blueBrush(QColor(0,0,255));

  // Insert new curves
  // -----------------
  for (int iC = 0; iC < _nCurves; ++iC) {

    QwtPlotCurve* curve = new QwtPlotCurve(_legend[iC]);

    // Set Curve Colors and Styles
    // ---------------------------
    curve->setStyle(_cStyle);

    switch( iC % 3 ) {
      case 0:
        curve->setPen(redPen);
        if (_cStyle == QwtPlotCurve::NoCurve) {
          QwtSymbol symbol(QwtSymbol::Diamond, redBrush,redPen,QSize(6,6));
          curve->setSymbol(symbol);
        }
      break;
      case 1:
        curve->setPen(greenPen);
        if (_cStyle == QwtPlotCurve::NoCurve) {
          QwtSymbol symbol(QwtSymbol::Diamond, greenBrush,greenPen,QSize(6,6));
          curve->setSymbol(symbol);
        }
      break;
      case 2:
        curve->setPen(bluePen);
        if (_cStyle == QwtPlotCurve::NoCurve) {
          QwtSymbol symbol(QwtSymbol::Diamond, blueBrush,bluePen,QSize(6,6));
          curve->setSymbol(symbol);
        }
      break;
    }

    // Copy the data
    // -------------
    if (_ownBuffer) {
      curve->setRawData(_xx, _yy[iC], _nPoints);
    }
    else {
      curve->setData(_xx, _yy[iC], _nPoints);
    }

    curve->attach(_plot);
  }

  // Set Axis
  // ---------------
  _plot->setAxisTitle(QwtPlot::xBottom, _xTitle);
  _plot->setAxisTitle(QwtPlot::yLeft,   _yTitle);

  if (_logarithmic) {
    _plot->setAxisScaleEngine(QwtPlot::xBottom, new QwtLog10ScaleEngine);
    _plot->setAxisScaleEngine(QwtPlot::yLeft, new QwtLog10ScaleEngine);
  }

  if (_y2Axis) {
    _plot->enableAxis(QwtPlot::yRight);
    _plot->setAxisTitle(QwtPlot::yRight, _y2Title);
    if (_logarithmic) {
      _plot->setAxisScaleEngine(QwtPlot::yRight, new QwtLog10ScaleEngine);
    }
  }


  // And some more Settings
  // ----------------------

  if (!_legend[0].isEmpty()) {
    _plot->insertLegend(new QwtLegend());
  }
  _plot->setCanvasBackground(Qt::white);
  _plot->setGeometry(0,0,width(),height()-20);

  // Initialize Zooming
  // ------------------
  new QwtPlotZoomer(_plot->canvas());

  _plot->replot();

  QApplication::restoreOverrideCursor();

  return QDialog::exec();
}

// Read the File with Data (static function)
///////////////////////////////////////////////////////////////////////////////
int t_graphwin::readFile(const QString& pltFile, QStringList& list,
                         bool skipMarked) {

  r_file inFile(pltFile);
  if ( !inFile.open(QIODevice::ReadOnly | QIODevice::Text) ) {
    errormsg("Cannot open file " + pltFile);
    return 1;
  }

  QString     line;

  // Read the File
  // -------------
  list.clear();
  while ( !inFile.eof() ) {
    line = inFile.readLine().stripWhiteSpace();

    if (line.isEmpty()) continue;
    if ( !line[0].isNumber() &&
         (line[0] != '-' || !line[1].isNumber()) ) {  // skip comment lines
      continue;
    }
    if (skipMarked && line.find('*') != -1) { // skip marked lines
      continue;
    }
    line.replace(QRegExp("[dD]"), "e");       // Fortran-style Double Precision
    list.append(line);
  }
  inFile.close();

  return 0;
}

// Read the Data (static function)
///////////////////////////////////////////////////////////////////////////////
int t_graphwin::readData(const QString& pltFile, int col1, int col2[],
                         int nCurves, int& nPoints, double* &xx, double** &yy,
                         double scale, double lowerLimit,
                         double upperLimit, bool skipMarked) {

  // Nullify pointers
  // ----------------
  xx = 0;
  yy = 0;

  // Read data from file
  // -------------------
  QStringList list;
  if ( readFile(pltFile, list, skipMarked) ) {
    return 1;
  }

  // Allocate Variables
  // ------------------
  xx = new double[list.count()];
  yy = new double* [nCurves];
  for (int iC = 0; iC < nCurves; ++iC) {
    yy[iC] = new double[list.count()];
  }

  // Read all values
  // ---------------
  nPoints = 0;
  double xMax = 0.0;
  double xMin = 0.0;
  double yMax = 0.0;
  double yMin = 0.0;

  if (scale == 0.0) {
    scale = 1.0;
  }

  for (QStringList::Iterator it = list.begin(); it != list.end(); ++it) {

    QStringList hlpList = QStringList::split( QRegExp("\\s"), *it);
    double xHlp = hlpList[col1-1].toDouble();

    if ( upperLimit == lowerLimit ||
         (xHlp >= lowerLimit && xHlp <= upperLimit) ) {
      nPoints++;
    }
    else {
      continue;
    }
    xx[nPoints-1] = xHlp;
    if (nPoints == 1 || xHlp > xMax) {
      xMax = xHlp;
    }
    if (nPoints == 1 || xHlp < xMin) {
      xMin = xHlp;
    }
    for (int iC = 0; iC < nCurves; ++iC) {
      double yHlp = hlpList[col2[iC]-1].toDouble() * scale;
      if (nPoints == 1 || yHlp > yMax) {
        yMax = yHlp;
      }
      if (nPoints == 1 || yHlp < yMin) {
        yMin = yHlp;
      }
      yy[iC][nPoints-1] = yHlp;
    }
  }
  list.clear();

  // Number of displayed points
  // --------------------------
  int oldModus = initmenu.getLogModus();
  initmenu.setLogModus(t_initmenu::quiet);
  static int maxPoints = initmenu.getKeySel0("PLOT_MAXPOINTS").toInt();
  initmenu.setLogModus(oldModus);
  if (maxPoints == 0) {
    maxPoints = 10000;
  }

  if (nPoints > maxPoints) {

    double dX = (xMax - xMin) / (maxPoints-1);
    double dY = (yMax - yMin) / (maxPoints-1);

    double*  xAll       = xx;
    double** yAll       = yy;
    int      nPointsAll = nPoints;

    xx = new double[nPointsAll];
    yy = new double* [nCurves];
    for (int jj = 0; jj < nCurves; ++jj) {
      yy[jj] = new double[nPointsAll];
    }

    nPoints = 0;

    for (int ii = 0; ii < nPointsAll; ii++) {

      bool addPoint = false;
      if ( nPoints == 0 ) {
        addPoint = true;
      }
      else if ( fabs(xx[nPoints-1] - xAll[ii]) > dX ) {
        addPoint = true;
      }
      else {
        for (int iC = 0; iC < nCurves; ++iC) {
          if ( fabs(yy[iC][nPoints-1] - yAll[iC][ii]) > dY ) {
            addPoint = true;
            break;
          }
        }
      }

      if (addPoint) {
        ++nPoints;
        xx[nPoints-1] = xAll[ii];
        for (int iC = 0; iC < nCurves; ++iC) {
          yy[iC][nPoints-1] = yAll[iC][ii];
        }
      }
    }
    delete[] xAll;
    for (int iC = 0; iC < nCurves; ++iC) {
      delete[] yAll[iC];
    }
    delete[] yAll;
  }
  return 0;
}

