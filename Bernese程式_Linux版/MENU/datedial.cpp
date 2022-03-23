
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_datedial
 *
 * Purpose:    This class implements a date selection dialog. The user may
 *             set the date in various formats (e.g. MJD or GPS week).
 *
 * Author:     L. Mervart
 *
 * Created:    18-APR-2000
 *
 * Changes:
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include <QHBoxLayout>

#include "datedial.h"
#include "initmenu.h"
#include "menutils.h"
#include "textwin.h"

// Constructor
////////////////////////////////////////////////////////////////////////////
t_datedial::t_datedial(bool showIt) : QDialog(0, "MJD", false) {

  this->setAttribute(Qt::WA_DeleteOnClose);

  int fontW = QFontMetrics(this->font()).width('W');

  _juldat = new t_juldat();
  _juldat->setMJD( initmenu.getMJD() );

  setCaption("Date Selection Dialog");

  this->setFont( initmenu.getFontLarge() );

  QGridLayout* gridLayout = new QGridLayout(this);

  // Year, Month and Day
  // -------------------
  _lab_ymd = new QLabel(" Year Month Day (YYYY MM DD)");
  gridLayout->addWidget(_lab_ymd, 0, 0);

  _edt_ymd = new QLineEdit;
  _edt_ymd->setMaximumWidth(11*fontW);
  _edt_ymd->setMinimumWidth(11*fontW);
  gridLayout->addWidget(_edt_ymd, 0, 1);

  _but_ymd = new QPushButton;
  _but_ymd->setMaximumWidth(5*fontW);
  gridLayout->addWidget(_but_ymd, 0, 2);
  connect( _but_ymd, SIGNAL(clicked()), this, SLOT(setYMD()));

  // Modified Julian Date
  // --------------------
  _lab_mjd = new QLabel(" Modified Julian Date");
  gridLayout->addWidget(_lab_mjd, 1, 0);

  _edt_mjd = new QLineEdit(0);
  _edt_mjd->setMaximumWidth(11*fontW);
  _edt_mjd->setMinimumWidth(11*fontW);
  gridLayout->addWidget(_edt_mjd, 1, 1);

  _but_mjd = new QPushButton(0);
  _but_mjd->setMaximumWidth(5*fontW);
  gridLayout->addWidget(_but_mjd, 1, 2);
  connect( _but_mjd, SIGNAL(clicked()), this, SLOT(setMJD()));

  // GPS Week and Day of Week
  // ------------------------
  _lab_gps = new QLabel(" GPS Week, Day of Week (WWWW D)");
  gridLayout->addWidget(_lab_gps, 2, 0);

  _edt_gps = new QLineEdit;
  _edt_gps->setMaximumWidth(11*fontW);
  _edt_gps->setMinimumWidth(11*fontW);
  gridLayout->addWidget(_edt_gps, 2, 1);

  _but_gps = new QPushButton;
  _but_gps->setMaximumWidth(5*fontW);
  gridLayout->addWidget(_but_gps, 2, 2);
  connect( _but_gps, SIGNAL(clicked()), this, SLOT(setGPS()));

  // Year and Day of Year
  // --------------------
  _lab_doy = new QLabel(" Year, Day of Year (YYYY DDD)");
  gridLayout->addWidget(_lab_doy, 3, 0);

  _edt_doy = new QLineEdit;
  _edt_doy->setMaximumWidth(11*fontW);
  _edt_doy->setMinimumWidth(11*fontW);
  gridLayout->addWidget(_edt_doy, 3, 1);

  _but_doy = new QPushButton;
  _but_doy->setMaximumWidth(5*fontW);
  gridLayout->addWidget(_but_doy, 3, 2);
  connect( _but_doy, SIGNAL(clicked()), this, SLOT(setDOY()));

  // Buttons
  // -------
  QHBoxLayout* pmLayout = new QHBoxLayout;

  _plusButton = new QPushButton("+1");
  _plusButton->setMaximumWidth(5*fontW);
  pmLayout->addWidget(_plusButton);
  connect( _plusButton, SIGNAL(clicked()), this, SLOT(slotPlus()) );

  _minusButton = new QPushButton("-1");
  _minusButton->setMaximumWidth(5*fontW);
  pmLayout->addWidget(_minusButton);
  connect( _minusButton, SIGNAL(clicked()), this, SLOT(slotMinus()) );

  _todayButton = new QPushButton("Today");
  _todayButton->setMaximumWidth(7*fontW);
  pmLayout->addWidget(_todayButton);
  connect( _todayButton, SIGNAL(clicked()), this, SLOT(today()) );

  _computeButton = new QPushButton("Compute");
  _computeButton->setMaximumWidth(8*fontW);
  pmLayout->addWidget(_computeButton);
  connect( _computeButton, SIGNAL(clicked()), this, SLOT(compute()) );

  gridLayout->addLayout(pmLayout,4,0,1,3);

  QHBoxLayout* butLayout = new QHBoxLayout;

#ifndef CELMECH

  // Set the last Session Character
  // ------------------------------
  _lab_chr = new QLabel(" Session Char ", 0);
  _lab_chr->setMaximumWidth(15*fontW);
  gridLayout->addWidget(_lab_chr, 5, 0);

  _edt_chr = new QLineEdit(0);
  _edt_chr->setMaximumWidth(3*fontW);
  gridLayout->addWidget(_edt_chr, 5, 1);
  _edt_chr->setMaxLength(1);
  QString sesChr = initmenu.getSessChar();
  _edt_chr->setText( sesChr.stripWhiteSpace().right(1) );

  // Select the Session Table
  // ------------------------
  _lab_tab = new QLabel(" Session Table", 0);
  _lab_tab->setMaximumWidth(15*fontW);
  gridLayout->addWidget(_lab_tab, 6, 0);

  _sessionKey = initmenu.getKey("SESSION_TABLE");

  _sessionTable = new t_selwin(0, gridLayout, _sessionKey, 6, 1, 0);
  _sessionTable->blockSignals(true);
  _sessionTable->setText(_sessionKey->getValue());
  _sessionTable->blockSignals(false);

  // Set the last Job ID
  // -------------------
  _lab_job = new QLabel(" Job ID", 0);
  _lab_job->setMaximumWidth(8*fontW);
  gridLayout->addWidget(_lab_job, 7, 0);

  _edt_job = new QLineEdit(0);
  _edt_job->setMaximumWidth(4*fontW);
  gridLayout->addWidget(_edt_job, 7, 1);
  _edt_job->setMaxLength(2);
  QString jobID = initmenu.getJobID();
  _edt_job->setText( jobID.stripWhiteSpace().right(2) );

  // Buttons
  // -------
  _hlpButton = new QPushButton("Help", 0);
  _hlpButton->setMaximumWidth(5*fontW);
  butLayout->addWidget(_hlpButton);
  connect( _hlpButton, SIGNAL(clicked()), this, SLOT(slotHlp()) );

  _setButton = new QPushButton("Set", 0);
  _setButton->setMaximumWidth(5*fontW);
  butLayout->addWidget(_setButton);
  connect( _setButton, SIGNAL(clicked()), this, SLOT(slotSet()) );

  _cancelButton = new QPushButton("Cancel", 0);
  _cancelButton->setMaximumWidth(7*fontW);
  butLayout->addWidget(_cancelButton);
  connect( _cancelButton, SIGNAL(clicked()), this, SIGNAL(dateClosed()) );

#else
  _lab_chr      = 0;
  _edt_chr      = 0;
  _lab_tab      = 0;
  _sessionTable = 0;
  _lab_job      = 0;
  _edt_job      = 0;
  _hlpButton    = 0;
  _setButton    = 0;
  _cancelButton = 0;
#endif

  _okButton = new QPushButton("OK", 0);
  _okButton->setMaximumWidth(5*fontW);
  butLayout->addWidget(_okButton);
  connect( _okButton, SIGNAL(clicked()), this, SLOT(slotOK()) );

  gridLayout->addLayout(butLayout,8,0,1,3);

  // Initialize all values
  // ---------------------
  compute();

  if (showIt) {
    show();
  }
}

// Destructor
////////////////////////////////////////////////////////////////////////////
t_datedial::~t_datedial() {
  delete _juldat  ;
  delete _lab_ymd ;
  delete _edt_ymd ;
  delete _but_ymd ;
  delete _lab_mjd ;
  delete _edt_mjd ;
  delete _but_mjd ;
  delete _lab_gps ;
  delete _edt_gps ;
  delete _but_gps ;
  delete _lab_doy ;
  delete _edt_doy ;
  delete _but_doy ;
  delete _lab_chr ;
  delete _lab_tab ;
  delete _sessionTable;
  delete _edt_chr ;
  delete _lab_job ;
  delete _edt_job ;
  delete _plusButton;
  delete _minusButton;
  delete _todayButton ;
  delete _computeButton ;
  delete _hlpButton ;
  delete _setButton ;
  delete _okButton ;
  delete _cancelButton ;
}

// Make all Fields visible
////////////////////////////////////////////////////////////////////////////
void t_datedial::showAll() {
  _edt_ymd->show();
  _edt_mjd->show();
  _edt_gps->show();
  _edt_doy->show();

  _edt_ymd->setEnabled(false);
  _edt_mjd->setEnabled(false);
  _edt_gps->setEnabled(false);
  _edt_doy->setEnabled(false);
  _computeButton->setEnabled(false);

  _edt_ymd->setEdited(false);
  _edt_mjd->setEdited(false);
  _edt_gps->setEdited(false);
  _edt_doy->setEdited(false);
}

// Perform the computation
////////////////////////////////////////////////////////////////////////////
void t_datedial::compute() {
  QStringList hlp;

  if      ( _edt_ymd->edited() ) {
    hlp = QStringList::split(QRegExp("\\s"), _edt_ymd->text());
    _juldat->setYMD( hlp[0].toInt(), hlp[1].toInt(), hlp[2].toDouble() );
  }
  else if ( _edt_mjd->edited() ) {
    _juldat->setMJD( _edt_mjd->text().toDouble() );
  }
  else if ( _edt_gps->edited() ) {
    hlp = QStringList::split(QRegExp("\\s"), _edt_gps->text());
    _juldat->setGPS( hlp[0].toInt(), hlp[1].toDouble() );
  }
  else if ( _edt_doy->edited() ) {
    hlp = QStringList::split(QRegExp("\\s"), _edt_doy->text());
    _juldat->setDoY( hlp[0].toInt(), hlp[1].toDouble() );
  }

  int    YY;
  int    MM;
  int    GPSWeek;

  double DD;
  double mjd;
  double WeekDay;
  double DoY;

  mjd = _juldat->getMJD();
  _juldat->getYMD(YY, MM, DD);
  _juldat->getGPS(GPSWeek, WeekDay);
  _juldat->getDoY(YY, DoY);

  QString stringYMD = QString().sprintf("%d %d %d", YY, MM, (int) DD);
  _edt_ymd->setText(stringYMD);

  QString stringMJD = QString().sprintf("%d", (int) mjd);
  _edt_mjd->setText(stringMJD);

  QString stringGPS = QString().sprintf("%d %d", GPSWeek, (int) WeekDay);
  _edt_gps->setText(stringGPS);

  QString stringDoY = QString().sprintf("%d %d", YY, (int) DoY);
  _edt_doy->setText(stringDoY);

  showAll();
}

// Prompt setting year, month and day
////////////////////////////////////////////////////////////////////////////
void t_datedial::setYMD() {
  hideAll();
  _edt_ymd->show();
  _edt_ymd->setEnabled(true);
}

// Prompt setting Modified Julian Date
////////////////////////////////////////////////////////////////////////////
void t_datedial::setMJD() {
  hideAll();
  _edt_mjd->show();
  _edt_mjd->setEnabled(true);
}

// Prompt setting GPS week and day of week
////////////////////////////////////////////////////////////////////////////
void t_datedial::setGPS() {
  hideAll();
  _edt_gps->show();
  _edt_gps->setEnabled(true);
}

// Prompt setting year and day of year
////////////////////////////////////////////////////////////////////////////
void t_datedial::setDOY() {
  hideAll();
  _edt_doy->show();
  _edt_doy->setEnabled(true);
}

// Make all Fields invisible
////////////////////////////////////////////////////////////////////////////
void t_datedial::hideAll() {
  _edt_ymd->hide();
  _edt_mjd->hide();
  _edt_gps->hide();
  _edt_doy->hide();
  _computeButton->setEnabled(true);
}

// Accept the new Setting
////////////////////////////////////////////////////////////////////////////
void t_datedial::slotOK() {
  slotSet();
  close();
}

// Help
////////////////////////////////////////////////////////////////////////////
void t_datedial::slotHlp() {
  new t_textwin(0, false, "SETDATE", t_textwin::html);
}

// Accept the new Setting
////////////////////////////////////////////////////////////////////////////
void t_datedial::slotSet() {
  compute();

  _sessionKey = initmenu.getKey("SESSION_TABLE");
  if (_sessionKey && _sessionTable) {
    _sessionKey->setValue( _sessionTable->text() );
    _sessionKey->updateSel();
    _sessionKey->saveInputFile();
    initmenu.updateListOfSessions();
  }

  initmenu.setMJD(_juldat->getMJD());
  if (_edt_chr) initmenu.setSessChar(_edt_chr->text());
  if (_edt_job) initmenu.setJobID(_edt_job->text());

  emit dateAccept();
}

// Plus One, Minus One
////////////////////////////////////////////////////////////////////////////
void t_datedial::slotPlus() {
  _juldat->setMJD(_juldat->getMJD() + 1);
  compute();
}

void t_datedial::slotMinus() {
  _juldat->setMJD(_juldat->getMJD() - 1);
  compute();
}

void t_datedial::setPlus() {
  _juldat->setMJD(_juldat->getMJD() + 1);
  slotSet();
}

void t_datedial::setMinus(){
  _juldat->setMJD(_juldat->getMJD() - 1);
  slotSet();
}

// Close the Dialog
////////////////////////////////////////////////////////////////////////////
void t_datedial::closeEvent(QCloseEvent* event) {
  emit dateClosed();
  event->accept();
}

// Today
////////////////////////////////////////////////////////////////////////////
void t_datedial::today() {
  int jd = QDate::currentDate().toJulianDay();

  _juldat->setMJD( jd - 2400000.5 );
  compute();
}
