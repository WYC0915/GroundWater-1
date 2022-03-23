
/* -------------------------------------------------------------------------
 * Bernese GPS Software Version 5.0
 * -------------------------------------------------------------------------
 *
 * Class:      t_mainwin
 *
 * Purpose:    This class implements the main application window.
 *
 * Author:     L. Mervart
 *
 * Created:    18-APR-2000
 *
 * Changes:    25-Sep-2012 SL/RD: add menu item README
 *
 * Copyright:  Astronomical Institute
 *              University of Berne
 *                  Switzerland
 * -----------------------------------------------------------------------*/

#include "mainwin.h"
#include "menu.h"
#include "initmenu.h"
#include "seldial.h"
#include "datedial.h"
#include "menutils.h"
#include "errormsg.h"
#include "textwin.h"
#include "lockfile.h"
#include "myfildlg.h"
#include "r_file.h"
#include "r_dir.h"

QStringList lockFiles;

using namespace Qt;

// Constructor
////////////////////////////////////////////////////////////////////////////
t_mainwin::t_mainwin(const QString& inpFileName, bool noInit) : QMainWindow() {

  inpfile             = 0;
  currentSlotMenuItem = -1;
  _datedial           = 0;

  // Font
  // ----
  QApplication::setFont( initmenu.getFontBase(), true );

  // Geometry
  // --------
  int width = initmenu.getKeySel0("MAIN_WIDTH").toInt();
  int height = initmenu.getKeySel0("MAIN_HEIGHT").toInt();
  if (width == 0 || height == 0) {
    width  = DEFAULT_MAIN_WIDTH;
    height = DEFAULT_MAIN_HEIGHT;
  }
  this->setGeometry(20, 20, width, height);

  // Palette
  // -------
  QString     rgbStr = initmenu.getKeySel0("MAIN_PALETTE");
  QStringList rgb    = QStringList::split(QRegExp("\\s+"), rgbStr);
  if (rgb.count() != 6) {
    rgbStr = DEFAULT_MAIN_PALETTE;
    rgb    = QStringList::split(QRegExp("\\s+"), rgbStr);
  }
  QApplication::setPalette(QPalette(
            QColor(rgb[0].toInt(), rgb[1].toInt(), rgb[2].toInt()),
            QColor(rgb[3].toInt(), rgb[4].toInt(), rgb[5].toInt())), true);

  // Canvas
  // ------
  _sb = new QScrollArea();
  setCentralWidget(_sb);
  canvas = 0;
  newCanvas();

  // Tool (Command) Bar and Actions
  // ------------------------------
  toolbar = new QToolBar( "toolbar", this);
  addToolBar(Qt::BottomToolBarArea, toolbar);

  topPanelAction = toolbar->addAction(" ^Top ");
  topPanelAction->setShortcut(CTRL+Key_T);
  connect(topPanelAction, SIGNAL(triggered()), this, SLOT(slotTopPanel()));

  toolbar->addSeparator();

  prevPanelAction = toolbar->addAction(" ^Prev ");
  prevPanelAction->setShortcut(CTRL+Key_P);
  connect(prevPanelAction, SIGNAL(triggered()), this, SLOT(slotPrevPanel()));

  toolbar->addSeparator();

  nextPanelAction = toolbar->addAction(" ^Next ");
  nextPanelAction->setShortcut(CTRL+Key_N);
  connect(nextPanelAction, SIGNAL(triggered()), this, SLOT(slotNextPanel()));

  toolbar->addSeparator();

  cancelAction = toolbar->addAction("Cance^l");
  cancelAction->setShortcut(CTRL+Key_L);
  connect(cancelAction, SIGNAL(triggered()), this, SLOT(slotCancel()));

  toolbar->addSeparator();

  saveAsAction = toolbar->addAction("Save^As");
  saveAsAction->setShortcut(CTRL+Key_A);
  connect(saveAsAction, SIGNAL(triggered()), this, SLOT(slotSaveAsPanels()));

  toolbar->addSeparator();

  saveAction = toolbar->addAction(" ^Save ");
  saveAction->setShortcut(CTRL+Key_S);
  connect(saveAction, SIGNAL(triggered()), this, SLOT(slotSavePanels()));

  toolbar->addSeparator();

  runAction = toolbar->addAction(" ^Run ");
  runAction->setShortcut(CTRL+Key_R);
  connect(runAction, SIGNAL(triggered()), this, SLOT(slotRun()));

  toolbar->addSeparator();

  outputAction = toolbar->addAction("^Output");
  outputAction->setShortcut(CTRL+Key_O);
  connect(outputAction, SIGNAL(triggered()), this, SLOT(slotLastOutput()));

  toolbar->addSeparator();

  rerunAction = toolbar->addAction("Rer^un");
  rerunAction->setShortcut(CTRL+Key_U);
  connect(rerunAction, SIGNAL(triggered()), this, SLOT(slotRerun()));

  toolbar->addSeparator();

  plusSessionAction = toolbar->addAction("^+Day");
  plusSessionAction->setShortcut(CTRL+Key_Plus);
  connect(plusSessionAction, SIGNAL(triggered()),
          this, SLOT(slotPlusSession()));

  minusSessionAction = toolbar->addAction("^-Day");
  minusSessionAction->setShortcut(CTRL+Key_Minus);
  connect(minusSessionAction, SIGNAL(triggered()),
          this, SLOT(slotMinusSession()));

  setActionsOnOff();

  _waitForPanelCheck = false;

  if (!noInit) {
    initialize(inpFileName);
  }

  connect(statusBar(), SIGNAL(messageChanged(const QString&)),
          this, SLOT(slotMessageChanged(const QString&)));

  show();
}


// Destructor
////////////////////////////////////////////////////////////////////////////
t_mainwin::~t_mainwin() {
  delete inpfile ;
}

// Show Help Window
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotHelp(int item) {

  if      (item == MENU_ID_HELP_ABOUT) {
    slotAbout();
  }
  else if (item == MENU_ID_HELP_README) {
    initmenu.readInput();
    new t_textwin(0, false, "README", t_textwin::html);
  }
  else if (item == MENU_ID_HELP_GENERAL || !inpfile) {
    initmenu.readInput();
    new t_textwin(0, false, "PRIMARY", t_textwin::html);
  }
  else if (inpfile) {
    QString fileName;
    if      ( currentSlotMenuItem == MENU_ID_EDTCPU   ) {
      fileName = "CPU";
    }
    else if ( currentSlotMenuItem == MENU_ID_SESSIONS ) {
      fileName = "SESSIONS";
    }
    else {
      fileName = inpfile->getName();
    }

    QString helpKey = canvas->focusWidgetName();

    t_textwin* textwin = new t_textwin(0, false, fileName,
                                       t_textwin::html, helpKey);
    textwin->scrollToAnchor(helpKey);
  }
}

// Read All Input Files
////////////////////////////////////////////////////////////////////////////
void t_mainwin::readAllInputs(bool rb) {
  enableRunAction = rb;
  currentInpFile  = -1;
  readNextInp();
}

// Read Input File
////////////////////////////////////////////////////////////////////////////
void t_mainwin::readNextInp() {

  if (++currentInpFile >= (int) inpFiles.count()) {
    return;
  }

  QString inpFileName = inpFiles[currentInpFile];
  if (inpFileName.isEmpty()) return;

  QString hlpFileName = inpFileName;
  QString jobID = initmenu.getKeySel0("JOB_ID");
  if ( enableRunAction && !jobID.isEmpty() ) {
    hlpFileName += "_" + jobID;
  }
  r_file inFile( expandEnvVar(hlpFileName) );

  if (!inFile.exists()) {
    QString templateFile = expandEnvVar(inpFileName);
    if (!r_file::exists(templateFile)) {
      templateFile = expandEnvVar( initmenu.getPath("PTH_UPDPAN") +
                                         stripPath(inpFileName) ) ;
    }
    if (r_file::exists(templateFile)) {
      QString msg("File " + hlpFileName + " does not exist,\n"
                  "Do you wish to use " + templateFile + " ?");
      if ( QMessageBox::warning(0, "Question", msg,
                                QMessageBox::Yes | QMessageBox::Default,
                                QMessageBox::No ) == QMessageBox::Yes ) {
        fileCopy( templateFile, hlpFileName );
      }
      else {
        readNextInp();
        return;
      }
    }
  }

  if (!initmenu.modemMode()){
    if ( !inFile.open(QIODevice::ReadOnly | QIODevice::Text) ) {
      errormsg("Cannot open file " + hlpFileName);
      return ;
    }
  }

  fileCopy( hlpFileName, stripExtension(hlpFileName) + ".bck");

  if (! mayIcloseInputFile()) return;

  bool expandSelLists = true;
  if ( currentSlotMenuItem == MENU_ID_EDTINP ||
       currentSlotMenuItem == MENU_ID_EDTBPE  ) {
    initmenu.setLogModus(t_initmenu::quiet);
    expandSelLists = false;
  }

  inpfile = new t_inpfile(hlpFileName, currentSlotMenuItem, expandSelLists);
  if (inpfile->hasPanels()) {
    setStatusBarMessage();
    inpfile->firstPan();
    if (inpfile->getCurPan()->isVisible()) {
      newCanvas();
      canvas->showPanel(inpfile);
      setActionsOnOff();
      runAction->setEnabled(enableRunAction);
    }
    else {
      slotNextPanel();
    }
  }
  else {
    delete inpfile;
    inpfile = 0;
  }

  initmenu.setLogModus(t_initmenu::normal);
}

// First Panel or Not
////////////////////////////////////////////////////////////////////////////
bool t_mainwin::isFirstPanel() {

  bool returnValue = false;

  if (inpfile) {
    t_panel* oldPanel = inpfile->getCurPan();
    if (oldPanel) {
      returnValue = true;
      t_panel* prevPanel;
      while ( (prevPanel = inpfile->prevPan()) != 0 ) {
        if ( prevPanel->isVisible() ) {
          returnValue = false;
          break;
        }
      }
      inpfile->setCurPan(oldPanel);
    }
  }
  return returnValue;
}

// Last Visible Panel or Not
////////////////////////////////////////////////////////////////////////////
bool t_mainwin::isLastVisiblePanel() {
  if (inpfile) {
    t_panel* oldPanel = inpfile->getCurPan();
    if (oldPanel == 0) return false;

    while (inpfile->nextPan()) {
      if (inpfile->getCurPan()->isVisible()) {
        inpfile->setCurPan(oldPanel);
        return false;
      }
    }
    inpfile->setCurPan(oldPanel);
    return true;
  }
  return false;
}

// Set Actions on/off (SLOT)
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotSetActionsOnOff() {
  canvas->savePanel();
  setActionsOnOff();
}

// Set Actions on/off
////////////////////////////////////////////////////////////////////////////
void t_mainwin::setActionsOnOff() {
  if (inpfile) {
    if (isLastVisiblePanel()) {
      nextPanelAction->setEnabled(false);
    }
    else {
      nextPanelAction->setEnabled(true);
    }
    if (isFirstPanel()) {
      topPanelAction->setEnabled(false);
      prevPanelAction->setEnabled(false);
    }
    else {
      topPanelAction->setEnabled(true);
      prevPanelAction->setEnabled(true);
    }
    cancelAction->setEnabled(true);
    saveAsAction->setEnabled(true);
    saveAction->setEnabled(true);
  }
  else {
    nextPanelAction->setEnabled(false);
    topPanelAction->setEnabled(false);
    prevPanelAction->setEnabled(false);
    cancelAction->setEnabled(false);
    saveAsAction->setEnabled(false);
    saveAction->setEnabled(false);
    runAction->setEnabled(false);
  }
  outputAction->setEnabled(false);
  rerunAction->setEnabled(false);
}

// Proceed to the next Panel
////////////////////////////////////////////////////////////////////////////
void t_mainwin::resetPanel() {
  if (inpfile) {
    newCanvas();;
    canvas->showPanel(inpfile);
  }
}

// Proceed to the next Panel
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotNextPanel() {
  if (_waitForPanelCheck) return;
  if (inpfile) {
    _waitForPanelCheck = true;
    canvas->savePanel();
    if ( canvas->checkPanel(currentSlotMenuItem) != QMessageBox::Yes ) {
      _waitForPanelCheck = false;
      return;
    }

    t_panel* oldPanel = inpfile->getCurPan();

    while (inpfile->nextPan()) {
      if (inpfile->getCurPan()->isVisible()) {
        newCanvas();
        canvas->showPanel(inpfile);
        setActionsOnOff();
        oldPanel = 0;
        break;
      }
    }
    if (oldPanel != 0) inpfile->setCurPan(oldPanel);
  }
  _waitForPanelCheck = false;
}

// Go back to the previous Panel
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotPrevPanel() {
  if (_waitForPanelCheck) return;
  if (inpfile) {
    _waitForPanelCheck = true;
    canvas->savePanel();
    if ( canvas->checkPanel(currentSlotMenuItem) != QMessageBox::Yes ) {
      _waitForPanelCheck = false;
      return;
    }
    while (inpfile->prevPan()) {
      if (inpfile->getCurPan()->isVisible()) {
        newCanvas();;
        canvas->showPanel(inpfile);
        setActionsOnOff();
        break;
      }
    }
  }
  _waitForPanelCheck = false;
}

// Go back to the First (Top) Panel
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotTopPanel() {
  if (_waitForPanelCheck) return;
  if (inpfile) {
    _waitForPanelCheck = true;
    canvas->savePanel();
    if ( canvas->checkPanel(currentSlotMenuItem) != QMessageBox::Yes ) {
      _waitForPanelCheck = false;
      return;
    }
    if (inpfile->prevPan()) {
      inpfile->firstPan();
      if (inpfile->getCurPan()->isVisible()) {
        newCanvas();;
        canvas->showPanel(inpfile);
        setActionsOnOff();
      }
      else {
        slotNextPanel();
      }
    }
  }
  _waitForPanelCheck = false;
}

// Show Campaign Selection Dialog
////////////////////////////////////////////////////////////////////////////
void t_mainwin::selCamp() {
  QStringList inpList;
  QStringList ioList;

  t_keyword* key = initmenu.getKey("CAMPAIGN");
  for (int ii = 0; ii < (int) key->getSelList().count(); ii++) {
    QString hlp = key->getSelList()[ii];
    inpList.append( hlp.mid(1, hlp.length()-2) );
  }

  ioList.append( initmenu.getActiveCamp() );

  t_seldial dialog(this, key->getName(), inpList, ioList, false);

  int irc = dialog.exec();

  if (irc == QDialog::Accepted) {
    initmenu.setActiveCamp(ioList.first());
    setStatusBarMessage();
    resetPanel();
  }
}

// Cancel Changes in Input File
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotCancel() {
  rungpsFile = "";
  if (mayIcloseInputFile()) {
    if (currentSlotMenuItem == MENU_ID_CAMPLIST) {
      unlockFile(inpFiles.last());
    }

    if ( currentInpFile + 1 < (int) inpFiles.count() &&
         QMessageBox::warning(0, "Question",
                              "Continue Editing remaining Files?",
                              QMessageBox::Yes | QMessageBox::Default,
                              QMessageBox::No ) == QMessageBox::Yes ) {
      readNextInp();
    }
    else {
      newCanvas();;
      setActionsOnOff();
      setStatusBarMessage();
    }
  }
}

// Save Input File As
////////////////////////////////////////////////////////////////////////////
int t_mainwin::slotSaveAsPanels() {
  if (inpfile) {
    QString filter;
    QString oldName;

    switch( inpfile->menuItem() ) {
      case MENU_ID_EDITCRD:
        filter  = "*." + initmenu.getKeySel0("EXT_CRD") + ";;*.*";
        oldName = inpfile->getKeySel0("COORDRS");
      break;
      case MENU_ID_ABBREV:
        filter  = "*." + initmenu.getKeySel0("EXT_ABB") + ";;*.*";
        oldName = inpfile->getKeySel0("ABBREVRS");
      break;
      case MENU_ID_EDSTACRX:
        filter  = "*." + initmenu.getKeySel0("EXT_STA") + ";;*.*";
        oldName = inpfile->getKeySel0("STACRXRS");
      break;
      case MENU_ID_EDITECC:
        filter  = "*." + initmenu.getKeySel0("EXT_ECC") + ";;*.*";
        oldName = inpfile->getKeySel0("ECCENRS");
      break;
      case MENU_ID_EDITVEL:
        filter  = "*." + initmenu.getKeySel0("EXT_VEL") + ";;*.*";
        oldName = inpfile->getKeySel0("VELORS");
      break;
      case MENU_ID_EDITFIX:
        filter  = "*." + initmenu.getKeySel0("EXT_FIX") + ";;*.*";
        oldName = inpfile->getKeySel0("STALSTRS");
      break;
      case MENU_ID_EDITSIG:
        filter  = "*." + initmenu.getKeySel0("EXT_SIG") + ";;*.*";
        oldName = inpfile->getKeySel0("SIGMARS");
      break;
      case MENU_ID_EDITSOS:
        filter  = "*." + initmenu.getKeySel0("EXT_SOS") + ";;*.*";
        oldName = inpfile->getKeySel0("STAWGTRS");
      break;
      case MENU_ID_EDITCLU:
        filter  = "*." + initmenu.getKeySel0("EXT_CLU") + ";;*.*";
        oldName = inpfile->getKeySel0("STACLURS");
      break;
      case MENU_ID_EDTPCF:
        filter  = "*." + initmenu.getKeySel0("EXT_PCF") + ";;*.*";
        oldName = inpfile->getKeySel0("PCFFILRS");
      break;
      case MENU_ID_EDTPCF_NEW:
        filter  = "*." + initmenu.getKeySel0("EXT_PCF") + ";;*.*";
        oldName = inpfile->getName();
      break;
      case MENU_ID_EDTCPU:
        filter  = "*." + initmenu.getKeySel0("EXT_CPU") + ";;*.*";
        oldName = inpfile->getName();
      break;
      case MENU_ID_EDITBSL:
        filter  = "*." + initmenu.getKeySel0("EXT_BSL") + ";;*.*";
        oldName = inpfile->getKeySel0("BASLINRS");
      break;
      case MENU_ID_EDITPLD:
        filter  = "*." + initmenu.getKeySel0("EXT_PLD") + ";;*.*";
        oldName = inpfile->getKeySel0("PLATERS");
      break;
      default:
        filter  = "*." + initmenu.getKeySel0("EXT_INP") + ";;*.*";
        oldName = inpfile->getName();
      break;
    }
    QString outFileName = t_myfildlg::getSaveFileName(oldName, filter, this);
    if (!outFileName.isEmpty()) {
      return savePanels(outFileName);
    }
  }
  return 0;
}

// Save Input File
////////////////////////////////////////////////////////////////////////////
int t_mainwin::slotSavePanels() {

  int irc;

  if (inpfile) {

    switch( inpfile->menuItem() ) {
      case MENU_ID_EDITCRD:
        return savePanels( inpfile->getKeySel0("COORDRS") );
      break;
      case MENU_ID_EDSTACRX:
        return savePanels( inpfile->getKeySel0("STACRXRS") );
      break;
      case MENU_ID_ABBREV:
        return savePanels( inpfile->getKeySel0("ABBREVRS") );
      break;
      case MENU_ID_EDITECC:
        return savePanels( inpfile->getKeySel0("ECCENRS") );
      break;
      case MENU_ID_EDITVEL:
        return savePanels( inpfile->getKeySel0("VELORS") );
      break;
      case MENU_ID_EDITFIX:
        return savePanels( inpfile->getKeySel0("STALSTRS") );
      break;
      case MENU_ID_EDITSIG:
        return savePanels( inpfile->getKeySel0("SIGMARS") );
      break;
      case MENU_ID_EDITSOS:
        return savePanels( inpfile->getKeySel0("STAWGTRS") );
      break;
      case MENU_ID_EDITCLU:
        return savePanels( inpfile->getKeySel0("STACLURS") );
      break;
      case MENU_ID_EDTPCF:
        return savePanels( inpfile->getKeySel0("PCFFILRS") );
      break;
      case MENU_ID_EDITBSL:
        return savePanels( inpfile->getKeySel0("BASLINRS") );
      break;
      case MENU_ID_EDITPLD:
        return savePanels( inpfile->getKeySel0("PLATERS") );
      break;
      case MENU_ID_VAR:
        irc = savePanels( inpfile->getName() );
        initmenu.readInput();
        setStatusBarMessage();
        return irc;
      break;
      default:
        if ( inpfile->getName() == initmenu.primaryInputFileName() ) {
          int irc = savePanels( inpfile->getName() );
          if (irc != 1) {
            return irc;
          }
          initmenu.readInput();
          readAllInputs();
        }
        return savePanels( inpfile->getName() );
      break;
    }
  }
  return 0;
}

// Save Input File
////////////////////////////////////////////////////////////////////////////
int t_mainwin::savePanels(const QString& outFileName) {

  int irc = 1;

  if (inpfile) {
    canvas->savePanel();

    if (inpfile->menuItem() == MENU_ID_CAMPLIST) {
      unlockFile(inpFiles.last());
    }

    if ( canvas->checkPanel(inpfile->menuItem()) != QMessageBox::Yes ) {
      return 0;
    }

    if ( inpfile->menuItem() == MENU_ID_EDTINP ||
         inpfile->menuItem() == MENU_ID_EDTBPE  ) {
      initmenu.setLogModus(t_initmenu::quiet);
    }

    int saveResult = 0;
    switch( inpfile->menuItem() ) {
      case MENU_ID_SESSIONS:
        inpfile->save();
        initmenu.updateListOfSessions();
      break;
      case MENU_ID_EDITCRD:
        inpfile->save();
        saveResult = saveStationFile( outFileName, "CRD_SAVE");
      break;
      case MENU_ID_EDSTACRX:
        inpfile->save();
        saveResult = saveStationFile( outFileName, "STAX_SAVE" );
      break;
      case MENU_ID_ABBREV:
        inpfile->save();
        saveResult = saveStationFile( outFileName, "ABB_SAVE");
      break;
      case MENU_ID_EDITECC:
        inpfile->save();
        saveResult = saveStationFile( outFileName, "ECC_SAVE" );
      break;
      case MENU_ID_EDITVEL:
        inpfile->save();
        saveResult = saveStationFile( outFileName, "VEL_SAVE" );
      break;
      case MENU_ID_EDITFIX:
        inpfile->save();
        saveResult = saveStationFile( outFileName, "FIX_SAVE" );
      break;
      case MENU_ID_EDITSIG:
        inpfile->save();
        saveResult = saveStationFile( outFileName, "SIG_SAVE" );
      break;
      case MENU_ID_EDITSOS:
        inpfile->save();
        saveResult = saveStationFile( outFileName, "SOS_SAVE" );
      break;
      case MENU_ID_EDITCLU:
        inpfile->save();
        saveResult = saveStationFile( outFileName, "CLU_SAVE" );
      break;
      case MENU_ID_EDITBSL:
        inpfile->save();
        saveResult = saveStationFile( outFileName, "BSL_SAVE" );
      break;
      case MENU_ID_EDITPLD:
        inpfile->save();
        saveResult = saveStationFile( outFileName, "PLD_SAVE" );
      break;
      case MENU_ID_EDTPCF:
        inpfile->save();
        saveResult = saveStationFile( outFileName, "PCF_SAVE" );
      break;
      default:
        inpfile->save( outFileName );
      break;
    }

    if (saveResult != 0) {
      return 0;
    }

    errFileName = expandEnvVar( inpfile->getErrorFileName() );

    if (!inpfile->getStdOutFileName().isEmpty()) {
      lastOutput = inpfile->getStdOutFileName();
    }

    if ( inpfile->menuItem() == MENU_ID_PROGRAMS) {
      updateMenuUser();
    }

    if (inpfile->status() != t_inpfile::status_OK) {
      irc = 0;
    }

    newCanvas();;
    delete inpfile;
    inpfile = 0;
    readNextInp();
  }
  setActionsOnOff();
  setStatusBarMessage();

  initmenu.setLogModus(t_initmenu::normal);

  this->setFocus();

  return irc;
}


// Date Selection Dialog
////////////////////////////////////////////////////////////////////////////
void t_mainwin::showJuldat() {
  if (! _datedial) {
    _datedial = new t_datedial();
    connect(_datedial, SIGNAL(dateAccept()), this, SLOT(slotDateAccept()));
    connect(_datedial, SIGNAL(dateClosed()), this, SLOT(slotDateClosed()));
  }
  else {
    _datedial->raise();
  }
}

void t_mainwin::slotDateAccept() {
  setStatusBarMessage();
  resetPanel();
}

void t_mainwin::slotDateClosed() {
  delete _datedial;
  _datedial = 0;
}

// Close the Input Files
////////////////////////////////////////////////////////////////////////////
bool t_mainwin::mayIcloseInputFile() {
  if (inpfile) {
    switch ( QMessageBox::warning(0, "Warning",
             "Previous input file not saved. Save it now?",
             QMessageBox::Yes    | QMessageBox::Default,
             QMessageBox::No,
             QMessageBox::Cancel | QMessageBox::Escape) ) {
      case QMessageBox::Yes:
        if (slotSavePanels()) {
          return true;
        }
        else {
          return false;
        }
      case QMessageBox::No:
        newCanvas();;
        delete inpfile;
        inpfile = 0;
        return true;
      case QMessageBox::Cancel:
        return false;
      default:
      break;
    }
  }
  else {
    return true;
  }
  return false;
}

// Close the application gracefully
////////////////////////////////////////////////////////////////////////////
void t_mainwin::closeEvent(QCloseEvent* event) {
  if (!close()) {
    event->ignore();
  }
}

bool t_mainwin::close() {
  if (mayIcloseInputFile()) {

    // Remove hanging lock files
    // -------------------------
    QStringList::ConstIterator it;
    for (it = lockFiles.begin(); it != lockFiles.end(); ++it) {
      r_file file( *it );
      file.remove();
    }

    // Close the BNP Connection
    // ------------------------
    if (initmenu.modemMode()) {
      Q3UrlOperator* urlOp = (Q3UrlOperator*) initmenu.urlOp();
      delete urlOp;
    }

    return QMainWindow::close();
  }
  else {
    return false;
  }
}

// Show Output Window
////////////////////////////////////////////////////////////////////////////
void t_mainwin::showOutput(const QString& filter, QString dir) {

  QString pgmName;

  if (!lastOutput.isEmpty()) {
    dir     = expandEnvVar( lastOutput );
    pgmName = stripPath(lastOutput, true);
  }
  else {
    dir = expandEnvVar(dir);
  }

  QString addDir;
  if (pgmName == "UPDPAN" || pgmName == "CHNGEN") {
    addDir   = expandEnvVar( initmenu.getPath("DIR_OUT") );
  }
  else {
    addDir   = expandEnvVar( initmenu.getPath("PTH_OUT") );
  }

  QString showFile = t_myfildlg::getFileName(dir, filter, this, 0,
                                             QString::null, addDir);
  if (!showFile.isEmpty()) {
    new t_textwin(this, false, showFile, t_textwin::browse);
  }
}

void t_mainwin::showErrMsg(const QString& filter, QString dir) {
  if ( !errFileName.isEmpty() && r_file::exists(errFileName) ) {
    r_file errFile(errFileName);
    if (errFile.size() > 0) {
      dir = errFileName;
    }
  }
  QString showFile = t_myfildlg::getOpenFileName(dir, filter, this);
  if (!showFile.isEmpty()) {
    new t_textwin(this, false, showFile, t_textwin::browse);
  }
}

// Show Last Output or Error Message File
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotLastOutput() {
  new t_textwin(this, false, lastOutput, t_textwin::browse);
  if ( !errFileName.isEmpty() && r_file::exists(errFileName) ) {
    r_file errFile(errFileName);
    if (errFile.size() > 0) {
      t_textwin* errWin = new t_textwin(this, false, errFileName,
                                                        t_textwin::browse);
      errWin->move(errWin->x() + 40, errWin->y() + 40);
    }
  }
}

// Show Last Output or Error Message File
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotRerun() {
  slotMenu(currentSlotMenuItem);
}

// Increse the session
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotPlusSession() {
  if (! _datedial) {
    t_datedial dlg(false);
    dlg.setPlus();
    slotDateAccept();
  }
  else {
    _datedial->setPlus();
    _datedial->raise();
  }
}

// Decrease the session
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotMinusSession() {
  if (! _datedial) {
    t_datedial dlg(false);
    dlg.setMinus();
    slotDateAccept();
  }
  else {
    _datedial->setMinus();
    _datedial->raise();
  }
}

// Edit File
////////////////////////////////////////////////////////////////////////////
void t_mainwin::editFile(QString dirKey, QString extKeys,
                         const QString& action) {
  QStringList extKeyList = QStringList::split(QRegExp("\\s"), extKeys);
  QString     filter;
  for (int ii = 0; ii < (int) extKeyList.count(); ii++) {
    filter = filter + "*." + initmenu.getKeySel0(extKeyList[ii]) + ";;";
  }
  filter = filter + "*.*";
  QString dir = expandEnvVar( initmenu.getPath(dirKey) );
  QString showFile = t_myfildlg::getOpenFileName(dir, filter, this);

  if (!showFile.isEmpty()) {
    t_textwin::Modus modus = t_textwin::edit;
    if (action == "OBSFILE") {
      modus = t_textwin::browse;
    }
    new t_textwin(this, true, showFile, modus, "", action);
  }
}

// Edit Session Table
////////////////////////////////////////////////////////////////////////////
void t_mainwin::editSessions() {

  t_keyword* tabKey = initmenu.getKey("SESSION_TABLE");
  if (tabKey) {
    tabKey->updateSel();
    QString tableName = expandEnvVar( initmenu.getKeySel0("SESSION_TABLE") );

    if (!tableName.isEmpty()) {
      inpFiles.clear();
      inpFiles.append(tableName);
      readAllInputs(false);
    }
  }
}

// Edit Station File
////////////////////////////////////////////////////////////////////////////
void t_mainwin::editStationFile( const QString& action,
                                 const QString& pathKey,
                                 const QString& extKey,
                                 const QString& skeletonKey,
                                 const QString& scratchKey,
                                 const QString& scratchPath,
                                 const QString& selTitle,
                                 const QString& crdPathKey,
                                 const QString& crdExtKey ) {

  if (! mayIcloseInputFile()) return;

  QString extension = initmenu.getKeySel0(extKey);
  QString filter;
  if (extension.isEmpty()) {
    filter   = "*.*";
  }
  else {
    filter   = "*." + extension + ";;*.*";
  }
  QString dir      = expandEnvVar( initmenu.getPath(pathKey) );
  QString fileName = t_myfildlg::getFileName(dir, filter,
                                             this, 0, selTitle);

  if ( !fileName.isEmpty() ) {

    // Select a Skeleton CRD File if editing a new File
    // ------------------------------------------------
    QString crdFileName = "";
    if ( !r_file::exists(fileName) && !crdExtKey.isEmpty() ) {
      QString crdFilter   = "*." + initmenu.getKeySel0(crdExtKey) + ";;*.*";
      QString crdDir      = expandEnvVar( initmenu.getPath(crdPathKey) );
      crdFileName = t_myfildlg::getOpenFileName(crdDir, crdFilter,
                            this, 0, "Select Skeleton Coordinate File");
    }

    if ( initmenu.getKey(skeletonKey) == 0) {
      return;
    }

    QString skeletonFileName = initmenu.getInpFileName(skeletonKey);

    QString hlpFileName = expandEnvVar( skeletonFileName );
    if (!r_file::exists(hlpFileName)) {
      QString templateFile = expandEnvVar( initmenu.getPath("PTH_UPDPAN") +
                                           stripPath(skeletonFileName) ) ;
      QString msg("File " + skeletonFileName + " does not exist,\n"
                  "Do you wish to use " + templateFile + " ?");
      if ( QMessageBox::warning(0, "Question", msg,
                                QMessageBox::Yes | QMessageBox::Default,
                                QMessageBox::No ) == QMessageBox::Yes ) {
        fileCopy( templateFile, skeletonFileName );
      }
      else {
        return;
      }
    }

    QString scratchFileName = initmenu.getInpFileName(scratchKey);
    if (scratchFileName.isEmpty()) {
      return;
    }
    else {
      scratchFileName = expandEnvVar( initmenu.getPath(scratchPath) +
                                      scratchFileName );
    }

    Q3PtrList<t_keyword>* hlp = new Q3PtrList<t_keyword>;
    hlp->setAutoDelete(true);
    hlp->append(new t_keyword("FILE_EDIT"));
    hlp->last()->storeSel(fileName);
    hlp->append(new t_keyword("FILE_SKELETON"));
    hlp->last()->storeSel(skeletonFileName);
    hlp->append(new t_keyword("FILE_SCRATCH"));
    hlp->last()->storeSel(scratchFileName);
    hlp->append(new t_keyword("FILE_CRD"));
    hlp->last()->storeSel(crdFileName);

    t_keyword dummy("DUMMY");
    dummy.menuaux(action, hlp);
    delete hlp;

    inpFiles.clear();
    inpFiles.append(skeletonFileName);
    readAllInputs(false);
  }
}

// Save Station Files
////////////////////////////////////////////////////////////////////////////
int t_mainwin::saveStationFile(const QString& outFileName,
                               const QString& action) {

  Q3PtrList<t_keyword>* hlp = new Q3PtrList<t_keyword>;
  hlp->setAutoDelete(true);
  hlp->append(new t_keyword("FILE_INPUT"));
  hlp->last()->storeSel(inpfile->getName());
  hlp->append(new t_keyword("FILE_OUTPUT"));
  hlp->last()->storeSel(outFileName);

  t_keyword dummy("DUMMY");
  int irc = dummy.menuaux(action, hlp);
  delete hlp;
  return irc;
}

// Create New Campaign
////////////////////////////////////////////////////////////////////////////
void t_mainwin::newCamp(const QString& inpFileName,
                        const QString& baseTargetDirName_) {

  QString baseTargetDirName;
  if (baseTargetDirName_.isEmpty()) {
    baseTargetDirName = expandEnvVar(initmenu.getActiveCamp());
  }
  else {
    baseTargetDirName = expandEnvVar(baseTargetDirName_);
  }

  r_dir baseDir(baseTargetDirName);
  baseDir.mkdir(baseTargetDirName);

  t_inpfile inpFile(inpFileName);

  QStringList subdirs = inpFile.getKey("LIST_SUBDIRS")->getSelList();
  for (int ii = 0; ii < subdirs.count(); ii++) {
    subdirs[ii] = subdirs[ii].replace(QRegExp("\""),"");
    QString subDirPath = baseTargetDirName + r_dir::separator() + subdirs[ii];
    r_dir subDir(subDirPath);
    if (!subDir.exists() && !baseDir.mkdir(subDirPath)) {
      errormsg("Error Creating Directory " + subDirPath);
    }
  }

  QStringList files = inpFile.getKey("LIST_FILES")->getSelList();
  for (int ii = 0; ii < files.count(); ii++) {
    QStringList hlp;
    unilineSplit(files[ii], hlp);
    if (hlp.count() != 2 || hlp[0].isEmpty() || hlp[1].isEmpty()) {
      errormsg("newCamp: wrong option " + files[ii]);
    }
    else {
      QString     sourceDirName = expandEnvVar( stripFileName(hlp[0]) );
      r_dir       dir(sourceDirName);
      QString     nameFilter = stripPath(hlp[0]);
      QStringList fileNames = dir.entryList(nameFilter, QDir::Files);

      QStringListIterator it(fileNames);
      while (it.hasNext()) {
        QString fileName = it.next();
        QString source = sourceDirName + r_dir::separator() + fileName;
        QString target = baseTargetDirName + r_dir::separator() + hlp[1]
                                   + r_dir::separator() + fileName;
        fileCopy(source, target);
      }
    }
  }
}

// Convert (and Edit) PCF File
////////////////////////////////////////////////////////////////////////////
QString t_mainwin::convPCF(const QString* p_PCFName, bool makeCopy) {

  // Pick the name of the process control file
  // -----------------------------------------
  QString PCFname;
  if (p_PCFName) {
    PCFname = expandEnvVar(*p_PCFName);
  }
  else {
    QString pcfFilter = "*." + initmenu.getKeySel0("EXT_PCF") + ";;*.*";
    QString pcfDir    = expandEnvVar( initmenu.getPath("PTH_PCF") );

    PCFname = t_myfildlg::getFileName(pcfDir, pcfFilter, 0, 0,
                                      "Process Control File");
  }
  if ( PCFname.isEmpty() ) {
    return "";
  }

  // Pick the name of the skeleton file
  // ----------------------------------
  QString skeletonFileName = initmenu.getInpFileName("EDITPCF_INP");
  if ( !r_file::exists(expandEnvVar(skeletonFileName)) ) {
    QString templateFile = expandEnvVar( initmenu.getPath("PTH_UPDPAN") +
                                         stripPath(skeletonFileName) ) ;
    if ( initmenu.getIntModus() == t_initmenu::NOINT ||
         QMessageBox::warning(0, "Question", "File " + skeletonFileName +
                              " does not exist,\n"
                              "Do you wish to use " + templateFile + " ?",
                              QMessageBox::Yes | QMessageBox::Default,
                              QMessageBox::No ) == QMessageBox::Yes ) {
      if ( fileCopy(templateFile, skeletonFileName) != success ) {
        return "";
      }
    }
    else {
      return "";
    }
  }
  // Make own copy of skeleton file
  // ------------------------------
  if (makeCopy) {
    QString newSkeletonFileName = initmenu.getPath("PTH_SCR")
                                + stripPath(skeletonFileName)
                                +  "_" + initmenu.uniqueString() ;
    if ( fileCopy(skeletonFileName, newSkeletonFileName) != success ) {
      return "";
    }
    skeletonFileName = newSkeletonFileName;
  }

  // Call the MENUAUX program
  // ------------------------
  Q3PtrList<t_keyword>* hlp = new Q3PtrList<t_keyword>;
  hlp->setAutoDelete(true);
  hlp->append(new t_keyword("FILE_PCF"));
  hlp->last()->storeSel(PCFname);
  hlp->append(new t_keyword("FILE_SKELETON"));
  hlp->last()->storeSel(skeletonFileName);

  t_keyword dummy("DUMMY");
  int irc = dummy.menuaux("PCF_EDIT", hlp);
  delete hlp;

  if (irc == 0) {
    return skeletonFileName;
  }
  else {
    if (makeCopy) {
      r_file hlpFile(expandEnvVar(skeletonFileName));
      hlpFile.remove();
    }
    return "";
  }
}


// Edit BPE Options
////////////////////////////////////////////////////////////////////////////
QDialog::DialogCode t_mainwin::edtBPE() {

  QString skeletonFileName = convPCF();

  if (skeletonFileName.isEmpty()) {
    return QDialog::Rejected;
  }

  // Read the List of Scripts from the PCF
  // -------------------------------------
  t_inpfile   hlpPCF(skeletonFileName);
  t_keyword*  listKey = hlpPCF.getKey("LIST_OF_SCRIPTS");

  if (!listKey) {
    errormsg("edtBPE: keyword not found: " + listKey->getName());
    return QDialog::Rejected;
  }

  QStringList inpList;
  QStringList ioList;

  QString errMsg;

  for (int ii = 0; ii < listKey->getSelList().count(); ii++) {
    QStringList hlp;
    unilineSplit(listKey->getSelList()[ii], hlp);
    QStringList* pgmList = findPgmInScript(hlp[1]);
    if (pgmList) {
      for (int jj = 0; jj < pgmList->count(); jj++) {
        QString pgm = pgmList->value(jj);
        inpList.append( QString("%1 %2 %3 %4").arg(hlp[0],4)
                                              .arg(hlp[1],-10)
                                              .arg(hlp[2],-10)
                                              .arg(pgm)
                              + "." + initmenu.getKeySel0("EXT_INP") ) ;
      }
      delete pgmList;
    }
    else {
      errMsg += hlp[1] + "\n";
    }
  }

  if (!errMsg.isEmpty()) {
    errormsg("Scripts Not found:\n" + errMsg);
  }

  t_seldial dialog(this, listKey->getName(), inpList, ioList, true);

  int irc = dialog.exec();

  if (irc == QDialog::Accepted) {

    for (int ii = 0; ii < ioList.count(); ii++) {
      QStringList hlp = QStringList::split(QRegExp("\\s"), ioList[ii]);
      inpFiles.append( initmenu.getPath("PTH_OPT") + hlp[2] +
                       r_dir::separator() + hlp[3] );

      // Check the existence of MENU*.INP Files
      // --------------------------------------
      Q3PtrList<t_inpfile> inp = initmenu.inpfiles();
      for (inp.first(); inp.current(); inp.next()) {
        QString fileName = initmenu.getPath("PTH_OPT") + hlp[2]
                         + r_dir::separator()
                         + stripPath(inp.current()->getName(), true) + "."
                         + initmenu.getKeySel0("EXT_INP") ;
        if ( !r_file::exists( expandEnvVar(fileName) ) ) {
          fileCopy( inp.current()->getName(), fileName);
        }
      }

    }
    readAllInputs(false);

    return QDialog::Accepted;
  }

  return QDialog::Rejected;
}

// Select Font
////////////////////////////////////////////////////////////////////////////
void t_mainwin::selectFont(int item) {

  bool ok;

  if      (item == MENU_ID_FONT_BASE) {
    QFont newFont = QFontDialog::getFont(&ok, this->font(), this);
    if (ok) {
      initmenu.setFontBase(newFont);
      QApplication::setFont(newFont, true);
    }
  }
  else if (item == MENU_ID_FONT_LARGE) {
    QFont newFont = QFontDialog::getFont(&ok, initmenu.getFontLarge(), this);
    if (ok) {
      if (QFontMetrics(newFont).width('i')!=QFontMetrics(newFont).width('W')) {
        errormsg("Selected font is proportional\n"
                 "display may become very ugly");
      }
      initmenu.setFontLarge(newFont);
      canvas->setFont(newFont);
      resetPanel();
    }
  }
  else if (item == MENU_ID_FONT_SMALL) {
    QFont newFont = QFontDialog::getFont(&ok, initmenu.getFontSmall(), this);
    if (ok) {
      if (QFontMetrics(newFont).width('i')!=QFontMetrics(newFont).width('W')) {
        errormsg("Selected font is proportional\n"
                 "display may become very ugly");
      }
      initmenu.setFontSmall(newFont);
    }
  }

}

// Response to the Window Resize
///////////////////////////////////////////////////////////////////////////////
void t_mainwin::resizeEvent(QResizeEvent*) {
  if (!initmenu.modemMode()) {
    initmenu.saveKey("MAIN_WIDTH", QString().sprintf("%d", this->width()));
    initmenu.saveKey("MAIN_HEIGHT", QString().sprintf("%d", this->height()));
    setStatusBarMessage();
  }
}

// Select Color
////////////////////////////////////////////////////////////////////////////
void t_mainwin::selectColor(int item) {

  QColor button = QApplication::palette().color(QPalette::Normal,
                                                  QColorGroup::Button);
  QColor bckgrd = QApplication::palette().color(QPalette::Normal,
                                                  QColorGroup::Background);

  QString keyValue;
  QColor newColor;
  if  (item == MENU_ID_BUTTON_COLOR) {
    newColor = QColorDialog::getColor(button);
    if (newColor.isValid()) {
      QApplication::setPalette(QPalette(newColor, bckgrd), true);
      keyValue.sprintf("%d %d %d %d %d %d",
                       newColor.red(), newColor.green(), newColor.blue(),
                       bckgrd.red(), bckgrd.green(), bckgrd.blue());
    }
  }
  else {
    newColor = QColorDialog::getColor(bckgrd);
    if (newColor.isValid()) {
      QApplication::setPalette(QPalette(button, newColor), true);
      keyValue.sprintf("%d %d %d %d %d %d",
                       button.red(), button.green(), button.blue(),
                       newColor.red(), newColor.green(), newColor.blue());
    }
  }
  if (newColor.isValid()) {
    initmenu.saveKey("MAIN_PALETTE", keyValue);
  }
}

//
////////////////////////////////////////////////////////////////////////////
void t_mainwin::newCanvas() {
  if (canvas) {
    _sb->takeWidget();
  }
  delete canvas;
  canvas = new t_canvas(this);
  connect(canvas, SIGNAL(fieldChanged()), this, SLOT(slotSetActionsOnOff()));
  _sb->setWidget(canvas);
  setFocus();
}

//
////////////////////////////////////////////////////////////////////////////
void t_mainwin::slotMessageChanged(const QString& message) {
  if (message.indexOf(">") != 0) {
    setStatusBarMessage();
  }
}
