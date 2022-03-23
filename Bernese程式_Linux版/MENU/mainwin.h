
#ifndef MAINWIN_H
#define MAINWIN_H

#include <QtCore>
#include <QtGui>

#include "canvas.h"
#include "inpfile.h"
#include "datedial.h"

#define DEFAULT_MAIN_WIDTH   750
#define DEFAULT_MAIN_HEIGHT  500
#define DEFAULT_MAIN_PALETTE "192 192 192 192 192 192"

class t_mymenu;

class t_mainwin : public QMainWindow
{
  Q_OBJECT

  public:
    t_mainwin(const QString& inpFileName, bool noInit = false);
    ~t_mainwin();
    static QString convPCF(const QString* p_PCFName = 0,
                           bool makeCopy = false);
    virtual void closeEvent(QCloseEvent* event);

    static void newCamp(const QString& inpFileName,
                        const QString& baseDirName = "");

  public slots:
    virtual bool close();
    virtual void slotMenu( int );

  protected:
    void resizeEvent(QResizeEvent *e);

  protected slots:
    void   slotHelp( int );
    virtual void slotAbout();
    void   slotTopPanel();
    void   slotNextPanel();
    void   slotPrevPanel();
    void   slotCancel();
    int    slotSaveAsPanels();
    int    slotSavePanels();
    virtual void slotRun();
    bool   isFirstPanel();
    bool   isLastVisiblePanel();
    void   slotSetActionsOnOff();
    void   slotLastOutput();
    void   slotRerun();
    void   slotPlusSession();
    void   slotMinusSession();
    void   slotDateAccept();
    void   slotDateClosed();
    void   slotMessageChanged(const QString&);

  protected:
    virtual void initialize(const QString& inpFileName);
    virtual void initialize();
    void         readAllInputs(bool rb = true);
    void         readNextInp();
    void         selCamp(void);
    void         showJuldat(void);
    void         setActionsOnOff();
    virtual void updateMenuUser();
    void         resetPanel();
    void         selectFont(int item);
    void         selectColor(int item);
    void         resetFonts();
    bool         mayIcloseInputFile();
    void         showOutput(const QString& filter, QString dir);
    void         showErrMsg(const QString& filter, QString dir);
    virtual void showPlot();
    void         editFile(QString dirKey, QString extKeys,
                          const QString& action = "");
    void         editSessions();
    QDialog::DialogCode edtBPE();
    void         editStationFile( const QString& action,
                                  const QString& pathKey,
                                  const QString& extKey,
                                  const QString& skeletonKey,
                                  const QString& scratchKey,
                                  const QString& scratchPath,
                                  const QString& selTitle,
                                  const QString& crdPathKey,
                                  const QString& crdExtKey );
    int          saveStationFile( const QString& outFileName,
                                  const QString& action );
    virtual void setStatusBarMessage();
    int          savePanels(const QString& outInpFileName);
    void         newCanvas();

    t_mymenu*    _menuUser;
    QAction*     prevPanelAction;
    QAction*     topPanelAction;
    QAction*     nextPanelAction;
    QAction*     cancelAction;
    QAction*     saveAsAction;
    QAction*     saveAction;
    QAction*     runAction;
    QAction*     outputAction;
    QAction*     rerunAction;
    QAction*     plusSessionAction;
    QAction*     minusSessionAction;
    QScrollArea* _sb;
    t_canvas*    canvas;
    t_inpfile*   inpfile;
    int          currentSlotMenuItem;
    int          currentInpFile;
    bool         enableRunAction;
    QStringList  inpFiles;
    QString      lastOutput;
    QString      errFileName;
    QString      rungpsFile;
    t_datedial*  _datedial;
    bool         _waitForPanelCheck;
    QToolBar*    toolbar;
};

class t_myaction : public QAction {
  Q_OBJECT
  public:
    t_myaction(const QString& text, t_mainwin* parent, int id) :
      QAction(text,0) {
      _id     = id;
      _parent = parent;
      connect(this, SIGNAL(triggered()), this, SLOT(myTriggered()));
    }
    ~t_myaction() {};
  private slots:
    void myTriggered() {
      _parent->slotMenu(_id);
    }
  private:
    t_mainwin* _parent;
    int        _id;
};


class t_mymenu : public QMenu {
 Q_OBJECT
 public:
   t_mymenu(t_mainwin* parent) : QMenu(parent) {
     _parent = parent;
   }
   ~t_mymenu() {
   }
   void insertItem(const QString& text, int id);
   void insertItem(const QString& text, QMenu* popup);
  private:
   t_mainwin* _parent;
};

#endif
