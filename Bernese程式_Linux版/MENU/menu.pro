# ==============================================================================
#
# Name:       menu.pro
#
# Authors:
#
# Created:    __-___-____
#
# Changes:    21-May-2012 SL: z and QtNetwork added to QMAKE_LIBS
#
# ==============================================================================

# Name of the executable file
# ---------------------------
TARGET = menu

# Configuration (release/debug)
# ----------------------------
CONFIG -= debug
QT     += qt3support
win32::INCLUDEPATH += $(QTDIR)/src/3rdparty/zlib

# Resources
# ---------
RESOURCES += menu.qrc

# Directories
# -----------
debug:MOC_DIR       = .moc/debug
debug:OBJECTS_DIR   = .obj/debug
debug:DESTDIR       = ./
release:MOC_DIR     = .moc/release
release:OBJECTS_DIR = .obj/release
release:DESTDIR     = ./

# Get rid of mingwm10.dll
# -----------------------
win32:QMAKE_LFLAGS                 -= -mthreads
win32:QMAKE_CXXFLAGS_EXCEPTIONS_ON -= -mthreads
win32:QMAKE_LFLAGS_EXCEPTIONS_ON   -= -mthreads

QMAKE_LFLAGS += -static-libgcc
##QMAKE_LIBS += -lz -lQtNetwork

# List of all header and source files
# -----------------------------------
HEADERS   =  menu.h       canvas.h     datedial.h   field.h      initmenu.h   \
             inpfile.h    juldat.h     keyword.h    mainwin.h    menutils.h   \
             panel.h      seldial.h    selwin.h     uniline.h    textwin.h    \
             updpan.h     runpgm.h     errormsg.h   mycheck.h    myfildlg.h   \
             server.h     script.h     cpufile.h    pcffile.h    bpelog.h     \
             bpedial.h    myhtml.h     session.h    keydesc.h    chngen.h     \
             mylineedit.h lockfile.h   myspinbox.h  bpe.h        bpeinp.h     \
             mycombobox.h menstyle.h   r_file.h     r_dir.h      bnp.h      \
             myapp.h

SOURCES   =  menu.cpp     canvas.cpp   datedial.cpp field.cpp    initmenu.cpp \
             inpfile.cpp  juldat.cpp   keyword.cpp  mainwin.cpp  menutils.cpp \
             panel.cpp    seldial.cpp  selwin.cpp   uniline.cpp  textwin.cpp  \
             updpan.cpp   runpgm.cpp   errormsg.cpp mycheck.cpp  myfildlg.cpp \
             server.cpp   script.cpp   cpufile.cpp  pcffile.cpp  bpelog.cpp   \
             bpedial.cpp  myhtml.cpp   session.cpp  keydesc.cpp  chngen.cpp   \
             mylineedit.cpp lockfile.cpp myspinbox.cpp mainwin2.cpp bpe.cpp   \
             bpeinp.cpp   mycombobox.cpp menstyle.cpp r_file.cpp r_dir.cpp    \
             bnp.cpp myapp.cpp

# The following 6 lines are necessary if you want to plot residuals etc.
# ----------------------------------------------------------------------
#DEFINES     += USE_QWT
#HEADERS     += graphwin.h
#SOURCES     += graphwin.cpp
#INCLUDEPATH += $(QWTDIR)/include
#unix:LIBS   += -L$(QWTDIR) -lqwt
#win32:LIBS  += $(QWTDIR)/qwt.lib

# ==============================================================================
